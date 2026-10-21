#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <sstream>

namespace LCompilers {

namespace {

struct GpuLoopTransformName {
    const char *name;
    GpuLoopTransform transform;
};

// The names `--gpu-loop-transforms` lists.
const GpuLoopTransformName gpu_loop_transform_names[] = {
    {"fuse", GpuLoopFuse},
    {"swap", GpuLoopSwap},
    {"split", GpuLoopSplit},
    {"jam", GpuLoopJam},
};

// The transformations a value of `--gpu-loop-transforms` selects, or
// false with the first name it does not know in `unknown`.
bool parse_gpu_loop_transforms(const std::string &s, int &mask,
        std::string &unknown) {
    mask = 0;
    if (s == "all") {
        for (auto &n : gpu_loop_transform_names) mask |= n.transform;
        return true;
    }
    if (s == "none") return true;
    std::stringstream ss(s);
    std::string item;
    bool any = false;
    while (std::getline(ss, item, ',')) {
        any = true;
        bool known = false;
        for (auto &n : gpu_loop_transform_names) {
            if (item == n.name) {
                mask |= n.transform;
                known = true;
            }
        }
        if (!known) {
            unknown = item;
            return false;
        }
    }
    // An empty value, or one that ends with a comma.
    if (!any || s.back() == ',') {
        unknown = "";
        return false;
    }
    return true;
}

} // namespace

bool gpu_loop_transforms_valid(const std::string &value,
        std::string &message) {
    int mask = 0;
    std::string unknown;
    if (parse_gpu_loop_transforms(value, mask, unknown)) return true;
    std::string names;
    for (auto &n : gpu_loop_transform_names) {
        names += std::string(names.empty() ? "" : ", ") + n.name;
    }
    message = "the value `" + value + "` of `--gpu-loop-transforms` is not "
        "supported";
    if (!unknown.empty()) {
        message += ": unknown transformation `" + unknown + "`";
    }
    message += "; supported values: all, none, or a comma separated list of "
        + names;
    return false;
}

bool gpu_loop_transform_enabled(const PassOptions &pass_options,
        GpuLoopTransform t) {
    int mask = 0;
    std::string unknown;
    if (!parse_gpu_loop_transforms(pass_options.gpu_loop_transforms, mask,
            unknown)) {
        return false;
    }
    return (mask & t) != 0;
}

bool gpu_loop_transforms_apply(const PassOptions &pass_options) {
    return gpu_device_selected(pass_options) == GpuDevice::Cuda;
}

ASR::expr_t *gpu_cache_bytes(Allocator &al, SymbolTable *unit_scope,
        const PassOptions &pass_options, const Location &loc) {
    ASRUtils::ASRBuilder b(al, loc);
    if (pass_options.gpu_cache_bytes >= 0) {
        return b.i64(pass_options.gpu_cache_bytes);
    }
    const std::string name = "lfortran_gpu_cache_bytes";
    ASR::symbol_t *sym = unit_scope->get_symbol(name);
    ASR::ttype_t *i64 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
    if (sym == nullptr) {
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(unit_scope);
        ASR::expr_t *return_var = b.Variable(fn_symtab, name,
            ASRUtils::duplicate_type(al, i64),
            ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC,
            false);
        ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc,
            fn_symtab, s2c(al, name), nullptr, 0, nullptr, 0, nullptr, 0,
            return_var, ASR::abiType::BindC, ASR::accessType::Public,
            ASR::deftypeType::Interface, s2c(al, name), false, false,
            false, false, false, nullptr, 0, false, false, false,
            nullptr);
        sym = ASR::down_cast<ASR::symbol_t>(fn);
        unit_scope->add_symbol(name, sym);
    }
    Vec<ASR::call_arg_t> args;
    args.reserve(al, 1);
    return b.Call(sym, args, ASRUtils::duplicate_type(al, i64));
}

ASR::expr_t *gpu_i32(Allocator &al, const Location &loc, int64_t v) {
    return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, v,
        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
        ASR::integerbozType::Decimal));
}

static ASR::expr_t *int_binop(Allocator &al, ASR::expr_t *a,
        ASR::binopType op, ASR::expr_t *b) {
    ASR::ttype_t *t = ASRUtils::expr_type(a);
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a->base.loc, a, op,
        b, ASRUtils::duplicate_type(al, t), nullptr));
}

ASR::expr_t *gpu_int_add(Allocator &al, ASR::expr_t *a, ASR::expr_t *b) {
    return int_binop(al, a, ASR::binopType::Add, b);
}

ASR::expr_t *gpu_int_sub(Allocator &al, ASR::expr_t *a, ASR::expr_t *b) {
    return int_binop(al, a, ASR::binopType::Sub, b);
}

int GpuLoopNest::position(ASR::symbol_t *v) const {
    for (size_t d = 0; d < vars.size(); d++) {
        if (vars[d] == v) return (int)d;
    }
    return -1;
}

bool gpu_loop_nest(ASR::stmt_t *s, GpuLoopNest &nest) {
    if (!ASR::is_a<ASR::OMPRegion_t>(*s)) return false;
    ASR::OMPRegion_t *r = ASR::down_cast<ASR::OMPRegion_t>(s);
    if (r->m_exec_target != ASR::exec_targetType::ExecDevice
            || r->m_region != ASR::omp_region_typeType::ParallelDo) {
        return false;
    }
    int64_t collapse = 1;
    bool independent = false;
    nest = GpuLoopNest();
    for (size_t i = 0; i < r->n_clauses; i++) {
        ASR::omp_clause_t *c = r->m_clauses[i];
        if (ASR::is_a<ASR::OMPIndependent_t>(*c)) {
            independent = true;
        } else if (ASR::is_a<ASR::OMPCollapse_t>(*c)) {
            ASR::expr_t *n = ASR::down_cast<ASR::OMPCollapse_t>(c)->m_count;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(n), collapse)) {
                return false;
            }
        } else if (ASR::is_a<ASR::OMPPrivate_t>(*c)) {
            ASR::OMPPrivate_t *p = ASR::down_cast<ASR::OMPPrivate_t>(c);
            for (size_t k = 0; k < p->n_vars; k++) {
                if (!ASR::is_a<ASR::Var_t>(*p->m_vars[k])) return false;
                nest.privates.push_back(ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(p->m_vars[k])->m_v));
            }
        } else if (!ASR::is_a<ASR::OMPOverwrite_t>(*c)) {
            return false;
        }
    }
    if (!independent || r->n_body != 1) return false;
    ASR::stmt_t *cur = r->m_body[0];
    for (int64_t d = 0; d < collapse; d++) {
        if (!ASR::is_a<ASR::DoLoop_t>(*cur)) return false;
        ASR::DoLoop_t *l = ASR::down_cast<ASR::DoLoop_t>(cur);
        if (l->n_orelse != 0 || l->m_head.m_v == nullptr
                || !ASR::is_a<ASR::Var_t>(*l->m_head.m_v)
                || l->m_head.m_start == nullptr
                || l->m_head.m_end == nullptr) {
            return false;
        }
        if (l->m_head.m_increment) {
            int64_t inc = 0;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(
                    l->m_head.m_increment), inc) || inc != 1) {
                return false;
            }
        }
        nest.loops.push_back(l);
        nest.vars.push_back(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(l->m_head.m_v)->m_v));
        nest.starts.push_back(l->m_head.m_start);
        nest.ends.push_back(l->m_head.m_end);
        if (d + 1 < collapse) {
            if (l->n_body != 1) return false;
            cur = l->m_body[0];
        }
    }
    nest.region = r;
    return true;
}

ASR::stmt_t *gpu_loop_nest_with_body(Allocator &al, const GpuLoopNest &nest,
        const std::vector<ASR::expr_t*> &starts,
        const std::vector<ASR::expr_t*> &ends, Vec<ASR::stmt_t*> &body,
        const std::vector<ASR::symbol_t*> &extra_privates) {
    ASR::stmt_t **cur_body = body.p;
    size_t cur_n = body.n;
    for (size_t k = nest.loops.size(); k-- > 0;) {
        ASR::DoLoop_t *l = nest.loops[k];
        ASR::do_loop_head_t head;
        head.loc = l->m_head.loc;
        head.m_v = ASRUtils::EXPR(ASR::make_Var_t(al, l->m_head.loc,
            nest.vars[k]));
        head.m_start = starts[k];
        head.m_end = ends[k];
        head.m_increment = nullptr;
        ASR::stmt_t *loop = ASRUtils::STMT(ASR::make_DoLoop_t(al,
            l->base.base.loc, nullptr, head, cur_body, cur_n, nullptr, 0));
        Vec<ASR::stmt_t*> one;
        one.reserve(al, 1);
        one.push_back(al, loop);
        cur_body = one.p;
        cur_n = 1;
    }
    Vec<ASR::omp_clause_t*> clauses;
    clauses.reserve(al, nest.region->n_clauses + 1);
    bool have_private = false;
    ASRUtils::ExprStmtDuplicator dup(al);
    for (size_t i = 0; i < nest.region->n_clauses; i++) {
        ASR::omp_clause_t *c = nest.region->m_clauses[i];
        if (ASR::is_a<ASR::OMPPrivate_t>(*c)) {
            ASR::OMPPrivate_t *p = ASR::down_cast<ASR::OMPPrivate_t>(c);
            Vec<ASR::expr_t*> vars;
            vars.reserve(al, p->n_vars + extra_privates.size());
            for (size_t k = 0; k < p->n_vars; k++) {
                vars.push_back(al, dup.duplicate_expr(p->m_vars[k]));
            }
            for (ASR::symbol_t *s : extra_privates) {
                vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al,
                    c->base.loc, s)));
            }
            clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPPrivate_t(al, c->base.loc, vars.p, vars.n)));
            have_private = true;
        } else if (!ASR::is_a<ASR::OMPOverwrite_t>(*c)) {
            // What the iterations of the nest overwrite is not known for
            // the new heads.
            clauses.push_back(al, dup.duplicate_omp_clause(c));
        }
    }
    if (!have_private && !extra_privates.empty()) {
        Vec<ASR::expr_t*> vars;
        vars.reserve(al, extra_privates.size());
        for (ASR::symbol_t *s : extra_privates) {
            vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al,
                nest.region->base.base.loc, s)));
        }
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPPrivate_t(al, nest.region->base.base.loc, vars.p,
                vars.n)));
    }
    return ASRUtils::STMT(ASR::make_OMPRegion_t(al,
        nest.region->base.base.loc, nest.region->m_region, clauses.p,
        clauses.n, cur_body, cur_n, nest.region->m_exec_target));
}

ASR::stmt_t *gpu_device_copy_nest(Allocator &al, const Location &loc,
        SymbolTable *scope, ASR::expr_t *dest, ASR::expr_t *src) {
    ASRUtils::ASRBuilder b(al, loc);
    ASRUtils::ExprStmtDuplicator d(al);
    size_t rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(src));
    ASR::ttype_t *it = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    std::vector<ASR::symbol_t*> vars;
    for (size_t r = 0; r < rank; r++) {
        vars.push_back(ASR::down_cast<ASR::Var_t>(b.Variable(scope,
            scope->get_unique_name("__gpu_copy_i", false),
            ASRUtils::duplicate_type(al, it), ASR::intentType::Local))->m_v);
    }
    auto item = [&](ASR::expr_t *a) {
        Vec<ASR::array_index_t> idx;
        idx.reserve(al, rank);
        for (size_t r = 0; r < rank; r++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_step = nullptr;
            ai.m_right = b.Var(vars[r]);
            idx.push_back(al, ai);
        }
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
            d.duplicate_expr(a), idx.p, idx.n, ASRUtils::duplicate_type(al,
                ASRUtils::extract_type(ASRUtils::expr_type(a))),
            ASR::arraystorageType::ColMajor, nullptr, false));
    };
    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, b.Assignment(item(dest), item(src)));
    // The first dimension is the innermost loop.
    for (size_t r = 0; r < rank; r++) {
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = b.Var(vars[r]);
        head.m_start = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
            d.duplicate_expr(src), gpu_i32(al, loc, r + 1),
            ASRUtils::duplicate_type(al, it), ASR::arrayboundType::LBound,
            nullptr));
        head.m_end = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
            d.duplicate_expr(src), gpu_i32(al, loc, r + 1),
            ASRUtils::duplicate_type(al, it), ASR::arrayboundType::UBound,
            nullptr));
        head.m_increment = nullptr;
        ASR::stmt_t *loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
            nullptr, head, body.p, body.n, nullptr, 0));
        Vec<ASR::stmt_t*> one;
        one.reserve(al, 1);
        one.push_back(al, loop);
        body = one;
    }
    Vec<ASR::omp_clause_t*> clauses;
    clauses.reserve(al, 2);
    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
        ASR::make_OMPIndependent_t(al, loc)));
    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
        ASR::make_OMPCollapse_t(al, loc, gpu_i32(al, loc, rank))));
    return ASRUtils::STMT(ASR::make_OMPRegion_t(al, loc,
        ASR::omp_region_typeType::ParallelDo, clauses.p, clauses.n, body.p,
        body.n, ASR::exec_targetType::ExecDevice));
}

std::string gpu_array_key(ASR::expr_t *e) {
    if (ASR::is_a<ASR::Var_t>(*e)) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*s)) return "";
        return std::to_string(reinterpret_cast<uintptr_t>(s)) + ":"
            + ASRUtils::symbol_name(s);
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        if (ASRUtils::is_pointer(ASRUtils::expr_type(m->m_v))) return "";
        std::string base = gpu_array_key(m->m_v);
        if (base.empty()) return "";
        ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(m->m_m);
        return base + "%" + std::to_string(reinterpret_cast<uintptr_t>(mem))
            + ":" + ASRUtils::symbol_name(mem);
    }
    return "";
}

// ---------------------------------------------------------------------------
// GpuDuplicator

// A name for a new BLOCK that no other BLOCK of the unit has: the offload
// pipeline finds the BLOCKs a kernel runs by name in one table.
static std::string unique_block_name(SymbolTable *scope,
        const std::string &base) {
    static int64_t counter = 0;
    std::string name;
    do {
        name = base + "_" + std::to_string(++counter);
    } while (scope->resolve_symbol(name) != nullptr);
    return name;
}

ASR::asr_t* GpuDuplicator::duplicate_Var(ASR::Var_t *x) {
    ASR::symbol_t *s = x->m_v;
    auto vm = value_map.find(s);
    if (vm != value_map.end()) {
        ASRUtils::ExprStmtDuplicator d(al);
        return (ASR::asr_t*)d.duplicate_expr(vm->second);
    }
    if (array_map.count(s)) {
        // A whole array dummy is only replaced as an actual argument.
        ok = false;
    }
    auto sm = symbol_map.find(s);
    if (sm != symbol_map.end()) s = sm->second;
    return ASR::make_Var_t(al, x->base.base.loc, s);
}

ASR::array_index_t* GpuDuplicator::duplicate_indices(ASR::ArrayItem_t *x,
        const ArrayActual *map) {
    Vec<ASR::array_index_t> args;
    args.reserve(al, x->n_args);
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::array_index_t idx;
        idx.loc = x->m_args[i].loc;
        idx.m_left = duplicate_expr(x->m_args[i].m_left);
        idx.m_right = duplicate_expr(x->m_args[i].m_right);
        idx.m_step = duplicate_expr(x->m_args[i].m_step);
        if (map) {
            if (idx.m_left || idx.m_step || idx.m_right == nullptr
                    || i >= map->shift.size()) {
                ok = false;
            } else if (map->shift[i]) {
                ASRUtils::ExprStmtDuplicator d(al);
                ASR::expr_t *shift = d.duplicate_expr(map->shift[i]);
                // In the kind of the subscript (integer(8) indices).
                ASR::ttype_t *t = ASRUtils::expr_type(idx.m_right);
                if (ASRUtils::extract_kind_from_ttype_t(t)
                        != ASRUtils::extract_kind_from_ttype_t(
                            ASRUtils::expr_type(shift))) {
                    ASRUtils::ASRBuilder b(al, idx.loc);
                    shift = b.i2i_t(shift, ASRUtils::duplicate_type(al, t));
                }
                idx.m_right = gpu_int_add(al, idx.m_right, shift);
            }
        }
        args.push_back(al, idx);
    }
    return args.p;
}

ASR::asr_t* GpuDuplicator::duplicate_ArrayItem(ASR::ArrayItem_t *x) {
    if (ASR::is_a<ASR::Var_t>(*x->m_v)) {
        auto am = array_map.find(ASR::down_cast<ASR::Var_t>(x->m_v)->m_v);
        if (am != array_map.end()) {
            ASRUtils::ExprStmtDuplicator d(al);
            ASR::expr_t *base = d.duplicate_expr(am->second.actual);
            ASR::array_index_t *idx = duplicate_indices(x, &am->second);
            return ASR::make_ArrayItem_t(al, x->base.base.loc, base, idx,
                x->n_args, duplicate_ttype(x->m_type), x->m_storage_format,
                nullptr, x->m_in_bounds);
        }
    }
    ASR::expr_t *base = duplicate_expr(x->m_v);
    ASR::array_index_t *idx = duplicate_indices(x, nullptr);
    return ASR::make_ArrayItem_t(al, x->base.base.loc, base, idx, x->n_args,
        duplicate_ttype(x->m_type), x->m_storage_format,
        duplicate_expr(x->m_value), x->m_in_bounds);
}

ASR::asr_t* GpuDuplicator::duplicate_ArraySize(ASR::ArraySize_t *x) {
    if (ASR::is_a<ASR::Var_t>(*x->m_v)) {
        auto am = array_map.find(ASR::down_cast<ASR::Var_t>(x->m_v)->m_v);
        if (am != array_map.end()) {
            ASRUtils::ExprStmtDuplicator d(al);
            return ASR::make_ArraySize_t(al, x->base.base.loc,
                d.duplicate_expr(am->second.actual),
                duplicate_expr(x->m_dim), duplicate_ttype(x->m_type),
                nullptr);
        }
    }
    return BaseExprStmtDuplicator<GpuDuplicator>::duplicate_ArraySize(x);
}

ASR::asr_t* GpuDuplicator::duplicate_ArrayBound(ASR::ArrayBound_t *x) {
    if (ASR::is_a<ASR::Var_t>(*x->m_v)) {
        auto am = array_map.find(ASR::down_cast<ASR::Var_t>(x->m_v)->m_v);
        if (am != array_map.end()) {
            int64_t dim = 0;
            if (x->m_dim == nullptr || !ASRUtils::extract_value(
                    ASRUtils::expr_value(x->m_dim), dim) || dim < 1
                    || (size_t)dim > am->second.lower.size()) {
                ok = false;
                return (ASR::asr_t*)gpu_i32(al, x->base.base.loc, 1);
            }
            ASRUtils::ExprStmtDuplicator d(al);
            ASR::expr_t *lower = d.duplicate_expr(am->second.lower[dim - 1]);
            if (x->m_bound == ASR::arrayboundType::LBound) {
                return (ASR::asr_t*)lower;
            }
            // ubound = lbound + size - 1, over the actual.
            ASR::expr_t *size = ASRUtils::EXPR(ASR::make_ArraySize_t(al,
                x->base.base.loc, d.duplicate_expr(am->second.actual),
                gpu_i32(al, x->base.base.loc, dim),
                ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4)),
                nullptr));
            return (ASR::asr_t*)gpu_int_sub(al, gpu_int_add(al, lower, size),
                gpu_i32(al, x->base.base.loc, 1));
        }
    }
    return BaseExprStmtDuplicator<GpuDuplicator>::duplicate_ArrayBound(x);
}

ASR::asr_t* GpuDuplicator::duplicate_FunctionCall(ASR::FunctionCall_t *x) {
    Vec<ASR::call_arg_t> args;
    args.reserve(al, x->n_args);
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::call_arg_t a;
        a.loc = x->m_args[i].loc;
        a.m_value = nullptr;
        ASR::expr_t *v = x->m_args[i].m_value;
        if (v && ASR::is_a<ASR::Var_t>(*v)) {
            auto am = array_map.find(ASR::down_cast<ASR::Var_t>(v)->m_v);
            if (am != array_map.end()) {
                // The callee sees the same shape through either; its own
                // lower bounds are its own.
                ASRUtils::ExprStmtDuplicator d(al);
                a.m_value = d.duplicate_expr(am->second.actual);
            }
        }
        if (a.m_value == nullptr) a.m_value = duplicate_expr(v);
        args.push_back(al, a);
    }
    return ASR::make_FunctionCall_t(al, x->base.base.loc, x->m_name,
        x->m_original_name, args.p, args.n, duplicate_ttype(x->m_type),
        duplicate_expr(x->m_value), duplicate_expr(x->m_dt));
}

ASR::asr_t* GpuDuplicator::duplicate_BlockCall(ASR::BlockCall_t *x) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x->m_m);
    if (!ASR::is_a<ASR::Block_t>(*s)) {
        ok = false;
        return ASR::make_BlockCall_t(al, x->base.base.loc, x->m_label,
            x->m_m);
    }
    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
    SymbolTable *outer = scope;
    SymbolTable *symtab = al.make_new<SymbolTable>(outer);
    for (auto &item : b->m_symtab->get_scope()) {
        ASR::symbol_t *old = item.second;
        // A BLOCK nested in this one is copied where the body runs it.
        if (ASR::is_a<ASR::Block_t>(*old)) continue;
        if (!ASR::is_a<ASR::Variable_t>(*old)) {
            ok = false;
            continue;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(old);
        if (v->m_symbolic_value || v->m_value
                || ASRUtils::is_array(v->m_type)) {
            ok = false;
        }
        ASR::symbol_t *nv = ASR::down_cast<ASR::Var_t>(
            ASRUtils::ASRBuilder(al, old->base.loc).Variable(symtab,
                item.first, ASRUtils::duplicate_type(al, v->m_type),
                v->m_intent, v->m_type_declaration))->m_v;
        symbol_map[old] = nv;
    }
    scope = symtab;
    Vec<ASR::stmt_t*> body;
    duplicate_body(b->m_body, b->n_body, body);
    scope = outer;
    std::string name = unique_block_name(outer, "__gpu_block");
    ASR::asr_t *nb = ASR::make_Block_t(al, b->base.base.loc, symtab,
        s2c(al, name), body.p, body.n);
    symtab->asr_owner = nb;
    ASR::symbol_t *nbs = ASR::down_cast<ASR::symbol_t>(nb);
    outer->add_symbol(name, nbs);
    if (added) added->push_back({outer, name});
    return ASR::make_BlockCall_t(al, x->base.base.loc, x->m_label, nbs);
}

void GpuDuplicator::duplicate_body(ASR::stmt_t **body, size_t n,
        Vec<ASR::stmt_t*> &out) {
    out.reserve(al, n);
    for (size_t i = 0; i < n; i++) {
        out.push_back(al, duplicate_stmt(body[i]));
    }
}

// ---------------------------------------------------------------------------
// Collectors

namespace {

// Whether a designator of this type is a derived-type object (not an
// array of them): reading or assigning it as a whole reads or assigns
// every component.
bool is_scalar_struct(ASR::ttype_t *t) {
    return !ASRUtils::is_array(t)
        && ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(t));
}

// How an expression of a given kind may define a variable. The kinds are
// listed explicitly: one this does not know may define anything.
enum class ExprEffect {
    None,       // computes a value from its operands
    Call,       // a function call: through its actual arguments
    ImpliedDo,  // an implied DO loop: its variable
    Unknown,
};

ExprEffect expr_effect(const ASR::expr_t &x) {
    switch (x.type) {
        case ASR::exprType::FunctionCall:
            return ExprEffect::Call;
        case ASR::exprType::ImpliedDoLoop:
            return ExprEffect::ImpliedDo;
        case ASR::exprType::IfExp:
        case ASR::exprType::ComplexConstructor:
        case ASR::exprType::IntrinsicElementalFunction:
        case ASR::exprType::IntrinsicArrayFunction:
        case ASR::exprType::TypeInquiry:
        case ASR::exprType::StructConstructor:
        case ASR::exprType::StructConstant:
        case ASR::exprType::EnumConstructor:
        case ASR::exprType::UnionConstructor:
        case ASR::exprType::IntegerConstant:
        case ASR::exprType::IntegerBitNot:
        case ASR::exprType::IntegerUnaryMinus:
        case ASR::exprType::IntegerCompare:
        case ASR::exprType::IntegerBinOp:
        case ASR::exprType::UnsignedIntegerConstant:
        case ASR::exprType::UnsignedIntegerUnaryMinus:
        case ASR::exprType::UnsignedIntegerBitNot:
        case ASR::exprType::UnsignedIntegerCompare:
        case ASR::exprType::UnsignedIntegerBinOp:
        case ASR::exprType::RealConstant:
        case ASR::exprType::RealUnaryMinus:
        case ASR::exprType::RealCompare:
        case ASR::exprType::RealBinOp:
        case ASR::exprType::RealCopySign:
        case ASR::exprType::RealSqrt:
        case ASR::exprType::ComplexConstant:
        case ASR::exprType::ComplexUnaryMinus:
        case ASR::exprType::ComplexCompare:
        case ASR::exprType::ComplexBinOp:
        case ASR::exprType::ComplexRe:
        case ASR::exprType::ComplexIm:
        case ASR::exprType::LogicalConstant:
        case ASR::exprType::LogicalNot:
        case ASR::exprType::LogicalCompare:
        case ASR::exprType::LogicalBinOp:
        case ASR::exprType::StringConstant:
        case ASR::exprType::StringConcat:
        case ASR::exprType::StringRepeat:
        case ASR::exprType::StringLen:
        case ASR::exprType::StringItem:
        case ASR::exprType::StringSection:
        case ASR::exprType::StringCompare:
        case ASR::exprType::StringContains:
        case ASR::exprType::StringOrd:
        case ASR::exprType::StringChr:
        case ASR::exprType::StringFormat:
        case ASR::exprType::StringPhysicalCast:
        case ASR::exprType::CompilerOptions:
        case ASR::exprType::Var:
        case ASR::exprType::ArrayConstructor:
        case ASR::exprType::ArrayConstant:
        case ASR::exprType::ArrayItem:
        case ASR::exprType::ArraySection:
        case ASR::exprType::ArraySize:
        case ASR::exprType::ArrayRank:
        case ASR::exprType::ArrayBound:
        case ASR::exprType::ArrayTranspose:
        case ASR::exprType::ArrayPack:
        case ASR::exprType::ArrayReshape:
        case ASR::exprType::ArrayBroadcast:
        case ASR::exprType::ArrayIsContiguous:
        case ASR::exprType::ArrayPhysicalCast:
        case ASR::exprType::BitCast:
        case ASR::exprType::Cast:
        case ASR::exprType::StructInstanceMember:
        case ASR::exprType::StructStaticMember:
        case ASR::exprType::EnumStaticMember:
        case ASR::exprType::UnionInstanceMember:
        case ASR::exprType::EnumName:
        case ASR::exprType::EnumValue:
        // An overloaded operator reads its operands; the function it calls
        // is a FunctionCall operand of its own.
        case ASR::exprType::OverloadedCompare:
        case ASR::exprType::OverloadedBinOp:
        case ASR::exprType::OverloadedBoolOp:
        case ASR::exprType::OverloadedUnaryMinus:
        case ASR::exprType::OverloadedStringConcat:
        case ASR::exprType::IntegerBitLen:
        case ASR::exprType::Ichar:
        case ASR::exprType::Iachar:
        case ASR::exprType::SizeOfType:
        case ASR::exprType::PointerNullConstant:
        case ASR::exprType::PointerAssociated:
        case ASR::exprType::CPtrCompare:
        case ASR::exprType::CLoc:
        case ASR::exprType::PointerToCPtr:
        case ASR::exprType::GetPointer:
            return ExprEffect::None;
        default:
            return ExprEffect::Unknown;
    }
}

// Computes GpuEffects. Every statement, those nested in DO, DO WHILE, IF,
// SELECT CASE, BLOCK and OpenMP constructs included, passes through
// visit_stmt, and every expression through visit_expr: each classifies the
// node by its kind, and a kind not modeled there makes the effects
// unknown. The generated walker reaches the nested statements and
// expressions through these two; a BLOCK's body, which it does not reach,
// is walked here.
class EffectsWalker : public ASR::BaseWalkVisitor<EffectsWalker> {
    using Base = ASR::BaseWalkVisitor<EffectsWalker>;
public:
    GpuEffects &e;
    // Inside the items of an output statement.
    int output = 0;
    explicit EffectsWalker(GpuEffects &e_) : e(e_) {}

    // A designator the code defines, `times` times as a whole if it is a
    // variable.
    void define(ASR::expr_t *t, int times = 1) {
        ASR::symbol_t *root = gpu_designator_base(t).root;
        if (root) {
            e.bases.insert(root);
        } else {
            e.unknown = true;
        }
        if (ASR::is_a<ASR::Var_t>(*t)) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(t)->m_v);
            e.scalars.insert(s);
            e.definitions[s] += times;
            if (ASRUtils::is_array(ASRUtils::expr_type(t))
                    || is_scalar_struct(ASRUtils::expr_type(t))) {
                std::string k = gpu_array_key(t);
                if (k.empty()) {
                    e.unknown = true;
                } else {
                    e.arrays.insert(k);
                }
            }
            return;
        }
        // A whole array component may be reallocated, which changes its
        // bounds, and so may the allocatable components of a whole
        // derived-type component: count either as a change of the
        // variable it belongs to.
        if (ASR::is_a<ASR::StructInstanceMember_t>(*t)
                && (ASRUtils::is_array(ASRUtils::expr_type(t))
                    || is_scalar_struct(ASRUtils::expr_type(t)))) {
            if (root) e.scalars.insert(root);
        }
        ASR::expr_t *base = t;
        if (ASR::is_a<ASR::ArrayItem_t>(*t)) {
            base = ASR::down_cast<ASR::ArrayItem_t>(t)->m_v;
        } else if (ASR::is_a<ASR::ArraySection_t>(*t)) {
            base = ASR::down_cast<ASR::ArraySection_t>(t)->m_v;
        }
        std::string k = gpu_array_key(base);
        if (k.empty()) {
            e.unknown = true;
        } else {
            e.arrays.insert(k);
        }
    }

    // A call of a procedure. A pure procedure defines nothing but the
    // actual arguments of its dummies that are not INTENT(IN) or VALUE; so
    // does a self-contained one without input/output. A pointer dummy may
    // define its target, which the actual does not name. Any other call is
    // unknown.
    void call(ASR::symbol_t *name, ASR::call_arg_t *args, size_t n_args,
            ASR::expr_t *dt) {
        ASR::symbol_t *f = ASRUtils::symbol_get_past_external(name);
        if (!ASR::is_a<ASR::Function_t>(*f) || dt) {
            e.unknown = true;
            return;
        }
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
        if (!ft->m_pure && !(fn->m_side_effect_free
                && gpu_function_self_contained(fn))) {
            e.unknown = true;
            return;
        }
        for (size_t k = 0; k < n_args; k++) {
            ASR::expr_t *a = args[k].m_value;
            if (a == nullptr) continue;
            if (k >= fn->n_args || !ASR::is_a<ASR::Var_t>(*fn->m_args[k])) {
                e.unknown = true;
                continue;
            }
            ASR::symbol_t *ds = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(fn->m_args[k])->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*ds)) {
                e.unknown = true;
                continue;
            }
            ASR::Variable_t *dv = ASR::down_cast<ASR::Variable_t>(ds);
            if (ASRUtils::is_pointer(dv->m_type)) {
                e.unknown = true;
                continue;
            }
            if (dv->m_intent == ASR::intentType::In || dv->m_value_attr) {
                continue;
            }
            while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*a)) {
                a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a)->m_arg;
            }
            // Any other actual is a value, passed in a temporary.
            if (is_designator(a)) define(a);
        }
    }

    static bool is_designator(ASR::expr_t *a) {
        return ASR::is_a<ASR::Var_t>(*a) || ASR::is_a<ASR::ArrayItem_t>(*a)
            || ASR::is_a<ASR::ArraySection_t>(*a)
            || ASR::is_a<ASR::StructInstanceMember_t>(*a);
    }

    void visit_stmt(const ASR::stmt_t &x) {
        switch (x.type) {
            case ASR::stmtType::Assignment: {
                const ASR::Assignment_t &a =
                    *ASR::down_cast<const ASR::Assignment_t>(&x);
                define(a.m_target);
                // The allocation moves from the value, which becomes
                // unallocated.
                if (a.m_move_allocation && is_designator(a.m_value)) {
                    define(a.m_value);
                }
                // A defined assignment is visited as the subroutine call
                // it is.
                break;
            }
            case ASR::stmtType::DoLoop: {
                const ASR::DoLoop_t &l =
                    *ASR::down_cast<const ASR::DoLoop_t>(&x);
                if (l.m_head.m_v) define(l.m_head.m_v, 2);
                break;
            }
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::If:
            case ASR::stmtType::Select:
            case ASR::stmtType::OMPRegion:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::Return:
            case ASR::stmtType::GoTo:
            case ASR::stmtType::GoToTarget:
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop:
            case ASR::stmtType::Assert:
                break;
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<const ASR::BlockCall_t>(&x)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*s)) {
                    e.unknown = true;
                    return;
                }
                ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
                for (size_t i = 0; i < b->n_body; i++) {
                    visit_stmt(*b->m_body[i]);
                }
                return;
            }
            case ASR::stmtType::SubroutineCall: {
                const ASR::SubroutineCall_t &c =
                    *ASR::down_cast<const ASR::SubroutineCall_t>(&x);
                call(c.m_name, c.m_args, c.n_args, c.m_dt);
                break;
            }
            case ASR::stmtType::Print:
                output++;
                Base::visit_stmt(x);
                output--;
                return;
            case ASR::stmtType::FileWrite: {
                const ASR::FileWrite_t &w =
                    *ASR::down_cast<const ASR::FileWrite_t>(&x);
                // IOSTAT=, IOMSG= and ID= are defined by the statement; a
                // namelist and a user-defined procedure for the output are
                // not modeled, nor asynchronous output.
                if (w.m_iostat || w.m_iomsg || w.m_id || w.m_nml
                        || w.m_overloaded || w.m_asynchronous) {
                    e.unknown = true;
                    return;
                }
                // An internal file is defined.
                if (w.m_unit && ASRUtils::is_character(
                        *ASRUtils::expr_type(w.m_unit))) {
                    define(w.m_unit);
                }
                output++;
                Base::visit_stmt(x);
                output--;
                return;
            }
            default:
                // Input, an allocation, a pointer association, an inquiry,
                // an intrinsic subroutine, an ASSOCIATE construct, ...
                e.unknown = true;
                return;
        }
        Base::visit_stmt(x);
    }

    void visit_expr(const ASR::expr_t &x) {
        switch (expr_effect(x)) {
            case ExprEffect::None:
            case ExprEffect::Call:
                break;
            case ExprEffect::ImpliedDo:
                // An implied DO of an output statement defines its
                // variable, which then keeps its final value; that of an
                // array constructor has a variable of its own, counted as
                // defined all the same.
                if (output) {
                    e.unknown = true;
                    return;
                }
                define(ASR::down_cast<const ASR::ImpliedDoLoop_t>(&x)->m_var,
                    2);
                break;
            case ExprEffect::Unknown:
                e.unknown = true;
                return;
        }
        Base::visit_expr(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call(x.m_name, x.m_args, x.n_args, x.m_dt);
        Base::visit_FunctionCall(x);
    }
};

class ReadCollector : public ASRUtils::BlockBodyWalkVisitor<ReadCollector> {
public:
    std::set<std::string> &arrays;
    explicit ReadCollector(std::set<std::string> &a) : arrays(a) {}
    void insert(ASR::expr_t *e) {
        std::string k = gpu_array_key(e);
        if (!k.empty()) arrays.insert(k);
    }
    void visit_Var(const ASR::Var_t &x) {
        ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
        ASR::ttype_t *t = ASRUtils::expr_type(e);
        if (ASRUtils::is_array(t)) {
            arrays.insert(gpu_array_key(e));
        } else if (is_scalar_struct(t)) {
            insert(e);
        }
    }
    // The object a component is selected from is read only in part: walk
    // its subscripts, but do not count it as read as a whole.
    void visit_component_base(ASR::expr_t *e) {
        while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        }
        if (ASR::is_a<ASR::Var_t>(*e) && !ASRUtils::is_array(
                ASRUtils::expr_type(e))) {
            return;
        }
        visit_expr(*e);
    }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
        ASR::ttype_t *t = ASRUtils::expr_type(e);
        if (ASRUtils::is_array(t)) {
            arrays.insert(gpu_array_key(e));
            return;
        }
        if (is_scalar_struct(t)) insert(e);
        visit_component_base(x.m_v);
    }
};

class SymbolCollector : public ASRUtils::BlockBodyWalkVisitor<
        SymbolCollector> {
public:
    std::set<ASR::symbol_t*> &syms;
    // Whether to leave out the array an inquiry of bounds or size names.
    bool values_only;
    SymbolCollector(std::set<ASR::symbol_t*> &s, bool v)
        : syms(s), values_only(v) {}
    void visit_Var(const ASR::Var_t &x) {
        syms.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }
    static bool is_designator(ASR::expr_t *e) {
        while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        }
        return ASR::is_a<ASR::Var_t>(*e);
    }
    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (values_only && is_designator(x.m_v)) {
            if (x.m_dim) visit_expr(*x.m_dim);
            return;
        }
        ASRUtils::BlockBodyWalkVisitor<SymbolCollector>::visit_ArraySize(x);
    }
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (values_only && is_designator(x.m_v)) {
            if (x.m_dim) visit_expr(*x.m_dim);
            return;
        }
        ASRUtils::BlockBodyWalkVisitor<SymbolCollector>::visit_ArrayBound(x);
    }
};

} // namespace

void gpu_collect_effects(ASR::stmt_t **body, size_t n, GpuEffects &effects) {
    EffectsWalker w(effects);
    for (size_t i = 0; i < n; i++) w.visit_stmt(*body[i]);
}

bool gpu_key_touched_whole(const std::set<std::string> &keys,
        const std::string &key) {
    for (size_t pos = key.find('%'); pos != std::string::npos;
            pos = key.find('%', pos + 1)) {
        if (keys.count(key.substr(0, pos))) return true;
    }
    return false;
}

bool gpu_key_touched(const std::set<std::string> &keys,
        const std::string &key) {
    return keys.count(key) || gpu_key_touched_whole(keys, key);
}

void gpu_collect_array_reads(ASR::expr_t *e, std::set<std::string> &arrays) {
    ReadCollector c(arrays);
    c.visit_expr(*e);
}

void gpu_collect_array_reads(ASR::stmt_t **body, size_t n,
        std::set<std::string> &arrays) {
    ReadCollector c(arrays);
    for (size_t i = 0; i < n; i++) c.visit_stmt(*body[i]);
}

namespace {

bool intrinsic_scalar(ASR::ttype_t *t) {
    if (ASRUtils::is_array(t) || ASRUtils::is_allocatable(t)
            || ASRUtils::is_pointer(t)) {
        return false;
    }
    return ASR::is_a<ASR::Real_t>(*t) || ASR::is_a<ASR::Integer_t>(*t)
        || ASR::is_a<ASR::Logical_t>(*t);
}

// Every statement, those nested in DO, IF, SELECT CASE and BLOCK
// constructs included, passes through visit_stmt, and every expression
// through visit_expr, which accept only the kinds listed.
class SimpleBody : public ASR::BaseWalkVisitor<SimpleBody> {
    using Base = ASR::BaseWalkVisitor<SimpleBody>;
public:
    bool ok = true;
    void visit_stmt(const ASR::stmt_t &x) {
        if (!ok) return;
        switch (x.type) {
            case ASR::stmtType::Assignment: {
                const ASR::Assignment_t &a =
                    *ASR::down_cast<const ASR::Assignment_t>(&x);
                if (a.m_overloaded || a.m_realloc_lhs || a.m_move_allocation
                        || !intrinsic_scalar(ASRUtils::expr_type(a.m_target))
                        || !(ASR::is_a<ASR::Var_t>(*a.m_target)
                            || ASR::is_a<ASR::ArrayItem_t>(*a.m_target))) {
                    ok = false;
                    return;
                }
                break;
            }
            case ASR::stmtType::If:
            case ASR::stmtType::Select:
            case ASR::stmtType::DoLoop:
                break;
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<const ASR::BlockCall_t>(&x)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) {
                    ok = false;
                    return;
                }
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                for (auto &item : blk->m_symtab->get_scope()) {
                    if (ASR::is_a<ASR::Block_t>(*item.second)) continue;
                    if (!ASR::is_a<ASR::Variable_t>(*item.second)
                            || !intrinsic_scalar(ASR::down_cast<
                                ASR::Variable_t>(item.second)->m_type)) {
                        ok = false;
                        return;
                    }
                }
                for (size_t i = 0; i < blk->n_body; i++) {
                    visit_stmt(*blk->m_body[i]);
                }
                return;
            }
            default:
                ok = false;
                return;
        }
        Base::visit_stmt(x);
    }
    void visit_expr(const ASR::expr_t &x) {
        if (!ok) return;
        switch (x.type) {
            case ASR::exprType::ArraySection:
            case ASR::exprType::StructConstructor:
            case ASR::exprType::IntrinsicArrayFunction:
                ok = false;
                return;
            default:
                break;
        }
        ExprEffect k = expr_effect(x);
        if (k != ExprEffect::None && k != ExprEffect::Call) {
            ok = false;
            return;
        }
        Base::visit_expr(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *f = ASRUtils::symbol_get_past_external(x.m_name);
        if (!ASR::is_a<ASR::Function_t>(*f)) {
            ok = false;
            return;
        }
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
        if ((!ft->m_pure && !ft->m_elemental)
                || !intrinsic_scalar(x.m_type)) {
            ok = false;
            return;
        }
        Base::visit_FunctionCall(x);
    }
};

} // namespace

bool gpu_simple_body(ASR::stmt_t **body, size_t n) {
    SimpleBody c;
    for (size_t i = 0; i < n && c.ok; i++) c.visit_stmt(*body[i]);
    return c.ok;
}

namespace {

class ExprReplaceAll : public ASR::CallReplacerOnExpressionsVisitor<
        ExprReplaceAll> {
public:
    const std::function<ASR::expr_t*(ASR::expr_t*)> *f;
    void call_replacer() {
        if (current_expr && *current_expr) {
            ASR::expr_t *r = (*f)(*current_expr);
            if (r) *current_expr = r;
        }
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
    }
};

} // namespace

void gpu_replace_exprs(ASR::stmt_t **body, size_t n,
        const std::function<ASR::expr_t*(ASR::expr_t*)> &f) {
    ExprReplaceAll r;
    r.f = &f;
    for (size_t i = 0; i < n; i++) r.visit_stmt(*body[i]);
}

void gpu_collect_blocks(ASR::stmt_t **body, size_t n,
        std::set<ASR::symbol_t*> &blocks) {
    struct Blocks : public ASRUtils::BlockBodyWalkVisitor<Blocks> {
        std::set<ASR::symbol_t*> *blocks;
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            blocks->insert(ASRUtils::symbol_get_past_external(x.m_m));
            ASRUtils::BlockBodyWalkVisitor<Blocks>::visit_BlockCall(x);
        }
    } c;
    c.blocks = &blocks;
    for (size_t i = 0; i < n; i++) c.visit_stmt(*body[i]);
}

void gpu_collect_symbols(ASR::expr_t *e, std::set<ASR::symbol_t*> &syms) {
    SymbolCollector c(syms, false);
    c.visit_expr(*e);
}

void gpu_collect_symbols(ASR::stmt_t **body, size_t n,
        std::set<ASR::symbol_t*> &syms) {
    SymbolCollector c(syms, false);
    for (size_t i = 0; i < n; i++) c.visit_stmt(*body[i]);
}

void gpu_collect_value_symbols(ASR::expr_t *e,
        std::set<ASR::symbol_t*> &syms) {
    SymbolCollector c(syms, true);
    c.visit_expr(*e);
}

// ---------------------------------------------------------------------------
// Self-contained functions

namespace {

bool self_contained(ASR::Function_t *fn, std::set<ASR::Function_t*> &active);

// Whether the procedure a call names is a function that is
// self-contained.
bool callee_self_contained(ASR::symbol_t *name,
        std::set<ASR::Function_t*> &active) {
    ASR::symbol_t *f = ASRUtils::symbol_get_past_external(name);
    if (!ASR::is_a<ASR::Function_t>(*f)) return false;
    return self_contained(ASR::down_cast<ASR::Function_t>(f), active);
}

class SelfContainedCheck : public ASR::BaseWalkVisitor<SelfContainedCheck> {
public:
    // The scope of the function checked, or null for the calls of a loop
    // body, where any variable may be named.
    SymbolTable *scope = nullptr;
    std::set<ASR::Function_t*> *active;
    bool ok = true;

    void visit_Var(const ASR::Var_t &x) {
        if (scope == nullptr) return;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (!ASR::is_a<ASR::Variable_t>(*s)) {
            // A procedure passed as an actual argument.
            ok = false;
            return;
        }
        if (ASR::down_cast<ASR::Variable_t>(s)->m_storage
                == ASR::storage_typeType::Parameter) {
            return;
        }
        for (SymbolTable *t = ASRUtils::symbol_parent_symtab(s); t;
                t = t->parent) {
            if (t == scope) return;
        }
        ok = false;
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (!callee_self_contained(x.m_name, *active)) ok = false;
        ASR::BaseWalkVisitor<SelfContainedCheck>::visit_FunctionCall(x);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        if (!callee_self_contained(x.m_name, *active)) ok = false;
        ASR::BaseWalkVisitor<SelfContainedCheck>::visit_SubroutineCall(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        // The BLOCKs of a function are in its scope; those of a loop body
        // are visited here.
        if (scope != nullptr) return;
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*b)) {
            ok = false;
            return;
        }
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
    }
};

bool self_contained(ASR::Function_t *fn, std::set<ASR::Function_t*> &active) {
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
    if (ft->m_deftype != ASR::deftypeType::Implementation) return false;
    // A recursive call is checked where it started.
    if (active.count(fn)) return true;
    active.insert(fn);
    SelfContainedCheck c;
    c.scope = fn->m_symtab;
    c.active = &active;
    c.visit_Function(*fn);
    active.erase(fn);
    return c.ok;
}

} // namespace

bool gpu_function_self_contained(ASR::Function_t *fn) {
    std::set<ASR::Function_t*> active;
    return self_contained(fn, active);
}

bool gpu_calls_self_contained(ASR::expr_t *e) {
    std::set<ASR::Function_t*> active;
    SelfContainedCheck c;
    c.active = &active;
    c.visit_expr(*e);
    return c.ok;
}

bool gpu_calls_self_contained(ASR::stmt_t **body, size_t n) {
    std::set<ASR::Function_t*> active;
    SelfContainedCheck c;
    c.active = &active;
    for (size_t i = 0; i < n && c.ok; i++) c.visit_stmt(*body[i]);
    return c.ok;
}

// ---------------------------------------------------------------------------
// Cost

int64_t gpu_expr_cost(ASR::expr_t *e, int depth) {
    if (e == nullptr) return 0;
    struct Cost : public ASR::BaseWalkVisitor<Cost> {
        int64_t n = 0;
        int depth;
        void visit_RealBinOp(const ASR::RealBinOp_t &x) {
            n++;
            ASR::BaseWalkVisitor<Cost>::visit_RealBinOp(x);
        }
        void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
            n++;
            ASR::BaseWalkVisitor<Cost>::visit_IntegerBinOp(x);
        }
        void visit_ArrayItem(const ASR::ArrayItem_t &x) {
            n += 2;
            ASR::BaseWalkVisitor<Cost>::visit_ArrayItem(x);
        }
        void visit_IntrinsicElementalFunction(
                const ASR::IntrinsicElementalFunction_t &x) {
            n += 4;
            ASR::BaseWalkVisitor<Cost>::visit_IntrinsicElementalFunction(x);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            ASR::symbol_t *f = ASRUtils::symbol_get_past_external(x.m_name);
            if (depth < 8 && ASR::is_a<ASR::Function_t>(*f)
                    && gpu_function_inlinable(
                        ASR::down_cast<ASR::Function_t>(f))) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
                n += gpu_stmts_cost(fn->m_body, fn->n_body, depth + 1);
            } else {
                n += 100;
            }
            ASR::BaseWalkVisitor<Cost>::visit_FunctionCall(x);
        }
    } c;
    c.depth = depth;
    c.visit_expr(*e);
    return c.n;
}

int64_t gpu_stmts_cost(ASR::stmt_t **body, size_t n, int depth) {
    int64_t total = 0;
    for (size_t i = 0; i < n; i++) {
        ASR::stmt_t *s = body[i];
        if (ASR::is_a<ASR::Assignment_t>(*s)) {
            ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
            total += gpu_expr_cost(a->m_value, depth) + (
                ASR::is_a<ASR::ArrayItem_t>(*a->m_target) ? 2 : 0);
        } else if (ASR::is_a<ASR::If_t>(*s)) {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
            total += gpu_expr_cost(x->m_test, depth) + 1 + std::max(
                gpu_stmts_cost(x->m_body, x->n_body, depth),
                gpu_stmts_cost(x->m_orelse, x->n_orelse, depth));
        } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
            if (ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *bb = ASR::down_cast<ASR::Block_t>(b);
                total += gpu_stmts_cost(bb->m_body, bb->n_body, depth);
            }
        } else {
            total += 10;
        }
    }
    return total;
}

// ---------------------------------------------------------------------------
// Inlining

bool gpu_expr_duplicable(ASR::expr_t *e) {
    struct Check : public ASR::BaseWalkVisitor<Check> {
        bool ok = true;
        void visit_FunctionCall(const ASR::FunctionCall_t &) { ok = false; }
    } c;
    c.visit_expr(*e);
    return c.ok;
}

namespace {

class BodyCheck : public ASR::BaseWalkVisitor<BodyCheck> {
public:
    bool ok = true;
    void visit_Return(const ASR::Return_t &) { ok = false; }
    void visit_Stop(const ASR::Stop_t &) { ok = false; }
    void visit_ErrorStop(const ASR::ErrorStop_t &) { ok = false; }
    void visit_GoTo(const ASR::GoTo_t &) { ok = false; }
    void visit_GoToTarget(const ASR::GoToTarget_t &) { ok = false; }
    void visit_Print(const ASR::Print_t &) { ok = false; }
    void visit_FileWrite(const ASR::FileWrite_t &) { ok = false; }
    void visit_FileRead(const ASR::FileRead_t &) { ok = false; }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &) { ok = false; }
    void visit_IntrinsicImpureSubroutine(
        const ASR::IntrinsicImpureSubroutine_t &) { ok = false; }
    void visit_Allocate(const ASR::Allocate_t &) { ok = false; }
    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &) {
        ok = false;
    }
    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &) {}
    void visit_BlockCall(const ASR::BlockCall_t &) { ok = false; }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &) {
        ok = false;
    }
    void visit_OMPRegion(const ASR::OMPRegion_t &) { ok = false; }
    void visit_WhileLoop(const ASR::WhileLoop_t &) { ok = false; }
    void visit_Exit(const ASR::Exit_t &) { ok = false; }
    void visit_Cycle(const ASR::Cycle_t &) { ok = false; }
};

bool scalar_value_type(ASR::ttype_t *t) {
    t = ASRUtils::type_get_past_allocatable_pointer(t);
    return ASR::is_a<ASR::Real_t>(*t) || ASR::is_a<ASR::Integer_t>(*t)
        || ASR::is_a<ASR::Logical_t>(*t);
}

bool assumed_shape(ASR::ttype_t *t) {
    if (ASRUtils::is_allocatable(t) || ASRUtils::is_pointer(t)) return false;
    if (!ASR::is_a<ASR::Array_t>(*t)) return false;
    ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(t);
    if (a->m_physical_type != ASR::array_physical_typeType::DescriptorArray) {
        return false;
    }
    for (size_t d = 0; d < a->n_dims; d++) {
        if (a->m_dims[d].m_length != nullptr) return false;
        if (a->m_dims[d].m_start != nullptr) {
            int64_t v;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(
                    a->m_dims[d].m_start), v)) {
                return false;
            }
        }
    }
    return scalar_value_type(a->m_type);
}

} // namespace

bool gpu_function_inlinable(ASR::Function_t *fn) {
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
    if (ft->m_deftype != ASR::deftypeType::Implementation
            || ft->m_abi != ASR::abiType::Source) {
        return false;
    }
    if (!ft->m_pure && !ft->m_elemental) return false;
    if (fn->m_return_var == nullptr || fn->n_body == 0) return false;
    if (!scalar_value_type(ASRUtils::expr_type(fn->m_return_var))
            || ASRUtils::is_array(ASRUtils::expr_type(fn->m_return_var))) {
        return false;
    }
    std::set<ASR::symbol_t*> args;
    for (size_t i = 0; i < fn->n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) return false;
        ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_presence == ASR::presenceType::Optional) return false;
        if (ASRUtils::is_array(v->m_type)) {
            if (!assumed_shape(v->m_type)) return false;
        } else if (!scalar_value_type(v->m_type)
                || ASRUtils::is_allocatable(v->m_type)
                || ASRUtils::is_pointer(v->m_type)) {
            return false;
        }
        args.insert(s);
    }
    for (auto &item : fn->m_symtab->get_scope()) {
        ASR::symbol_t *s = item.second;
        if (!ASR::is_a<ASR::Variable_t>(*s)) {
            // An interface or an external symbol of the function's own
            // scope cannot be named from the caller.
            return false;
        }
        if (args.count(s)) continue;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_storage == ASR::storage_typeType::Parameter) continue;
        if (v->m_symbolic_value || v->m_value
                || v->m_storage == ASR::storage_typeType::Save
                || !scalar_value_type(v->m_type)
                || ASRUtils::is_array(v->m_type)
                || ASRUtils::is_allocatable(v->m_type)
                || ASRUtils::is_pointer(v->m_type)) {
            return false;
        }
    }
    BodyCheck check;
    for (size_t i = 0; i < fn->n_body; i++) check.visit_stmt(*fn->m_body[i]);
    return check.ok;
}

namespace {

// Whether a variable of the callee may be assigned in its body.
bool assigned_in(ASR::Function_t *fn, ASR::symbol_t *s) {
    GpuEffects e;
    gpu_collect_effects(fn->m_body, fn->n_body, e);
    return e.unknown || e.scalars.count(s) > 0;
}

} // namespace

// Replaces the picked calls of an expression by the variables their
// inlined bodies assign, appending those bodies to `pre`.
class GpuCallReplacer : public ASR::BaseExprReplacer<GpuCallReplacer> {
public:
    GpuInliner &inl;
    SymbolTable *block;
    Vec<ASR::stmt_t*> &pre;
    // Inside an operand that need not be evaluated (the second operand of
    // a logical operator, a branch of merge): a picked call there cannot
    // be moved before the statement.
    int conditional = 0;
    bool ok = true;
    GpuCallReplacer(GpuInliner &inl_, SymbolTable *block_,
        Vec<ASR::stmt_t*> &pre_) : inl(inl_), block(block_), pre(pre_) {}

    void replace_LogicalBinOp(ASR::LogicalBinOp_t *x) {
        ASR::expr_t **cur = current_expr;
        current_expr = &x->m_left;
        replace_expr(x->m_left);
        conditional++;
        current_expr = &x->m_right;
        replace_expr(x->m_right);
        conditional--;
        current_expr = cur;
    }
    void replace_IfExp(ASR::IfExp_t *x) {
        ASR::expr_t **cur = current_expr;
        current_expr = &x->m_test;
        replace_expr(x->m_test);
        conditional++;
        current_expr = &x->m_body;
        replace_expr(x->m_body);
        current_expr = &x->m_orelse;
        replace_expr(x->m_orelse);
        conditional--;
        current_expr = cur;
    }
    void replace_IntrinsicElementalFunction(
            ASR::IntrinsicElementalFunction_t *x) {
        bool is_merge = x->m_intrinsic_id == static_cast<int64_t>(
            ASRUtils::IntrinsicElementalFunctions::Merge);
        if (is_merge) conditional++;
        ASR::BaseExprReplacer<GpuCallReplacer>::
            replace_IntrinsicElementalFunction(x);
        if (is_merge) conditional--;
    }
    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        ASR::BaseExprReplacer<GpuCallReplacer>::replace_FunctionCall(x);
        if (!ok || !inl.select(x)) return;
        if (conditional > 0) {
            ok = false;
            return;
        }
        ASR::expr_t *r = inl.inline_call(x, block, pre);
        if (r == nullptr) {
            ok = false;
            return;
        }
        *current_expr = r;
    }
};

namespace {

// Whether an expression has a call `select` picks.
bool has_picked_call(ASR::expr_t *e,
        const std::function<bool(ASR::FunctionCall_t*)> &select) {
    if (e == nullptr) return false;
    struct Find : public ASR::BaseWalkVisitor<Find> {
        const std::function<bool(ASR::FunctionCall_t*)> *select;
        bool found = false;
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            if ((*select)(const_cast<ASR::FunctionCall_t*>(&x))) {
                found = true;
            }
            ASR::BaseWalkVisitor<Find>::visit_FunctionCall(x);
        }
    } f;
    f.select = &select;
    f.visit_expr(*e);
    return f.found;
}

} // namespace

ASR::expr_t *GpuInliner::inline_call(ASR::FunctionCall_t *call,
        SymbolTable *block, Vec<ASR::stmt_t*> &pre) {
    ASR::symbol_t *f = ASRUtils::symbol_get_past_external(call->m_name);
    if (!ASR::is_a<ASR::Function_t>(*f)) return nullptr;
    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
    if (!gpu_function_inlinable(fn) || fn->n_args != call->n_args
            || call->m_dt != nullptr) {
        return nullptr;
    }
    Location loc = call->base.base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    GpuDuplicator dup(al, block);
    dup.added = &added;
    std::string prefix = std::string("__") + fn->m_name + "_";
    auto new_var = [&](ASR::symbol_t *old, const std::string &base) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(old);
        std::string name = block->get_unique_name(prefix + base, false);
        ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(b.Variable(block,
            name, ASRUtils::duplicate_type(al, v->m_type),
            ASR::intentType::Local))->m_v;
        added.push_back({block, name});
        return s;
    };
    for (size_t i = 0; i < fn->n_args; i++) {
        ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
        ASR::Variable_t *dv = ASR::down_cast<ASR::Variable_t>(dummy);
        ASR::expr_t *actual = call->m_args[i].m_value;
        if (actual == nullptr) return nullptr;
        if (ASRUtils::is_array(dv->m_type)) {
            // A change of the descriptor of the actual is not one of its
            // elements.
            while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*actual)) {
                actual = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    actual)->m_arg;
            }
            if (gpu_array_key(actual).empty()) return nullptr;
            ASR::ttype_t *at = ASRUtils::expr_type(actual);
            if (!ASRUtils::is_array(at) || ASRUtils::extract_n_dims_from_ttype(
                    at) != ASRUtils::extract_n_dims_from_ttype(dv->m_type)) {
                return nullptr;
            }
            GpuDuplicator::ArrayActual aa;
            aa.actual = actual;
            ASR::Array_t *da = ASR::down_cast<ASR::Array_t>(dv->m_type);
            ASR::dimension_t *adims = nullptr;
            size_t n = ASRUtils::extract_dimensions_from_ttype(at, adims);
            for (size_t d = 0; d < n; d++) {
                int64_t dl = 1;
                if (da->m_dims[d].m_start) {
                    ASRUtils::extract_value(ASRUtils::expr_value(
                        da->m_dims[d].m_start), dl);
                }
                aa.lower.push_back(gpu_i32(al, loc, dl));
                // The lower bound of the actual: stated as a constant by
                // its type, or read from it.
                int64_t al_const = 0;
                ASR::expr_t *alb = nullptr;
                if (!ASRUtils::is_allocatable(at) && !ASRUtils::is_pointer(at)
                        && adims && adims[d].m_length != nullptr) {
                    if (adims[d].m_start == nullptr) {
                        al_const = 1;
                        alb = gpu_i32(al, loc, 1);
                    } else if (ASRUtils::extract_value(ASRUtils::expr_value(
                            adims[d].m_start), al_const)) {
                        alb = gpu_i32(al, loc, al_const);
                    }
                }
                if (alb && al_const == dl) {
                    aa.shift.push_back(nullptr);
                } else if (alb) {
                    aa.shift.push_back(gpu_i32(al, loc, al_const - dl));
                } else {
                    ASR::expr_t *lb = ASRUtils::EXPR(ASR::make_ArrayBound_t(
                        al, loc, actual, gpu_i32(al, loc, d + 1),
                        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
                        ASR::arrayboundType::LBound, nullptr));
                    aa.shift.push_back(gpu_int_sub(al, lb,
                        gpu_i32(al, loc, dl)));
                }
            }
            dup.array_map[dummy] = aa;
        } else if (!assigned_in(fn, dummy) && gpu_expr_duplicable(actual)) {
            dup.value_map[dummy] = actual;
        } else {
            ASR::symbol_t *t = new_var(dummy, dv->m_name);
            pre.push_back(al, b.Assignment(b.Var(t), actual));
            dup.symbol_map[dummy] = t;
        }
    }
    ASR::symbol_t *result = ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v;
    for (auto &item : fn->m_symtab->get_scope()) {
        ASR::symbol_t *s = item.second;
        if (dup.value_map.count(s) || dup.array_map.count(s)
                || dup.symbol_map.count(s)) {
            continue;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_storage == ASR::storage_typeType::Parameter) {
            if (v->m_value == nullptr) return nullptr;
            dup.value_map[s] = v->m_value;
            continue;
        }
        dup.symbol_map[s] = new_var(s, item.first);
    }
    Vec<ASR::stmt_t*> body;
    dup.duplicate_body(fn->m_body, fn->n_body, body);
    if (!dup.ok) return nullptr;
    // Every variable and procedure the copied body names has to be one the
    // caller's scope reaches by its name.
    std::set<ASR::symbol_t*> syms;
    gpu_collect_symbols(body.p, body.n, syms);
    for (ASR::symbol_t *s : syms) {
        if (ASRUtils::symbol_parent_symtab(s) == block) continue;
        bool local = false;
        for (auto &m : dup.symbol_map) if (m.second == s) local = true;
        if (local) continue;
        ASR::symbol_t *r = block->resolve_symbol(ASRUtils::symbol_name(s));
        if (r == nullptr || ASRUtils::symbol_get_past_external(r)
                != ASRUtils::symbol_get_past_external(s)) {
            return nullptr;
        }
    }
    struct Calls : public ASR::BaseWalkVisitor<Calls> {
        SymbolTable *scope;
        bool ok = true;
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            ASR::symbol_t *r = scope->resolve_symbol(
                ASRUtils::symbol_name(x.m_name));
            if (r == nullptr || ASRUtils::symbol_get_past_external(r)
                    != ASRUtils::symbol_get_past_external(x.m_name)) {
                ok = false;
            }
            ASR::BaseWalkVisitor<Calls>::visit_FunctionCall(x);
        }
    } calls;
    calls.scope = block;
    for (size_t i = 0; i < body.n; i++) calls.visit_stmt(*body[i]);
    if (!calls.ok) return nullptr;
    // The calls the copied body makes are inlined too, when picked.
    depth++;
    if (depth > 8) {
        depth--;
        return nullptr;
    }
    ASR::stmt_t **bp = body.p;
    size_t bn = body.n;
    bool ok = inline_stmts(block, bp, bn);
    depth--;
    if (!ok) return nullptr;
    for (size_t i = 0; i < bn; i++) pre.push_back(al, bp[i]);
    return b.Var(dup.symbol_map[result]);
}

bool GpuInliner::inline_stmt(SymbolTable *scope, ASR::stmt_t *&s) {
    // The expressions the statement itself evaluates.
    std::vector<ASR::expr_t**> own;
    switch (s->type) {
        case ASR::stmtType::Assignment: {
            ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
            if (a->m_overloaded) return !has_picked_call(a->m_value, select);
            own.push_back(&a->m_target);
            own.push_back(&a->m_value);
            break;
        }
        case ASR::stmtType::If:
            own.push_back(&ASR::down_cast<ASR::If_t>(s)->m_test);
            break;
        case ASR::stmtType::Select:
            own.push_back(&ASR::down_cast<ASR::Select_t>(s)->m_test);
            break;
        case ASR::stmtType::DoLoop: {
            ASR::DoLoop_t *l = ASR::down_cast<ASR::DoLoop_t>(s);
            own.push_back(&l->m_head.m_start);
            own.push_back(&l->m_head.m_end);
            if (l->m_head.m_increment) own.push_back(&l->m_head.m_increment);
            break;
        }
        case ASR::stmtType::WhileLoop: {
            ASR::WhileLoop_t *w = ASR::down_cast<ASR::WhileLoop_t>(s);
            if (has_picked_call(w->m_test, select)) return false;
            break;
        }
        case ASR::stmtType::BlockCall: {
            ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
            if (!ASR::is_a<ASR::Block_t>(*bs)) return true;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
            return inline_stmts(blk->m_symtab, blk->m_body, blk->n_body);
        }
        default: {
            // A statement of another kind must not hold a picked call.
            struct Find : public ASR::BaseWalkVisitor<Find> {
                const std::function<bool(ASR::FunctionCall_t*)> *select;
                bool found = false;
                void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                    if ((*select)(const_cast<ASR::FunctionCall_t*>(&x))) {
                        found = true;
                    }
                }
            } f;
            f.select = &select;
            f.visit_stmt(*s);
            return !f.found;
        }
    }
    bool any = false;
    for (ASR::expr_t **e : own) {
        if (*e && has_picked_call(*e, select)) any = true;
    }
    SymbolTable *inner = scope;
    ASR::Block_t *blk = nullptr;
    Vec<ASR::stmt_t*> pre;
    pre.reserve(al, 4);
    if (any) {
        SymbolTable *symtab = al.make_new<SymbolTable>(scope);
        std::string name = unique_block_name(scope, "__gpu_inline");
        ASR::asr_t *b = ASR::make_Block_t(al, s->base.loc, symtab,
            s2c(al, name), nullptr, 0);
        symtab->asr_owner = b;
        scope->add_symbol(name, ASR::down_cast<ASR::symbol_t>(b));
        added.push_back({scope, name});
        blk = ASR::down_cast<ASR::Block_t>(ASR::down_cast<ASR::symbol_t>(b));
        inner = symtab;
        GpuCallReplacer r(*this, symtab, pre);
        for (ASR::expr_t **e : own) {
            if (*e == nullptr) continue;
            r.current_expr = e;
            r.replace_expr(*e);
            if (!r.ok) return false;
        }
    }
    // The statements the statement runs.
    switch (s->type) {
        case ASR::stmtType::If: {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
            if (!inline_stmts(inner, x->m_body, x->n_body)
                    || !inline_stmts(inner, x->m_orelse, x->n_orelse)) {
                return false;
            }
            break;
        }
        case ASR::stmtType::Select: {
            ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
            for (size_t i = 0; i < x->n_body; i++) {
                ASR::case_stmt_t *c = x->m_body[i];
                if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                    ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(c);
                    if (!inline_stmts(inner, cs->m_body, cs->n_body)) {
                        return false;
                    }
                } else {
                    ASR::CaseStmt_Range_t *cs =
                        ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                    if (!inline_stmts(inner, cs->m_body, cs->n_body)) {
                        return false;
                    }
                }
            }
            if (!inline_stmts(inner, x->m_default, x->n_default)) {
                return false;
            }
            break;
        }
        case ASR::stmtType::DoLoop: {
            ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
            if (!inline_stmts(inner, x->m_body, x->n_body)) return false;
            break;
        }
        case ASR::stmtType::WhileLoop: {
            ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
            if (!inline_stmts(inner, x->m_body, x->n_body)) return false;
            break;
        }
        default:
            break;
    }
    if (blk) {
        pre.push_back(al, s);
        blk->m_body = pre.p;
        blk->n_body = pre.n;
        s = ASRUtils::STMT(ASR::make_BlockCall_t(al, s->base.loc, -1,
            ASR::down_cast<ASR::symbol_t>((ASR::asr_t*)blk)));
    }
    return true;
}

bool GpuInliner::inline_stmts(SymbolTable *scope, ASR::stmt_t **&body,
        size_t &n_body) {
    for (size_t i = 0; i < n_body; i++) {
        if (!inline_stmt(scope, body[i])) return false;
    }
    return true;
}

bool GpuInliner::run(SymbolTable *scope, ASR::stmt_t **&body,
        size_t &n_body) {
    return inline_stmts(scope, body, n_body);
}

void GpuInliner::rollback() {
    for (auto it = added.rbegin(); it != added.rend(); ++it) {
        if (it->first->get_symbol(it->second)) {
            it->first->erase_symbol(it->second);
        }
    }
    added.clear();
}

} // namespace LCompilers
