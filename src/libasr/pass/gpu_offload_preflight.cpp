#include <algorithm>

#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_offload_preflight.h>

namespace LCompilers {

bool match_gpu_struct_array_member_extent_base(ASR::expr_t *expr,
        int64_t d, const std::vector<std::string> &arg_names,
        GpuStructArrayMemberExtentRef &out) {
    if (d < 1) return false;
    ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(expr);
    if (!base || !ASR::is_a<ASR::StructInstanceMember_t>(*base)) {
        return false;
    }
    ASR::StructInstanceMember_t *sim =
        ASR::down_cast<ASR::StructInstanceMember_t>(base);
    ASR::expr_t *inner = ASRUtils::get_past_array_physical_cast(sim->m_v);
    if (!inner) return false;
    ASR::ArrayItem_t *ai = nullptr;
    ASR::expr_t *root = inner;
    if (ASR::is_a<ASR::ArrayItem_t>(*inner)) {
        ai = ASR::down_cast<ASR::ArrayItem_t>(inner);
        root = ASRUtils::get_past_array_physical_cast(ai->m_v);
    }
    if (!root || !ASR::is_a<ASR::Var_t>(*root)) return false;
    std::string arr_name = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(root)->m_v);
    bool is_arg = false;
    for (auto &a : arg_names) {
        if (a == arr_name) { is_arg = true; break; }
    }
    if (!is_arg) return false;
    ASR::ttype_t *mem_t =
        ASRUtils::type_get_past_allocatable_pointer(sim->m_type);
    if (!ASR::is_a<ASR::Array_t>(*mem_t)) return false;
    if ((size_t)d > ASR::down_cast<ASR::Array_t>(mem_t)->n_dims) {
        return false;
    }
    // Every subscript has to be reproducible in the host scope too: a
    // compile-time integer constant (a named constant included), or a
    // scalar the kernel is handed as an argument.
    for (size_t k = 0; ai != nullptr && k < ai->n_args; k++) {
        ASR::array_index_t &ix = ai->m_args[k];
        if (ix.m_left || ix.m_step || !ix.m_right) return false;
        int64_t sub_val;
        if (try_eval_int_constant(ix.m_right, sub_val)) continue;
        if (!ASR::is_a<ASR::Var_t>(*ix.m_right)) return false;
        std::string sub_name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(ix.m_right)->m_v);
        bool sub_is_arg = false;
        for (auto &a : arg_names) {
            if (a == sub_name) { sub_is_arg = true; break; }
        }
        if (!sub_is_arg) return false;
    }
    out.item = ai;
    out.base = ASR::down_cast<ASR::Var_t>(root);
    out.member = sim;
    out.dim = d;
    return true;
}

bool match_gpu_struct_array_member_extent(ASR::expr_t *expr,
        const std::vector<std::string> &arg_names,
        GpuStructArrayMemberExtentRef &out) {
    if (!expr || !ASR::is_a<ASR::ArraySize_t>(*expr)) return false;
    ASR::ArraySize_t *as = ASR::down_cast<ASR::ArraySize_t>(expr);
    if (!as->m_dim || !ASR::is_a<ASR::IntegerConstant_t>(*as->m_dim)) {
        return false;
    }
    if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::extract_type(as->m_type))) {
        return false;
    }
    return match_gpu_struct_array_member_extent_base(as->m_v,
        ASR::down_cast<ASR::IntegerConstant_t>(as->m_dim)->m_n,
        arg_names, out);
}

// The value of the first whole-array assignment to `var_name` in `body`,
// or nullptr.  A local that carries no shape anywhere else is sized by
// what is assigned to it.
// An array-valued operand of the elementwise expression `e`, or nullptr
// when `e` is not built elementwise or has no array operand of its own.
//
// `sqrt((x-x0)**2 + (y(j)-y0)**2)` carries no dimensions in its type --
// an elementwise result is a descriptor with empty extents -- but every
// array operand of an elementwise expression is conformable with the
// result, so any one of them has the result's shape.  A broadcast operand
// is skipped: it is a scalar given a shape borrowed from elsewhere, so it
// is never the operand that fixes the shape.
static ASR::expr_t* gpu_elemental_shape_source(ASR::expr_t *e) {
    if (!e) return nullptr;
    ASR::expr_t *operands[2] = {nullptr, nullptr};
    size_t n_operands = 0;
    switch (e->type) {
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *o =
                ASR::down_cast<ASR::IntegerBinOp_t>(e);
            operands[0] = o->m_left; operands[1] = o->m_right;
            n_operands = 2; break;
        }
        case ASR::exprType::RealBinOp: {
            ASR::RealBinOp_t *o = ASR::down_cast<ASR::RealBinOp_t>(e);
            operands[0] = o->m_left; operands[1] = o->m_right;
            n_operands = 2; break;
        }
        case ASR::exprType::ComplexBinOp: {
            ASR::ComplexBinOp_t *o =
                ASR::down_cast<ASR::ComplexBinOp_t>(e);
            operands[0] = o->m_left; operands[1] = o->m_right;
            n_operands = 2; break;
        }
        case ASR::exprType::LogicalBinOp: {
            ASR::LogicalBinOp_t *o =
                ASR::down_cast<ASR::LogicalBinOp_t>(e);
            operands[0] = o->m_left; operands[1] = o->m_right;
            n_operands = 2; break;
        }
        case ASR::exprType::IntegerUnaryMinus: {
            operands[0] =
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg;
            n_operands = 1; break;
        }
        case ASR::exprType::RealUnaryMinus: {
            operands[0] = ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg;
            n_operands = 1; break;
        }
        case ASR::exprType::ComplexUnaryMinus: {
            operands[0] =
                ASR::down_cast<ASR::ComplexUnaryMinus_t>(e)->m_arg;
            n_operands = 1; break;
        }
        case ASR::exprType::LogicalNot: {
            operands[0] = ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg;
            n_operands = 1; break;
        }
        case ASR::exprType::Cast: {
            operands[0] = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
            n_operands = 1; break;
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t *f =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            for (size_t i = 0; i < f->n_args; i++) {
                ASR::expr_t *a =
                    ASRUtils::get_past_array_physical_cast(f->m_args[i]);
                if (!a || ASR::is_a<ASR::ArrayBroadcast_t>(*a)) continue;
                if (ASRUtils::is_array(ASRUtils::expr_type(a))) return a;
            }
            return nullptr;
        }
        default: return nullptr;
    }
    for (size_t i = 0; i < n_operands; i++) {
        ASR::expr_t *a = ASRUtils::get_past_array_physical_cast(operands[i]);
        if (!a || ASR::is_a<ASR::ArrayBroadcast_t>(*a)) continue;
        if (ASRUtils::is_array(ASRUtils::expr_type(a))) return a;
    }
    return nullptr;
}

static ASR::expr_t* find_gpu_first_assigned_value(ASR::stmt_t **body,
        size_t n_body, const std::string &var_name) {
    for (size_t i = 0; i < n_body; i++) {
        if (!ASR::is_a<ASR::Assignment_t>(*body[i])) continue;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(body[i]);
        if (!ASR::is_a<ASR::Var_t>(*a->m_target)) continue;
        std::string tname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
        if (tname != var_name) continue;
        return a->m_value;
    }
    return nullptr;
}

ASR::expr_t* gpu_scope_array_shape_source(const ASR::Variable_t *var,
        ASR::stmt_t **body, size_t n_body) {
    if (!ASRUtils::is_allocatable(var->m_type)) return nullptr;
    ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*inner)) return nullptr;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);
    if (arr->n_dims == 0) return nullptr;
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_length) return nullptr;
    }
    std::string vname(var->m_name);
    if (find_allocate_for_var(body, n_body, vname)) return nullptr;
    ASR::expr_t *value = find_gpu_first_assigned_value(body, n_body, vname);
    if (!value) return nullptr;
    ASR::expr_t *src = ASRUtils::get_past_array_physical_cast(value);
    while (src) {
        ASR::expr_t *next = gpu_elemental_shape_source(src);
        if (!next) break;
        src = next;
    }
    if (!src || !ASRUtils::is_array(ASRUtils::expr_type(src))) {
        return nullptr;
    }
    if (ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(src))
            != static_cast<int>(arr->n_dims)) {
        return nullptr;
    }
    return src;
}

// Pre-flight for the offload pass: would the backend be able to size every
// per-thread workspace the loop body needs?
//
// The backend describes a workspace with `declared_shape_to_vla_workspace`
// or `alloc_shape_to_vla_workspace`, and an array whose extents it cannot
// describe is simply left to the device language, which then reports that
// the size is not a constant expression -- a code-generation error, far too
// late, because the offload has been committed to by then. Asking the very
// same two functions here lets the pass decline instead and leave the loop
// on the host, and means the pre-flight and the backend cannot disagree.
static bool gpu_scope_workspaces_resolvable(SymbolTable *symtab,
        ASR::stmt_t **body, size_t n_body,
        ASR::stmt_t **root_body, size_t root_n,
        const std::vector<std::string> &arg_names,
        std::string &unresolved_name) {
    if (symtab == nullptr) return true;
    for (auto &item : symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(item.second);
        ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(
            var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);
        std::string vname(var->m_name);
        GpuVlaWorkspace ws;
        ws.var = item.second;
        if (!ASRUtils::is_allocatable(var->m_type)) {
            // An automatic array: every extent is in its own type. One
            // that is a compile-time constant needs no workspace at all.
            bool runtime = false;
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_length &&
                        !ASR::is_a<ASR::IntegerConstant_t>(
                            *arr->m_dims[d].m_length)) {
                    runtime = true;
                    break;
                }
            }
            if (!runtime) continue;
            if (declared_shape_to_vla_workspace(arr, vname,
                    GpuExtentScope{nullptr, arg_names, symtab, root_body,
                        root_n}, ws)) {
                continue;
            }
            unresolved_name = vname;
            return false;
        }
        ASR::Allocate_t *alloc = find_allocate_for_var(body, n_body, vname);
        if (alloc == nullptr) continue;
        ASR::alloc_arg_t *target = find_alloc_arg_for_var(alloc, vname);
        if (target == nullptr) continue;
        bool runtime = false;
        for (size_t d = 0; d < target->n_dims; d++) {
            if (target->m_dims[d].m_length &&
                    !ASR::is_a<ASR::IntegerConstant_t>(
                        *target->m_dims[d].m_length)) {
                runtime = true;
                break;
            }
        }
        if (!runtime) continue;
        if (alloc_shape_to_vla_workspace(*target, arr, vname,
                GpuExtentScope{nullptr, arg_names, symtab, root_body,
                    root_n}, ws)) {
            continue;
        }
        unresolved_name = vname;
        return false;
    }
    return true;
}

bool gpu_block_workspace_extents_resolvable(
        ASR::stmt_t **body, size_t n_body,
        const std::vector<std::string> &arg_names,
        std::string &unresolved_name) {
    bool ok = true;
    gpu_walk_scopes(body, n_body,
        [&](SymbolTable *symtab, ASR::stmt_t **sbody, size_t sn) {
            if (!ok) return;
            if (!gpu_scope_workspaces_resolvable(symtab, sbody, sn,
                    body, n_body, arg_names, unresolved_name)) {
                ok = false;
            }
        });
    return ok;
}

// An out-of-line result becomes a caller-owned buffer. An unconditional
// allocation may establish its shape, but every other allocation must
// agree: selecting one branch or the last allocation is not sound.
class GpuResultAllocationChecker :
        public ASRUtils::BlockBodyWalkVisitor<GpuResultAllocationChecker> {
    ASR::symbol_t *result;
    size_t depth = 0;
    size_t allocations = 0;
    std::vector<int64_t> buffer_shape;

    bool is_result(ASR::expr_t *target) const {
        target = ASRUtils::get_past_array_physical_cast(target);
        return target && ASR::is_a<ASR::Var_t>(*target) &&
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(target)->m_v) == result;
    }

    static std::vector<ASR::expr_t*> lengths_of(const ASR::alloc_arg_t &arg) {
        std::vector<ASR::expr_t*> lengths;
        for (size_t d = 0; d < arg.n_dims; d++) {
            lengths.push_back(arg.m_dims[d].m_length);
        }
        return lengths;
    }

    static std::vector<int64_t> constant_shape(
            const std::vector<ASR::expr_t*> &lengths) {
        std::vector<int64_t> shape;
        for (ASR::expr_t *length : lengths) {
            int64_t extent = 0;
            if (!try_eval_int_constant(length, extent)) return {};
            shape.push_back(std::max<int64_t>(extent, 0));
        }
        return shape;
    }

    void record(const std::vector<ASR::expr_t*> &lengths) {
        allocations++;
        if (!buffer_shape.empty()) {
            // Equal element counts alone are insufficient when individual
            // dimensions have different lengths.
            if (constant_shape(lengths) != buffer_shape) supported = false;
        } else if (depth != 1 || allocations > 1) {
            supported = false;
        }
    }

    void record(const ASR::alloc_arg_t &arg) {
        if (is_result(arg.m_a)) record(lengths_of(arg));
    }

public:
    bool supported = true;

    GpuResultAllocationChecker(ASR::symbol_t *result,
            ASR::stmt_t **body, size_t n_body) : result(result) {
        // The out-of-line call path propagates an explicit top-level
        // allocation to the caller's result buffer.
        for (size_t i = 0; i < n_body; i++) {
            if (!ASR::is_a<ASR::Allocate_t>(*body[i])) continue;
            ASR::Allocate_t *alloc = ASR::down_cast<ASR::Allocate_t>(body[i]);
            for (size_t j = 0; j < alloc->n_args; j++) {
                if (!is_result(alloc->m_args[j].m_a)) continue;
                buffer_shape = constant_shape(lengths_of(alloc->m_args[j]));
                if (!buffer_shape.empty()) return;
            }
        }
    }

    void visit_stmt(const ASR::stmt_t &stmt) {
        depth++;
        ASR::BaseWalkVisitor<GpuResultAllocationChecker>::visit_stmt(stmt);
        depth--;
    }

    void visit_Allocate(const ASR::Allocate_t &x) {
        for (size_t i = 0; i < x.n_args; i++) record(x.m_args[i]);
    }

    void visit_ReAlloc(const ASR::ReAlloc_t &x) {
        for (size_t i = 0; i < x.n_args; i++) record(x.m_args[i]);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (!is_result(x.m_target)) return;
        ASR::expr_t *value = ASRUtils::get_past_array_physical_cast(x.m_value);
        // Scalar broadcast changes the elements, not the allocation.
        if ((x.m_realloc_lhs || x.m_move_allocation) &&
                ASRUtils::is_array(ASRUtils::expr_type(value)) &&
                !ASR::is_a<ASR::ArrayBroadcast_t>(*value)) {
            std::vector<ASR::expr_t*> lengths;
            if (!gpu_expr_shape_extents(value, nullptr, lengths)) {
                lengths.clear();
            }
            record(lengths);
        }
    }
};

bool gpu_function_result_allocation_is_supported(const ASR::Function_t &fn) {
    if (!fn.m_return_var ||
            !ASRUtils::is_allocatable(ASRUtils::expr_type(fn.m_return_var)) ||
            !ASRUtils::is_array(ASRUtils::expr_type(fn.m_return_var))) {
        return true;
    }
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Var_t>(*fn.m_return_var));
    GpuResultAllocationChecker checker(ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(fn.m_return_var)->m_v),
        fn.m_body, fn.n_body);
    for (size_t i = 0; i < fn.n_body; i++) {
        checker.visit_stmt(*fn.m_body[i]);
    }
    return checker.supported;
}

bool gpu_struct_members_ok(ASR::symbol_t *struct_sym,
        std::set<ASR::Struct_t*> &visited,
        const GpuDeviceCapabilities &caps) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(struct_sym);
    if (!s || !ASR::is_a<ASR::Struct_t>(*s)) {
        // The derived type cannot be inspected, so it cannot be shown to
        // be representable: keep the loop on the CPU.
        return false;
    }
    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
    if (!visited.insert(st).second) {
        // Already on the walk stack; its members are checked there.
        return true;
    }
    if (st->m_parent
            && !gpu_struct_members_ok(st->m_parent, visited, caps)) {
        return false;
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *msym = st->m_symtab->get_symbol(st->m_members[i]);
        if (!msym) continue;
        msym = ASRUtils::symbol_get_past_external(msym);
        if (!ASR::is_a<ASR::Variable_t>(*msym)) continue;
        ASR::Variable_t *mvar = ASR::down_cast<ASR::Variable_t>(msym);
        ASR::ttype_t *mtype = ASRUtils::extract_type(mvar->m_type);
        if (ASR::is_a<ASR::StructType_t>(*mtype)) {
            if (!mvar->m_type_declaration
                    || !gpu_struct_members_ok(
                        mvar->m_type_declaration, visited, caps)) {
                return false;
            }
        } else if (!caps.has_scalar_type(mtype)) {
            return false;
        }
    }
    return true;
}

bool gpu_device_can_represent_type(const GpuDeviceCapabilities &caps,
        ASR::ttype_t *t, ASR::expr_t *e) {
    ASR::ttype_t *base_t = ASRUtils::extract_type(t);
    if (ASR::is_a<ASR::StructType_t>(*base_t)) {
        if (!e) return false;
        std::set<ASR::Struct_t*> visited;
        return gpu_struct_members_ok(
            ASRUtils::get_struct_sym_from_struct_expr(e), visited, caps);
    }
    return caps.has_scalar_type(base_t);
}

ASR::ttype_t* scalar_type_of(ASR::ttype_t *t) {
    if (!t) return nullptr;
    ASR::ttype_t *base_t = ASRUtils::extract_type(t);
    if (ASR::is_a<ASR::StructType_t>(*base_t)) return nullptr;
    return base_t;
}

GpuDeclineReason unsupported_on_device(const ASR::stmt_t &s) {
    switch (s.type) {
        case ASR::stmtType::Print:
        case ASR::stmtType::FileWrite:
        case ASR::stmtType::FileRead:
        case ASR::stmtType::FileOpen:
        case ASR::stmtType::FileClose:
        case ASR::stmtType::FileInquire:
        case ASR::stmtType::FileBackspace:
        case ASR::stmtType::FileRewind:
        case ASR::stmtType::FileEndfile:
        case ASR::stmtType::Flush:
            return GpuDeclineReason::StatementIo;
        case ASR::stmtType::Stop:
        case ASR::stmtType::ErrorStop:
            return GpuDeclineReason::StatementStop;
        default:
            return GpuDeclineReason::None;
    }
}

} // namespace LCompilers
