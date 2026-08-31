#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/pass/replace_gpu_offload.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/stmt_walk_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/string_utils.h>
#include <libasr/codegen/gpu_utils.h>

#include <filesystem>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

// `--gpu-offload-report`. Every `do concurrent` that does not end up as a
// GPU kernel of its own is reported on stderr with the source position, the
// enclosing procedure and a `reason=` tag. A loop that device code has to
// run in order is reported too, under its own reason, so a loop running
// serially *on the device* can be told apart from one left on the host.
//
// The state is per-run and set once by the visitor's constructor, so every
// report site costs a single bool test while the report is off.
namespace {

struct GpuOffloadReport {
    static bool enabled;
    static const LocationManager *lm;
    // Extra `key=value` fields for the next report, filled in by the
    // analysis that found the obstruction.
    static std::string detail;

    static void configure(const PassOptions &po) {
        enabled = po.gpu_offload_report;
        lm = po.loc_manager;
        detail.clear();
    }

    static void set_detail(const std::string &d) {
        if (enabled) detail = d;
    }

    static void clear_detail() {
        detail.clear();
    }

    // `on_device` distinguishes a loop the device runs serially from one
    // that falls back to the host altogether.
    static void emit(const Location &loc, const std::string &proc,
            const std::string &reason, bool on_device = false) {
        if (!enabled) return;
        std::string where;
        if (lm != nullptr && !lm->files.empty()) {
            uint32_t line = 0, col = 0;
            std::string filename;
            lm->pos_to_linecol(lm->output_to_input_pos(loc.first, false),
                line, col, filename);
            where = filename + ":" + std::to_string(line) + ":"
                + std::to_string(col);
        } else {
            where = "<offset " + std::to_string(loc.first) + ">";
        }
        std::cerr << "gpu-offload-report: " << where << ": do concurrent"
                  << " status=" << (on_device ? "device-serial" : "host")
                  << " proc=" << proc << " reason=" << reason;
        if (!detail.empty()) {
            std::cerr << " " << detail;
        }
        std::cerr << std::endl;
        detail.clear();
    }
};

bool GpuOffloadReport::enabled = false;
const LocationManager *GpuOffloadReport::lm = nullptr;
std::string GpuOffloadReport::detail;

// `inline_device_function_calls` splices a device callee into the loop
// body by replacing whole statement vectors -- the loop's own and that of
// every BLOCK it recurses into -- and never edits a statement in place.
// It only ever adds symbols to the enclosing scope, one Block per splice.
// Recording those vectors and that scope's symbol names therefore makes
// the splice exactly reversible. That is what lets the workspace
// pre-flight run on the spliced shape, where a callee's locals have
// become kernel workspaces, and still leave the loop exactly as it was
// found when the offload is declined.
//
// Dropping the spliced-in Blocks again matters as much as restoring the
// statements: an orphaned Block is still a symbol of the enclosing scope,
// so the pass would walk into it on its next round, offload the loops it
// holds, splice once more and never reach a fixed point.
// The type a scope-local array carried before the pass replaced it, so
// that a declined offload leaves the variable exactly as it found it.
struct ScopeArrayDims {
    ASR::Variable_t *var;
    ASR::ttype_t *type;
};

class GpuLoopBodySnapshot {
public:
    void record(ASR::DoConcurrentLoop_t &loop, SymbolTable *scope) {
        loop_ = &loop;
        loop_body_ = loop.m_body;
        loop_n_body_ = loop.n_body;
        scope_ = scope;
        if (scope_ != nullptr) {
            for (auto &item : scope_->get_scope()) {
                scope_symbols_.insert(item.first);
            }
        }
        record_blocks(loop.m_body, loop.n_body);
    }

    void restore() {
        if (loop_ == nullptr) return;
        loop_->m_body = loop_body_;
        loop_->n_body = loop_n_body_;
        for (auto &saved : blocks_) {
            saved.block->m_body = saved.body;
            saved.block->n_body = saved.n_body;
        }
        if (scope_ == nullptr) return;
        std::vector<std::string> added;
        for (auto &item : scope_->get_scope()) {
            if (scope_symbols_.count(item.first) == 0) {
                added.push_back(item.first);
            }
        }
        for (auto &name : added) {
            scope_->erase_symbol(name);
        }
    }

private:
    struct SavedBlock {
        ASR::Block_t *block;
        ASR::stmt_t **body;
        size_t n_body;
    };

    void record_blocks(ASR::stmt_t **body, size_t n_body) {
        for (size_t i = 0; i < n_body; i++) {
            if (!ASR::is_a<ASR::BlockCall_t>(*body[i])) continue;
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(body[i])->m_m);
            if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
            blocks_.push_back({block, block->m_body, block->n_body});
            record_blocks(block->m_body, block->n_body);
        }
    }

    ASR::DoConcurrentLoop_t *loop_ = nullptr;
    ASR::stmt_t **loop_body_ = nullptr;
    size_t loop_n_body_ = 0;
    std::vector<SavedBlock> blocks_;
    SymbolTable *scope_ = nullptr;
    std::set<std::string> scope_symbols_;
};

} // anonymous namespace

static int gpu_kernel_counter = 0;

// Look up a member (component or type-bound procedure) by name in a
// Struct's symbol table, walking the inheritance chain: a component
// inherited from a parent type lives in the parent Struct's symtab, not
// in the extending type's own scope.
// Fill in the `start`/`end` bounds of a synthesized `do` loop head for
// dimension `d` of `arr_expr`. Descriptor arrays (allocatables, pointers,
// assumed-shape dummies and the temporaries created for array-valued
// `associate` selectors) carry no compile-time `dimension_t` entries, so
// their `m_start`/`m_length` are null; fall back to the runtime bounds in
// that case instead of emitting a loop head with null bounds.
static std::pair<ASR::expr_t*, ASR::expr_t*> get_dim_bounds(Allocator &al,
        const Location &loc, ASR::dimension_t *dims, size_t d,
        ASR::expr_t *arr_expr) {
    if (dims && dims[d].m_start && dims[d].m_length) {
        return {dims[d].m_start, dims[d].m_length};
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t *dim_expr = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, (int64_t)d + 1, int_type,
            ASR::integerbozType::Decimal));
    return {ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arr_expr,
                dim_expr, int_type, ASR::arrayboundType::LBound, nullptr)),
            ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arr_expr,
                dim_expr, int_type, ASR::arrayboundType::UBound, nullptr))};
}

static void set_loop_head_bounds(Allocator &al, const Location &loc,
        ASR::do_loop_head_t &head, ASR::dimension_t *dims, size_t d,
        ASR::expr_t *arr_expr) {
    std::pair<ASR::expr_t*, ASR::expr_t*> bounds =
        get_dim_bounds(al, loc, dims, d, arr_expr);
    head.m_start = bounds.first;
    head.m_end = bounds.second;
}

static ASR::symbol_t* get_struct_member_recursive(ASR::Struct_t *s,
        const std::string &name) {
    std::set<ASR::Struct_t*> seen;
    while (s != nullptr) {
        if (!seen.insert(s).second) break;
        ASR::symbol_t *member = s->m_symtab->get_symbol(name);
        if (member) return member;
        if (!s->m_parent) break;
        ASR::symbol_t *parent = ASRUtils::symbol_get_past_external(
            s->m_parent);
        if (!ASR::is_a<ASR::Struct_t>(*parent)) break;
        s = ASR::down_cast<ASR::Struct_t>(parent);
    }
    return nullptr;
}

// Name of the Struct that owns `member`, used as the m_module_name of an
// ExternalSymbol pointing at it. For an inherited member this is the
// ancestor type, not the type the reference was written through.
static std::string struct_member_owner_name(ASR::symbol_t *member,
        const std::string &fallback) {
    SymbolTable *owner_st = ASRUtils::symbol_parent_symtab(member);
    if (owner_st && owner_st->asr_owner &&
            owner_st->asr_owner->type == ASR::asrType::symbol) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            owner_st->asr_owner);
        if (ASR::is_a<ASR::Struct_t>(*owner)) {
            return std::string(ASRUtils::symbol_name(owner));
        }
    }
    return fallback;
}

// Collects all symbols referenced in expressions/statements
class GpuSymbolCollector : public ASR::BaseWalkVisitor<GpuSymbolCollector> {
public:
    Allocator &al;
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &symbols;
    std::set<SymbolTable*> enclosing_scopes;

    GpuSymbolCollector(Allocator &al_,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &syms,
        const std::set<SymbolTable*> &scopes = {})
        : al(al_), symbols(syms), enclosing_scopes(scopes) {}

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        // Walk variable types in the block's symbol table to collect
        // referenced symbols (e.g., VLA dimension expressions like n(i))
        for (auto &item : block->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start) {
                            visit_expr(*arr->m_dims[d].m_start);
                        }
                        if (arr->m_dims[d].m_length) {
                            visit_expr(*arr->m_dims[d].m_length);
                        }
                    }
                }
            }
        }
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        // A variable owned by a Block or AssociateBlock scope that is
        // nested *inside* the loop travels with that scope into the
        // kernel, so it must not become a kernel parameter. A variable
        // owned by a Block or AssociateBlock that *encloses* the loop is
        // left behind when the loop is extracted, so it does have to be
        // passed in. `enclosing_scopes` holds exactly the latter.
        if (ASR::is_a<ASR::Variable_t>(*x.m_v)) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(x.m_v);
            if (var->m_parent_symtab->asr_owner &&
                var->m_parent_symtab->asr_owner->type
                    == ASR::asrType::symbol) {
                ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                    var->m_parent_symtab->asr_owner);
                if ((ASR::is_a<ASR::Block_t>(*owner) ||
                     ASR::is_a<ASR::AssociateBlock_t>(*owner)) &&
                    enclosing_scopes.find(var->m_parent_symtab)
                        == enclosing_scopes.end()) {
                    return;
                }
            }
        }
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (symbols.find(name) == symbols.end()) {
            symbols[name] = {ASRUtils::symbol_type(x.m_v),
                ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, x.m_v))};
        }
    }
};

// Answers whether the Metal Shading Language can represent `t` with the
// same in-memory width the host uses. MSL has no 64-bit floating point
// type (`float`/`half`/`bfloat` only), no 64-bit boolean, and no complex
// type, so the Metal backend lowers all of those to a narrower (or bogus)
// type. Offloading a `do concurrent` that touches such data would make the
// kernel reinterpret the host buffers and the by-value scalar-argument
// struct at the wrong element size, silently producing wrong results, so
// such a loop has to stay on the CPU.
//
// Note: kind-8 integers are representable (MSL `long` is 8 bytes), but are
// rejected here to preserve the pre-existing bail-out behaviour.
static bool is_metal_representable_scalar_type(ASR::ttype_t *base_t) {
    switch (base_t->type) {
        case ASR::ttypeType::Real:
            return ASR::down_cast<ASR::Real_t>(base_t)->m_kind != 8;
        case ASR::ttypeType::Integer:
            return ASR::down_cast<ASR::Integer_t>(base_t)->m_kind != 8;
        case ASR::ttypeType::Logical:
            return ASR::down_cast<ASR::Logical_t>(base_t)->m_kind != 8;
        case ASR::ttypeType::Complex:
            return false;
        default:
            return true;
    }
}

// A derived type is representable only when every one of its data members
// is, because the Metal struct is laid out member by member: a single fp64
// member anywhere in the type changes the element size the kernel would
// have to stride by, while the host buffer keeps the wider layout. The
// members inherited through `extends` live in the parent Struct (they are
// reached at run time through the `__parent` member), so the parent chain
// has to be walked as well. `visited` guards against self-referential
// types such as `type(node), pointer :: next`, whose member graph is
// cyclic.
static bool is_metal_representable_struct(ASR::symbol_t *struct_sym,
        std::set<ASR::Struct_t*> &visited) {
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
            && !is_metal_representable_struct(st->m_parent, visited)) {
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
                    || !is_metal_representable_struct(
                        mvar->m_type_declaration, visited)) {
                return false;
            }
        } else if (!is_metal_representable_scalar_type(mtype)) {
            return false;
        }
    }
    return true;
}

// Answers whether the Metal Shading Language can represent the type of `e`
// with the same in-memory width the host uses. MSL has no 64-bit floating
// point type (`float`/`half`/`bfloat` only), no 64-bit boolean, and no
// complex type, so the Metal backend lowers all of those to a narrower (or
// bogus) type. Offloading a `do concurrent` that touches such data would
// make the kernel reinterpret the host buffers and the by-value
// scalar-argument struct at the wrong element size, silently producing
// wrong results, so such a loop has to stay on the CPU.
static bool is_metal_representable_type(ASR::ttype_t *t, ASR::expr_t *e) {
    ASR::ttype_t *base_t = ASRUtils::extract_type(t);
    if (ASR::is_a<ASR::StructType_t>(*base_t)) {
        if (!e) return false;
        std::set<ASR::Struct_t*> visited;
        return is_metal_representable_struct(
            ASRUtils::get_struct_sym_from_struct_expr(e), visited);
    }
    return is_metal_representable_scalar_type(base_t);
}

// A variable declared inside the `do concurrent` body by a BLOCK or an
// ASSOCIATE construct is carried into the generated kernel as a
// kernel-local declaration. The Metal Shading Language has no
// variable-length arrays and no heap, so a local whose extent is not a
// compile-time constant can only be emitted when the GPU workspace
// machinery (`analyze_gpu_vla_workspaces`) can bind it to a per-thread
// slice of a device buffer. That needs an extent expression: either the
// array declares one itself (an automatic array `real :: t(m)`) or an
// ALLOCATE in the same construct supplies one.
//
// What is left over has no extent anywhere: a descriptor local -- the
// temporary the frontend materialises for an array-valued ASSOCIATE
// selector whose operand is an allocatable or an assumed-shape dummy, or
// an allocatable never allocated in this construct -- carries deferred
// `dimension_t` with null lengths, so the kernel would declare it with a
// bogus one-element extent and index out of bounds. Such a loop has to
// stay on the CPU.
class GpuLocalArrayChecker :
        public ASR::BaseWalkVisitor<GpuLocalArrayChecker> {
public:
    bool has_unsized_local_array = false;
    // Name of the first offending local, for --gpu-offload-report.
    std::string unsized_name;

    // True when every extent of the array is present as an expression,
    // so a workspace buffer can be sized from it.
    static bool dims_have_lengths(ASR::dimension_t *dims, size_t n) {
        if (n == 0) return false;
        for (size_t d = 0; d < n; d++) {
            if (!dims[d].m_length) return false;
        }
        return true;
    }

    void check_scope(SymbolTable *symtab, ASR::stmt_t **body,
            size_t n_body) {
        if (!symtab) return;
        for (auto &item : symtab->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            ASR::ttype_t *t =
                ASRUtils::type_get_past_allocatable_pointer(var->m_type);
            ASR::dimension_t *dims = nullptr;
            int rank = ASRUtils::extract_dimensions_from_ttype(t, dims);
            if (rank <= 0) continue;
            if (ASRUtils::is_fixed_size_array(dims, rank)) continue;
            if (dims_have_lengths(dims, (size_t)rank)) continue;
            if (ASRUtils::is_allocatable(var->m_type)) {
                ASR::alloc_arg_t *aa = find_alloc_arg_for_var(body, n_body,
                    std::string(var->m_name));
                if (aa && dims_have_lengths(aa->m_dims, aa->n_dims)) {
                    continue;
                }
                // No shape anywhere, but the expression assigned to it
                // has one: size_scope_array_temporaries() writes that
                // shape into the temporary's own type before the kernel
                // is built, and the workspace pre-flight then decides
                // whether the host can evaluate it.
                if (gpu_scope_array_shape_source(var, body, n_body)) {
                    continue;
                }
            }
            has_unsized_local_array = true;
            if (unsized_name.empty()) unsized_name = item.first;
        }
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        check_scope(blk->m_symtab, blk->m_body, blk->n_body);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        check_scope(blk->m_symtab, blk->m_body, blk->n_body);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }
};

// Checks whether an expression tree contains a FunctionCall node.
class ContainsFunctionCall : public ASR::BaseWalkVisitor<ContainsFunctionCall> {
public:
    bool found = false;
    void visit_FunctionCall(const ASR::FunctionCall_t &) { found = true; }
};

static bool expr_has_function_call(ASR::expr_t *expr) {
    if (!expr) return false;
    ContainsFunctionCall checker;
    checker.visit_expr(*expr);
    return checker.found;
}

// `size(f(...))`: the extent of an array expression that is not a
// designator.  Evaluating it on the host would mean calling `f` there,
// with whatever the arguments happen to be outside the loop -- the loop
// index has no value there at all.  The shape is already recorded in the
// expression's own type, though: a result declared `real :: r(n)` carries
// `n` as the length of its one dimension, written in the symbols of the
// scope the call is made from.  Rewrite the ArraySize to that length, so
// the host evaluates the extent without evaluating the call.  This is the
// same shape `build_gpu_array_extent_node` reads when it decides the
// extent is resolvable, so emitter and pre-flight agree.  Returns nullptr
// when the type does not record the length, leaving the node alone.
static ASR::expr_t *gpu_array_size_from_type(Allocator &al,
        const ASR::ArraySize_t *sz) {
    if (!sz->m_v || ASR::is_a<ASR::Var_t>(*sz->m_v)) return nullptr;
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(sz->m_v));
    if (!t || !ASR::is_a<ASR::Array_t>(*t)) return nullptr;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    if (arr->n_dims == 0) return nullptr;
    size_t begin = 0, end = arr->n_dims;
    if (sz->m_dim) {
        int64_t d;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(sz->m_dim), d)) {
            return nullptr;
        }
        if (d < 1 || (size_t)d > arr->n_dims) return nullptr;
        begin = (size_t)d - 1;
        end = begin + 1;
    }
    ASR::ttype_t *int_t = ASRUtils::TYPE(ASR::make_Integer_t(al,
        sz->base.base.loc, 4));
    ASR::expr_t *acc = nullptr;
    for (size_t d = begin; d < end; d++) {
        ASR::expr_t *len = arr->m_dims[d].m_length;
        if (!len) return nullptr;
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.success = true;
        ASR::expr_t *one = dup.duplicate_expr(len);
        if (!one || !dup.success) return nullptr;
        acc = acc ? ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
            sz->base.base.loc, acc, ASR::binopType::Mul, one, int_t,
            nullptr)) : one;
    }
    return acc;
}

// Rewrites every `size(<non-designator>)` in an expression tree to the
// extent recorded in that expression's own type.
class GpuArraySizeFromTypeReplacer :
        public ASR::BaseExprReplacer<GpuArraySizeFromTypeReplacer> {
public:
    Allocator &al;
    GpuArraySizeFromTypeReplacer(Allocator &al_) : al(al_) {}
    void replace_ArraySize(ASR::ArraySize_t *x) {
        ASR::BaseExprReplacer<GpuArraySizeFromTypeReplacer>
            ::replace_ArraySize(x);
        ASR::expr_t *rep = gpu_array_size_from_type(al, x);
        if (rep) *current_expr = rep;
    }
};

static ASR::expr_t *gpu_simplify_array_sizes(Allocator &al,
        ASR::expr_t *expr) {
    if (!expr) return expr;
    GpuArraySizeFromTypeReplacer r(al);
    ASR::expr_t *root = expr;
    r.current_expr = &root;
    r.replace_expr(root);
    return root;
}

// Collects the names every Var in an expression tree refers to.
class GpuVarNameCollector : public ASR::BaseWalkVisitor<GpuVarNameCollector> {
public:
    std::set<std::string> names;
    void visit_Var(const ASR::Var_t &x) {
        names.insert(ASRUtils::symbol_name(x.m_v));
    }
};

// A host-side expression may not name a loop index: the host evaluates it
// before the launch, where the index has no value.  The workspace
// pre-flight already declines such a loop, so reaching here means the
// emitter and the pre-flight disagree -- a compiler bug, which must be a
// clean failure rather than a plausible-but-wrong number read out of an
// undefined variable.
static void gpu_check_host_expr_index_free(ASR::expr_t *host_expr,
        const std::set<std::string> &index_names, const std::string &what) {
    if (!host_expr || index_names.empty()) return;
    GpuVarNameCollector c;
    c.visit_expr(*host_expr);
    for (const std::string &n : c.names) {
        if (index_names.count(n)) {
            throw LCompilersException(
                "GPU offload: the host expression for " + what +
                " refers to the loop index '" + n + "', which has no "
                "value outside the loop");
        }
    }
}

// Replaces all Var references in-place to point to the kernel scope symbols
class GpuReplaceSymbols : public ASR::BaseExprReplacer<GpuReplaceSymbols> {
public:
    SymbolTable &kernel_scope;
    std::set<SymbolTable*> skip_scopes;
    GpuReplaceSymbols(SymbolTable &scope) : kernel_scope(scope) {}

    void replace_Var(ASR::Var_t *x) {
        std::string name = ASRUtils::symbol_name(x->m_v);
        for (auto *ss : skip_scopes) {
            if (ss->get_symbol(name)) return;
        }
        ASR::symbol_t *new_sym = kernel_scope.get_symbol(name);
        if (new_sym) {
            x->m_v = new_sym;
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        // Replace the struct variable expression (e.g., Var x)
        ASR::expr_t **current_expr_copy = current_expr;
        current_expr = &(x->m_v);
        replace_expr(x->m_v);
        current_expr = current_expr_copy;
        // Replace the member symbol to point to kernel scope's ExternalSymbol
        std::string mem_name = ASRUtils::symbol_name(x->m_m);
        ASR::symbol_t *new_mem = kernel_scope.get_symbol(mem_name);
        if (new_mem) {
            x->m_m = new_mem;
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        // Remap m_name to kernel scope symbol
        std::string name = ASRUtils::symbol_name(x->m_name);
        ASR::symbol_t *new_sym = kernel_scope.get_symbol(name);
        if (!new_sym && ASR::is_a<ASR::ExternalSymbol_t>(*x->m_name)) {
            // Try sanitized ExternalSymbol name (handles disambiguated
            // functions where different modules define same-named functions)
            std::string sanitized = name;
            for (char &c : sanitized) {
                if (c == '~' || c == '@') c = '_';
            }
            new_sym = kernel_scope.get_symbol(sanitized);
            if (!new_sym) {
                // ExternalSymbol name differs from resolved function name;
                // try the underlying function's name (e.g., "construct"
                // instead of "~mytype_t@construct").
                std::string resolved_name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(x->m_name));
                new_sym = kernel_scope.get_symbol(resolved_name);
            }
        }
        if (new_sym) {
            x->m_name = new_sym;
        }
        if (x->m_original_name) {
            std::string orig_name = ASRUtils::symbol_name(x->m_original_name);
            ASR::symbol_t *new_orig = kernel_scope.get_symbol(orig_name);
            if (new_orig) {
                x->m_original_name = new_orig;
            }
        }
        // Call base to handle arguments, type, value, dt
        ASR::BaseExprReplacer<GpuReplaceSymbols>::replace_FunctionCall(x);
    }
};

class GpuReplaceSymbolsVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<GpuReplaceSymbolsVisitor> {
public:
    GpuReplaceSymbols replacer;
    GpuReplaceSymbolsVisitor(SymbolTable &scope) : replacer(scope) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Resolves associate variable references to their original targets.
// When a DoConcurrentLoop is inside an AssociateBlock, variables like `nn`
// (associated with `n`) must be resolved to their associate value before
// kernel extraction, because the kernel scope cannot access the
// AssociateBlock's symbol table. The mapped expression may be a simple
// Var (e.g., associate(nn => n)) or a complex expression such as
// ArrayPhysicalCast(StructInstanceMember(...)) for derived-type components.
class AssociateVarResolver : public ASR::BaseExprReplacer<AssociateVarResolver> {
public:
    Allocator &al;
    std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map;
    AssociateVarResolver(Allocator &al_,
                         std::map<ASR::symbol_t*, ASR::expr_t*> &map)
        : al(al_), assoc_map(map) {}

    void replace_Var(ASR::Var_t *x) {
        auto it = assoc_map.find(x->m_v);
        if (it != assoc_map.end()) {
            // Deep-copy so the original Associate expression is not
            // modified when GpuReplaceSymbolsVisitor remaps symbols later
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            ASR::expr_t *copy = dup.duplicate_expr(it->second);
            if (copy) {
                *current_expr = copy;
            }
        }
    }
};

class AssociateVarResolverVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<AssociateVarResolverVisitor> {
public:
    AssociateVarResolver replacer;
    AssociateVarResolverVisitor(Allocator &al,
                                std::map<ASR::symbol_t*, ASR::expr_t*> &map)
        : replacer(al, map) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// A workspace extent written as `size(a(i)%m, d)` -- the extent of an
// allocatable array component reached through a subscript into an array
// of derived types -- can be reproduced on neither side of the launch as
// it stands.  The kernel receives such a component as one flat data
// buffer plus a per-element total size, never the component's individual
// extents, and the host resolver cannot walk a subscript in the middle of
// a component path.  The value is an ordinary integer the host can
// compute before the launch, though, so it becomes one more scalar kernel
// argument: this replacer rewrites every such `size(...)` in the kernel
// body to a Var naming that argument and records the matching host-scope
// expression for the caller to pass as the actual.  Host workspace sizing
// and shader stride then read one and the same scalar and cannot
// disagree.
class GpuStructArrayMemberExtent :
        public ASR::BaseExprReplacer<GpuStructArrayMemberExtent> {
public:
    Allocator &al;
    SymbolTable *orig_scope;
    SymbolTable *kernel_scope;
    const std::vector<std::string> &arg_names;
    // Kernel parameter and the host expression that supplies its value,
    // in the order the parameters were created.
    std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>> &added;
    std::map<std::string, ASR::symbol_t*> by_key;

    GpuStructArrayMemberExtent(Allocator &al_, SymbolTable *orig_scope_,
            SymbolTable *kernel_scope_,
            const std::vector<std::string> &arg_names_,
            std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>> &added_)
        : al(al_), orig_scope(orig_scope_), kernel_scope(kernel_scope_),
          arg_names(arg_names_), added(added_) {}

    // The component symbol as the host scope names it.  The kernel body
    // may reach a component through an ExternalSymbol that only the
    // kernel scope holds, which the host expression must not reference.
    ASR::symbol_t* host_member_ref(ASR::symbol_t *m) {
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(m);
        for (auto &item : orig_scope->get_scope()) {
            if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
            if (ASRUtils::symbol_get_past_external(item.second) == target) {
                return item.second;
            }
        }
        return m;
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        if (rewrite_array_size(x)) return;
        ASR::BaseExprReplacer<GpuStructArrayMemberExtent>
            ::replace_ArraySize(x);
    }

    bool rewrite_array_size(ASR::ArraySize_t *x) {
        GpuStructArrayMemberExtentRef ref;
        if (!match_gpu_struct_array_member_extent(
                ASRUtils::EXPR((ASR::asr_t*)x), arg_names, ref)) {
            return false;
        }
        ASR::ArrayItem_t *ai = ref.item;
        ASR::StructInstanceMember_t *sim = ref.member;
        int64_t d = ref.dim;
        std::string arr_name = ASRUtils::symbol_name(ref.base->m_v);
        ASR::symbol_t *host_arr = orig_scope->resolve_symbol(arr_name);
        if (!host_arr) return false;
        ASR::ttype_t *size_type = ASRUtils::extract_type(x->m_type);
        const Location &loc = x->base.base.loc;

        Vec<ASR::array_index_t> host_idx;
        host_idx.reserve(al, ai ? ai->n_args : 0);
        std::string key = arr_name;
        for (size_t k = 0; ai != nullptr && k < ai->n_args; k++) {
            ASR::array_index_t &ix = ai->m_args[k];
            ASR::expr_t *host_sub = nullptr;
            std::string tag;
            int64_t sub_val;
            if (try_eval_int_constant(ix.m_right, sub_val)) {
                host_sub = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                    loc, sub_val, ASRUtils::expr_type(ix.m_right),
                    ASR::integerbozType::Decimal));
                tag = sub_val < 0 ? "m" + std::to_string(-sub_val)
                                  : std::to_string(sub_val);
            } else {
                std::string sub_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(ix.m_right)->m_v);
                ASR::symbol_t *host_sub_sym =
                    orig_scope->resolve_symbol(sub_name);
                if (!host_sub_sym) return false;
                host_sub = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    host_sub_sym));
                tag = sub_name;
            }
            ASR::array_index_t host_ix;
            host_ix.loc = ix.loc;
            host_ix.m_left = nullptr;
            host_ix.m_step = nullptr;
            host_ix.m_right = host_sub;
            host_idx.push_back(al, host_ix);
            key += "_" + tag;
        }
        std::string mem_name = ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(sim->m_m));
        key += "_" + mem_name + "_" + std::to_string(d);

        ASR::symbol_t *sym = nullptr;
        auto it = by_key.find(key);
        if (it != by_key.end()) {
            sym = it->second;
        } else {
            std::string param_name = kernel_scope->get_unique_name(
                "__memdim_" + key);
            sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, param_name), nullptr, 0,
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, size_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            kernel_scope->add_symbol(param_name, sym);
            ASR::expr_t *host_item = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, host_arr));
            if (ai != nullptr) {
                host_item = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                    host_item, host_idx.p, host_idx.n, ai->m_type,
                    ai->m_storage_format, nullptr));
            }
            ASR::expr_t *host_member = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc, host_item,
                    host_member_ref(sim->m_m), sim->m_type, nullptr));
            ASR::expr_t *host_dim = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, d, size_type,
                    ASR::integerbozType::Decimal));
            ASR::expr_t *host_size = ASRUtils::EXPR(ASR::make_ArraySize_t(
                al, loc, host_member, host_dim, size_type, nullptr));
            added.push_back({sym, host_size});
            by_key[key] = sym;
        }
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        return true;
    }
};

class GpuStructArrayMemberExtentVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<
            GpuStructArrayMemberExtentVisitor> {
public:
    GpuStructArrayMemberExtent replacer;

    GpuStructArrayMemberExtentVisitor(Allocator &al,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const std::vector<std::string> &arg_names,
            std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>> &added)
        : replacer(al, orig_scope, kernel_scope, arg_names, added) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    // A BLOCK is where the workspace temporaries live, and the generated
    // visitor does not descend into one on its own.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// A section whose base is itself a section -- what splicing a device
// function leaves behind when its assumed-shape dummy was already
// referenced through a section and the actual is a section too.  The
// Metal emitter renders a section's base with the ordinary expression
// emitter, which has no address to give for an `ArraySection`, so a
// kernel built from such a body compiles to a shader that does not
// build.  The offload declines instead.
class GpuNestedSectionFinder :
        public ASR::BaseWalkVisitor<GpuNestedSectionFinder> {
public:
    bool found = false;

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(x.m_v);
        if (base != nullptr && ASR::is_a<ASR::ArraySection_t>(*base)) {
            found = true;
        }
        ASR::BaseWalkVisitor<GpuNestedSectionFinder>::visit_ArraySection(x);
    }

    // The generated walker stops at a BLOCK or ASSOCIATE call, and both
    // are where a spliced device function body ends up.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// True when a statement list takes a section of `dummy` -- `c(1:k)` for a
// dummy `c`.  Splicing the callee substitutes the actual argument for the
// dummy, so such a section over a sectioned actual becomes a section of a
// section, which no device pointer can express.
class GpuDummySectionFinder :
        public ASR::BaseWalkVisitor<GpuDummySectionFinder> {
public:
    ASR::symbol_t *dummy;
    bool found = false;

    GpuDummySectionFinder(ASR::symbol_t *dummy_) : dummy(dummy_) {}

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(x.m_v);
        if (base != nullptr && ASR::is_a<ASR::Var_t>(*base) &&
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(base)->m_v) == dummy) {
            found = true;
        }
        ASR::BaseWalkVisitor<GpuDummySectionFinder>::visit_ArraySection(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// True when a statement list holds an implied-do. The array-constructor
// lowering leaves one standing as a whole element of the constructor, and
// the Metal code generator has no rendering for it, so a kernel built from
// such a body reaches the driver as a shader that will not compile.
class GpuImpliedDoFinder :
        public ASR::BaseWalkVisitor<GpuImpliedDoFinder> {
public:
    bool found = false;

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &x) {
        found = true;
        ASR::BaseWalkVisitor<GpuImpliedDoFinder>::visit_ImpliedDoLoop(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// Counts how many times a given symbol is written to within a list of
// statements. Used to distinguish a genuine ASSOCIATE selector temporary
// (written exactly once, at its point of definition) from an ordinary
// block-local variable such as the counter that array-constructor
// lowering introduces, which is written repeatedly and therefore must
// not be folded into a constant.
class AssignmentTargetCounter :
    public ASR::BaseWalkVisitor<AssignmentTargetCounter> {
public:
    ASR::symbol_t *target;
    size_t count;

    AssignmentTargetCounter(ASR::symbol_t *target_)
        : target(target_), count(0) {}

    void count_var(ASR::expr_t *e) {
        if (e && ASR::is_a<ASR::Var_t>(*e) &&
                ASR::down_cast<ASR::Var_t>(e)->m_v == target) {
            count++;
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        count_var(x.m_target);
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        count_var(x.m_target);
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_Associate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        count_var(x.m_head.m_v);
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) {
            count_var(x.m_head[i].m_v);
        }
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_DoConcurrentLoop(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        // Conservatively treat any appearance as an argument as a write
        for (size_t i = 0; i < x.n_args; i++) {
            count_var(x.m_args[i].m_value);
        }
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_SubroutineCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *block =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// True if `sym` is written exactly once across `body` — the shape of a
// single-assignment ASSOCIATE selector binding, which may safely be
// folded into its value at every use site.
static bool is_single_assignment_binding(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body) {
    AssignmentTargetCounter counter(sym);
    for (size_t i = 0; i < n_body; i++) {
        counter.visit_stmt(*body[i]);
    }
    return counter.count == 1;
}

// Collects local variables used in do concurrent body that are NOT
// arrays and NOT the loop variables — these are per-thread temporaries
class GpuLocalVarCollector : public ASR::BaseWalkVisitor<GpuLocalVarCollector> {
public:
    std::set<std::string> &local_vars;
    std::set<std::string> &assigned_vars;
    std::set<SymbolTable*> enclosing_scopes;

    GpuLocalVarCollector(std::set<std::string> &lv, std::set<std::string> &av,
        const std::set<SymbolTable*> &scopes = {})
        : local_vars(lv), assigned_vars(av), enclosing_scopes(scopes) {}

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        // Check if target is a simple Var (not ArrayItem)
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_target);
            // Skip variables local to Block or AssociateBlock scopes
            bool is_block_local = false;
            if (ASR::is_a<ASR::Variable_t>(*v->m_v)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v->m_v);
                if (var->m_parent_symtab->asr_owner) {
                    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                        var->m_parent_symtab->asr_owner);
                    if (ASR::is_a<ASR::Block_t>(*owner) ||
                        ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
                        if (enclosing_scopes.find(var->m_parent_symtab)
                                == enclosing_scopes.end()) {
                            is_block_local = true;
                        }
                    }
                }
            }
            if (!is_block_local) {
                std::string name = ASRUtils::symbol_name(v->m_v);
                ASR::ttype_t *type = ASRUtils::symbol_type(v->m_v);
                if (!ASRUtils::is_array(type)) {
                    assigned_vars.insert(name);
                }
            }
        }
        // Check if target is a StructInstanceMember (e.g., x%v = ...)
        if (ASR::is_a<ASR::StructInstanceMember_t>(*x.m_target)) {
            ASR::StructInstanceMember_t *sm =
                ASR::down_cast<ASR::StructInstanceMember_t>(x.m_target);
            if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(sm->m_v);
                std::string name = ASRUtils::symbol_name(v->m_v);
                assigned_vars.insert(name);
            }
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_Assignment(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        // DoLoop loop variables are local temporaries
        if (x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_head.m_v);
            std::string name = ASRUtils::symbol_name(v->m_v);
            assigned_vars.insert(name);
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_DoLoop(x);
    }
};

// Collects all Function symbols referenced by FunctionCall/SubroutineCall
// nodes in the do concurrent body so they can be imported into the kernel.
class GpuFunctionCollector : public ASR::BaseWalkVisitor<GpuFunctionCollector> {
public:
    std::map<std::string, ASR::symbol_t*> functions;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*resolved) ||
                ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved)) {
            std::string name = ASRUtils::symbol_name(x.m_name);
            if (functions.find(name) == functions.end()) {
                functions[name] = x.m_name;
            }
        }
        if (x.m_original_name) {
            std::string orig_name = ASRUtils::symbol_name(x.m_original_name);
            if (functions.find(orig_name) == functions.end()) {
                functions[orig_name] = x.m_original_name;
            }
        }
        ASR::BaseWalkVisitor<GpuFunctionCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*resolved) ||
                ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved)) {
            std::string name = ASRUtils::symbol_name(x.m_name);
            if (functions.find(name) == functions.end()) {
                functions[name] = x.m_name;
            }
        }
        ASR::BaseWalkVisitor<GpuFunctionCollector>::visit_SubroutineCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }
};

// Collects every DoConcurrentLoop reached from the statements walked,
// descending into BLOCK and ASSOCIATE scopes so a loop nested there is
// seen too.
class GpuDoConcurrentCollector :
        public ASR::BaseWalkVisitor<GpuDoConcurrentCollector> {
public:
    std::set<const ASR::DoConcurrentLoop_t*> loops;

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        loops.insert(&x);
        ASR::BaseWalkVisitor<GpuDoConcurrentCollector>::
            visit_DoConcurrentLoop(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }
};

// Rewrites the listed DoConcurrentLoops into ordinary sequential
// DoLoops. `do concurrent` only permits the iterations to run in any
// order, so running them in order is always correct; inside device code
// it is the only thing that can be done.
class GpuHostOnlyLoopSequentializer :
        public ASR::StatementWalkVisitor<GpuHostOnlyLoopSequentializer> {
public:
    const std::set<const ASR::DoConcurrentLoop_t*> &sequential_loops;

    GpuHostOnlyLoopSequentializer(Allocator &al_,
            const std::set<const ASR::DoConcurrentLoop_t*> &loops)
        : StatementWalkVisitor(al_), sequential_loops(loops) {}

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (!sequential_loops.count(&x)) return;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            body.push_back(al, x.m_body[i]);
        }
        for (size_t i = x.n_head; i-- > 1; ) {
            ASR::stmt_t *inner = ASRUtils::STMT(ASR::make_DoLoop_t(al,
                x.base.base.loc, s2c(al, ""), x.m_head[i], body.p, body.n,
                nullptr, 0));
            body.reserve(al, 1);
            body.n = 0;
            body.push_back(al, inner);
        }
        pass_result.reserve(al, 1);
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_DoLoop_t(al,
            x.base.base.loc, s2c(al, ""), x.m_head[0], body.p, body.n,
            nullptr, 0)));
    }
};

// Metal shaders have neither variable-length arrays nor a heap, so a
// device function (an `inline` callee of a kernel) can only declare
// locals whose extent the shader compiler can fold to a constant. An
// array constructor whose elements are themselves array-valued
// expressions is materialized by the later `array_struct_temporary`
// pass into a temporary sized from those elements. An element sized
// from an assumed-shape or deferred-shape dummy argument, or from a
// local allocatable whose ALLOCATE bounds are themselves only known at
// run time, would have to be a VLA inside the device function -- which
// Metal cannot express. Detect that shape here so the loop can be
// declined and run on the host instead. Elements sized from a local
// allocatable with constant ALLOCATE bounds are fine: the Metal backend
// resolves those extents from the ALLOCATE statement.
class GpuDeviceFunctionArrayTempChecker :
        public ASR::BaseWalkVisitor<GpuDeviceFunctionArrayTempChecker> {
private:

    // Entities of the function being checked whose extent is not a
    // compile-time constant: assumed-shape/deferred-shape dummy
    // arguments, and local allocatables whose ALLOCATE bounds are only
    // known at run time.
    std::set<ASR::symbol_t*> runtime_extent_syms;

    static bool has_runtime_extent(ASR::ttype_t *t) {
        if (!t) return false;
        ASR::dimension_t *dims = nullptr;
        int rank = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::type_get_past_allocatable_pointer(t), dims);
        return rank > 0 && !ASRUtils::is_fixed_size_array(dims, rank);
    }

    class VarRefCollector :
            public ASR::BaseWalkVisitor<VarRefCollector> {
    public:
        std::set<ASR::symbol_t*> vars;

        void visit_Var(const ASR::Var_t &x) {
            vars.insert(ASRUtils::symbol_get_past_external(x.m_v));
        }
    };

    // Collects the targets of ALLOCATE statements whose bounds are not
    // compile-time constants, descending into BLOCK and ASSOCIATE
    // scopes so an allocation nested there is seen too.
    class RuntimeAllocCollector :
            public ASR::BaseWalkVisitor<RuntimeAllocCollector> {
    public:
        std::set<ASR::symbol_t*> vars;

        void visit_Allocate(const ASR::Allocate_t &x) {
            for (size_t i = 0; i < x.n_args; i++) {
                if (!x.m_args[i].m_a ||
                        !ASR::is_a<ASR::Var_t>(*x.m_args[i].m_a)) continue;
                bool all_const = true;
                for (size_t d = 0; d < x.m_args[i].n_dims; d++) {
                    ASR::expr_t *len = x.m_args[i].m_dims[d].m_length;
                    if (!len || !ASRUtils::expr_value(len)) {
                        all_const = false;
                    }
                }
                if (all_const) continue;
                vars.insert(ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(x.m_args[i].m_a)->m_v));
            }
        }

        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            for (size_t i = 0; i < blk->n_body; i++) {
                visit_stmt(*blk->m_body[i]);
            }
        }

        void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
            ASR::AssociateBlock_t *blk =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            for (size_t i = 0; i < blk->n_body; i++) {
                visit_stmt(*blk->m_body[i]);
            }
        }
    };

public:
    bool has_runtime_sized_temp = false;

    void check_function(ASR::Function_t *fn) {
        runtime_extent_syms.clear();
        for (size_t i = 0; i < fn->n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) continue;
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
            if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            ASR::ttype_t *bare = ASRUtils::extract_type(var->m_type);
            // A struct dummy carries the extents of its allocatable
            // components in run-time parameters too, so an element sized
            // from one of its components is just as unrepresentable.
            if (has_runtime_extent(var->m_type) ||
                    ASR::is_a<ASR::StructType_t>(*bare) ||
                    ASRUtils::is_class_type(bare)) {
                runtime_extent_syms.insert(sym);
            }
        }
        std::set<ASR::symbol_t*> passed_in;
        passed_in.insert(runtime_extent_syms.begin(),
            runtime_extent_syms.end());
        if (fn->m_return_var && ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
            passed_in.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v));
        }
        {
            RuntimeAllocCollector alloc_collector;
            for (size_t i = 0; i < fn->n_body; i++) {
                alloc_collector.visit_stmt(*fn->m_body[i]);
            }
            // A local array the callee sizes at run time is a
            // variable-length array in the shader, and Metal has no
            // declaration for one. This is how an array constructor
            // already lowered to a temporary reaches here: the
            // constructor is gone, and only the temporary is left.
            // Splicing the callee into the kernel moves the temporary
            // to a scope the per-thread workspace machinery can size.
            for (ASR::symbol_t *v : alloc_collector.vars) {
                if (!passed_in.count(v)) has_runtime_sized_temp = true;
            }
            runtime_extent_syms.insert(alloc_collector.vars.begin(),
                alloc_collector.vars.end());
        }
        for (auto &item : fn->m_symtab->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
            if (passed_in.count(sym)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (var->m_intent != ASR::intentType::Local) continue;
            if (ASRUtils::is_allocatable(var->m_type)) continue;
            if (has_runtime_extent(var->m_type)) {
                has_runtime_sized_temp = true;
            }
        }
        if (runtime_extent_syms.empty()) return;
        for (size_t i = 0; i < fn->n_body; i++) {
            visit_stmt(*fn->m_body[i]);
        }
    }

    void visit_ArrayConstructor(const ASR::ArrayConstructor_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::expr_t *arg = x.m_args[i];
            if (!arg) continue;
            if (!has_runtime_extent(ASRUtils::expr_type(arg))) continue;
            VarRefCollector vc;
            vc.visit_expr(*arg);
            for (ASR::symbol_t *v : vc.vars) {
                if (runtime_extent_syms.count(v)) {
                    has_runtime_sized_temp = true;
                }
            }
        }
        ASR::BaseWalkVisitor<GpuDeviceFunctionArrayTempChecker>::
            visit_ArrayConstructor(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }
};

// Counts Return statements so a device function with early returns --
// control flow a straight-line splice cannot reproduce -- is rejected.
class GpuReturnCounter : public ASR::BaseWalkVisitor<GpuReturnCounter> {
public:
    size_t count = 0;

    void visit_Return(const ASR::Return_t & /*x*/) {
        count++;
    }
};

// Counts the BLOCK and ASSOCIATE constructs a statement enters. The
// splice can only flatten a nested scope that sits directly in a
// statement list, so one reached from inside an IF or a loop -- where
// the ASR holds a single statement, not a list -- makes the callee
// un-spliceable.
class GpuNestedScopeCounter :
        public ASR::BaseWalkVisitor<GpuNestedScopeCounter> {
public:
    size_t count = 0;

    void visit_BlockCall(const ASR::BlockCall_t & /*x*/) {
        count++;
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t & /*x*/) {
        count++;
    }
};

// Collects every FunctionCall in a statement, so the inliner can tell a
// call in a spliceable position (the whole right-hand side of an
// assignment) from one buried inside a larger expression.
class GpuCallSiteCollector :
        public ASR::BaseWalkVisitor<GpuCallSiteCollector> {
public:
    std::vector<const ASR::FunctionCall_t*> calls;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        calls.push_back(&x);
        ASR::BaseWalkVisitor<GpuCallSiteCollector>::visit_FunctionCall(x);
    }
};

// Collects Var references in function bodies that point to symbols
// not reachable through the function's scope chain. This happens when
// a contained function references host-scope variables (e.g., Parameters)
// that are not present in the kernel scope hierarchy.
class DanglingVarCollector : public ASR::BaseWalkVisitor<DanglingVarCollector> {
public:
    SymbolTable *func_scope;
    std::map<std::string, ASR::symbol_t*> dangling;
    std::set<SymbolTable*> inner_scopes;
    DanglingVarCollector(SymbolTable *fs) : func_scope(fs) {}
    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        for (auto *scope : inner_scopes) {
            if (scope->get_symbol(name)) return;
        }
        if (!func_scope->resolve_symbol(name) &&
                dangling.find(name) == dangling.end()) {
            dangling[name] = x.m_v;
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        inner_scopes.insert(ab->m_symtab);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
        inner_scopes.erase(ab->m_symtab);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        inner_scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
        inner_scopes.erase(block->m_symtab);
    }
};

// Fixes dangling Var references in function bodies by resolving symbol
// names through the function's scope chain and replacing the Var target.
class DanglingVarFixer : public ASR::BaseWalkVisitor<DanglingVarFixer> {
public:
    SymbolTable *func_scope;
    std::set<std::string> &target_names;
    DanglingVarFixer(SymbolTable *fs, std::set<std::string> &names)
        : func_scope(fs), target_names(names) {}
    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (target_names.count(name)) {
            ASR::symbol_t *new_sym = func_scope->resolve_symbol(name);
            if (new_sym) {
                const_cast<ASR::Var_t&>(x).m_v = new_sym;
            }
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Collects StructInstanceMember references to allocatable array members
// in the do concurrent body. Used to decompose struct-typed kernel
// parameters into separate flat array buffers for Metal.
// Collects all variable names referenced (read) in a set of statements.
// Used to determine which variables are live after a do concurrent loop.
class PostLoopVarCollector : public ASR::BaseWalkVisitor<PostLoopVarCollector> {
public:
    std::set<std::string> &referenced_vars;
    PostLoopVarCollector(std::set<std::string> &rv) : referenced_vars(rv) {}
    void visit_Var(const ASR::Var_t &x) {
        referenced_vars.insert(ASRUtils::symbol_name(x.m_v));
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }
};

// Collects the symbols targeted by Var nodes in a set of statements,
// descending into nested Block and AssociateBlock bodies (which the base
// walker does not enter on its own). Used to decide whether a scope is
// still needed after its associate aliases have been inlined.
class VarSymbolCollector : public ASR::BaseWalkVisitor<VarSymbolCollector> {
public:
    std::set<ASR::symbol_t*> &referenced_syms;
    VarSymbolCollector(std::set<ASR::symbol_t*> &rs) : referenced_syms(rs) {}

    void visit_Var(const ASR::Var_t &x) {
        referenced_syms.insert(x.m_v);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }
};

class GpuAllocStructMemberCollector :
    public ASR::BaseWalkVisitor<GpuAllocStructMemberCollector> {
public:
    // Maps struct_var_name -> { member_name -> (member_sym, member_type) }
    std::map<std::string,
        std::map<std::string, std::pair<ASR::symbol_t*, ASR::ttype_t*>>>
            alloc_members;
    // Struct var names that have any non-allocatable-array member access
    std::set<std::string> has_non_alloc_access;

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        if (ASR::is_a<ASR::Var_t>(*x.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_v);
            std::string struct_name = ASRUtils::symbol_name(v->m_v);
            ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(x.m_m);
            std::string mem_name = ASRUtils::symbol_name(mem);
            ASR::ttype_t *mem_type = x.m_type;
            if (ASRUtils::is_allocatable(mem_type)) {
                ASR::ttype_t *inner =
                    ASRUtils::type_get_past_allocatable(mem_type);
                if (ASR::is_a<ASR::Array_t>(*inner)) {
                    alloc_members[struct_name][mem_name] =
                        {x.m_m, mem_type};
                } else {
                    has_non_alloc_access.insert(struct_name);
                }
            } else {
                has_non_alloc_access.insert(struct_name);
            }
        }
        ASR::BaseWalkVisitor<GpuAllocStructMemberCollector>::
            visit_StructInstanceMember(x);
    }
};

// Replaces StructInstanceMember(Var(x), a) with Var(x__a) for
// allocatable array members that have been decomposed into separate
// kernel parameters.
class GpuDecomposeStructReplacer :
    public ASR::BaseExprReplacer<GpuDecomposeStructReplacer> {
public:
    Allocator &al;
    SymbolTable *kernel_scope;
    std::map<std::pair<std::string, std::string>, std::string> &decomp_map;

    GpuDecomposeStructReplacer(Allocator &al_, SymbolTable *scope,
        std::map<std::pair<std::string, std::string>, std::string> &dmap)
        : al(al_), kernel_scope(scope), decomp_map(dmap) {}

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        if (ASR::is_a<ASR::Var_t>(*x->m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x->m_v);
            std::string struct_name = ASRUtils::symbol_name(v->m_v);
            ASR::symbol_t *mem =
                ASRUtils::symbol_get_past_external(x->m_m);
            std::string mem_name = ASRUtils::symbol_name(mem);
            auto key = std::make_pair(struct_name, mem_name);
            auto it = decomp_map.find(key);
            if (it != decomp_map.end()) {
                ASR::symbol_t *param_sym =
                    kernel_scope->get_symbol(it->second);
                if (param_sym) {
                    *current_expr = ASRUtils::EXPR(
                        ASR::make_Var_t(al, x->base.base.loc, param_sym));
                    return;
                }
            }
        }
        ASR::BaseExprReplacer<GpuDecomposeStructReplacer>::
            replace_StructInstanceMember(x);
    }
};

class GpuDecomposeStructVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<GpuDecomposeStructVisitor> {
public:
    GpuDecomposeStructReplacer replacer;
    GpuDecomposeStructVisitor(Allocator &al, SymbolTable *scope,
        std::map<std::pair<std::string, std::string>, std::string> &dmap)
        : replacer(al, scope, dmap) {}
    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Fortran requires the right-hand side of an array assignment to be
// evaluated as if every element were read before any element of the
// left-hand side is written. The GPU lowerings below turn an array
// assignment into an ascending element-by-element copy, which breaks that
// rule when both sides designate overlapping storage of the same array
// (`a(:) = a(n:1:-1)`, `a(3:6) = a(2:5)`): the copy then reads elements
// that the same statement has already overwritten. On the CPU path the
// later `array_struct_temporary` pass materialises the temporary that
// makes the copy safe; `gpu_offload` runs before it and lowers the loop
// body itself, so it has to materialise that temporary here. The three
// helpers below decide, conservatively, when that is needed.

// The storage a designator ultimately refers to: the variable at the
// root of the designator together with the structure members walked
// through on the way down to it. Two designators name the same storage
// only when the root variable and the whole member path agree, so that
// `x%a` and `x%b` are told apart by the member and `x%a` and `y%a` by
// the root. `root` stays null for anything that is not a designator
// this walk understands, and such a base compares unequal to every
// base, including another unknown one.
struct GpuDesignatorBase {
    ASR::symbol_t *root = nullptr;
    // Innermost member last, i.e. `x%a%b` records {b, a}. The order is
    // the walk's own and only ever compared against another path built
    // the same way.
    std::vector<ASR::symbol_t*> members;

    bool is_known() const { return root != nullptr; }

    bool operator==(const GpuDesignatorBase &other) const {
        return root != nullptr && root == other.root
            && members == other.members;
    }

    bool operator!=(const GpuDesignatorBase &other) const {
        return !(*this == other);
    }
};

static GpuDesignatorBase gpu_designator_base(ASR::expr_t *e) {
    GpuDesignatorBase base;
    while (e) {
        switch (e->type) {
            case ASR::exprType::Var: {
                base.root = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                return base;
            }
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(e);
                base.members.push_back(
                    ASRUtils::symbol_get_past_external(sm->m_m));
                e = sm->m_v;
                break;
            }
            case ASR::exprType::ArraySection: {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            }
            case ASR::exprType::ArrayItem: {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            }
            case ASR::exprType::ArrayPhysicalCast: {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            }
            default: {
                return GpuDesignatorBase();
            }
        }
    }
    return GpuDesignatorBase();
}

// Strict structural equality of two subscript expressions. Anything not
// understood here compares unequal, which makes the two designators
// differ and so errs towards materialising a temporary.
static bool gpu_same_subscript(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == b) return true;
    if (!a || !b || a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::Var: {
            return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(a)->m_v)
                == ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(b)->m_v);
        }
        case ASR::exprType::IntegerConstant: {
            return ASR::down_cast<ASR::IntegerConstant_t>(a)->m_n
                == ASR::down_cast<ASR::IntegerConstant_t>(b)->m_n;
        }
        case ASR::exprType::IntegerUnaryMinus: {
            return gpu_same_subscript(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(a)->m_arg,
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(b)->m_arg);
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *x = ASR::down_cast<ASR::IntegerBinOp_t>(a);
            ASR::IntegerBinOp_t *y = ASR::down_cast<ASR::IntegerBinOp_t>(b);
            return x->m_op == y->m_op
                && gpu_same_subscript(x->m_left, y->m_left)
                && gpu_same_subscript(x->m_right, y->m_right);
        }
        default: {
            return false;
        }
    }
}

// True when two designators are provably the same storage in the same
// element order, so an element-by-element copy between them reads only
// what it has already written to the same element.
//
// A derived-type component chain has to be recognised here, not just a
// bare variable: `self%points_(k)` written twice is two ASR nodes for
// one object, and answering false for them makes every occurrence of
// such a chain look like a different object.
static bool gpu_same_designator(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == b) return true;
    if (!a || !b || a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::Var: {
            return gpu_same_subscript(a, b);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t *x = ASR::down_cast<ASR::ArraySection_t>(a);
            ASR::ArraySection_t *y = ASR::down_cast<ASR::ArraySection_t>(b);
            if (x->n_args != y->n_args) return false;
            if (!gpu_same_designator(x->m_v, y->m_v)) return false;
            for (size_t i = 0; i < x->n_args; i++) {
                if (!gpu_same_subscript(x->m_args[i].m_left,
                        y->m_args[i].m_left)
                    || !gpu_same_subscript(x->m_args[i].m_right,
                        y->m_args[i].m_right)
                    || !gpu_same_subscript(x->m_args[i].m_step,
                        y->m_args[i].m_step)) {
                    return false;
                }
            }
            return true;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *x =
                ASR::down_cast<ASR::StructInstanceMember_t>(a);
            ASR::StructInstanceMember_t *y =
                ASR::down_cast<ASR::StructInstanceMember_t>(b);
            if (ASRUtils::symbol_get_past_external(x->m_m)
                    != ASRUtils::symbol_get_past_external(y->m_m)) {
                return false;
            }
            return gpu_same_designator(x->m_v, y->m_v);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *x = ASR::down_cast<ASR::ArrayItem_t>(a);
            ASR::ArrayItem_t *y = ASR::down_cast<ASR::ArrayItem_t>(b);
            if (x->n_args != y->n_args) return false;
            if (!gpu_same_designator(x->m_v, y->m_v)) return false;
            for (size_t i = 0; i < x->n_args; i++) {
                if (!gpu_same_subscript(x->m_args[i].m_left,
                        y->m_args[i].m_left)
                    || !gpu_same_subscript(x->m_args[i].m_right,
                        y->m_args[i].m_right)
                    || !gpu_same_subscript(x->m_args[i].m_step,
                        y->m_args[i].m_step)) {
                    return false;
                }
            }
            return true;
        }
        default: {
            return false;
        }
    }
}

// True when `outer` designates storage inside `inner`, or `inner`
// itself: the descent from `outer` towards its root passes through a
// designator naming the very same element. `x%c_(k)%v_(:,j)` is within
// `x%c_(k)`; `x%c_` is not, and neither is `x%c_(m)`.
static bool gpu_designator_within(ASR::expr_t *outer, ASR::expr_t *inner) {
    while (outer) {
        if (gpu_same_designator(outer, inner)) return true;
        switch (outer->type) {
            case ASR::exprType::StructInstanceMember: {
                outer = ASR::down_cast<ASR::StructInstanceMember_t>(
                    outer)->m_v;
                break;
            }
            case ASR::exprType::ArraySection: {
                outer = ASR::down_cast<ASR::ArraySection_t>(outer)->m_v;
                break;
            }
            case ASR::exprType::ArrayItem: {
                outer = ASR::down_cast<ASR::ArrayItem_t>(outer)->m_v;
                break;
            }
            case ASR::exprType::ArrayPhysicalCast: {
                outer = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    outer)->m_arg;
                break;
            }
            default: {
                return false;
            }
        }
    }
    return false;
}

// Reports whether an expression reads the storage of `base` through a
// designator that is not element-for-element identical to `target`. Such
// a read may see an element the assignment to `target` has already
// overwritten, so the assignment needs a temporary.
class GpuSelfAliasChecker : public ASR::BaseWalkVisitor<GpuSelfAliasChecker> {
public:
    GpuDesignatorBase base;
    ASR::expr_t *target = nullptr;
    bool aliased = false;

    void check_designator(ASR::expr_t *e) {
        if (gpu_designator_base(e) != base) return;
        if (!gpu_same_designator(target, e)) aliased = true;
    }

    void visit_Var(const ASR::Var_t &x) {
        check_designator(const_cast<ASR::expr_t*>(&x.base));
    }

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
        if (gpu_designator_base(e) == base) {
            check_designator(e);
            // The subscripts may themselves read the array, so keep
            // walking them, but do not re-visit the base designator as a
            // bare whole-array reference.
            for (size_t i = 0; i < x.n_args; i++) {
                if (x.m_args[i].m_left) visit_expr(*x.m_args[i].m_left);
                if (x.m_args[i].m_right) visit_expr(*x.m_args[i].m_right);
                if (x.m_args[i].m_step) visit_expr(*x.m_args[i].m_step);
            }
            return;
        }
        ASR::BaseWalkVisitor<GpuSelfAliasChecker>::visit_ArraySection(x);
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
        if (gpu_designator_base(e) == base) {
            check_designator(e);
            for (size_t i = 0; i < x.n_args; i++) {
                if (x.m_args[i].m_left) visit_expr(*x.m_args[i].m_left);
                if (x.m_args[i].m_right) visit_expr(*x.m_args[i].m_right);
                if (x.m_args[i].m_step) visit_expr(*x.m_args[i].m_step);
            }
            return;
        }
        ASR::BaseWalkVisitor<GpuSelfAliasChecker>::visit_ArrayItem(x);
    }
};

// Every symbol whose storage a statement list may modify, identified by
// the root of the designator written to.  A gather may only be hoisted
// out of a loop when nothing in the loop can change what it copied, so
// this errs towards reporting a write: an actual argument bound to a
// dummy that is not `intent(in)`, or to a callee that cannot be
// resolved, counts as written.
class GpuWrittenRootCollector :
        public ASR::BaseWalkVisitor<GpuWrittenRootCollector> {
public:
    std::set<ASR::symbol_t*> roots;
    // Every designator that was written, kept alongside the roots so a
    // caller can ask not just whether an object is written but where.
    std::vector<ASR::expr_t*> targets;
    // The roots among them that a callee writes through an actual
    // argument, rather than the loop body writing them itself.
    std::set<ASR::symbol_t*> call_roots;

    void note(ASR::expr_t *e) {
        GpuDesignatorBase b = gpu_designator_base(e);
        if (b.is_known()) {
            roots.insert(b.root);
            targets.push_back(e);
        }
    }

    void note_call_written(ASR::expr_t *e) {
        GpuDesignatorBase b = gpu_designator_base(e);
        if (b.is_known()) call_roots.insert(b.root);
        note(e);
    }

    // The dummy at position `i` of `name`, or nullptr when the callee
    // is not a plain Function.
    //
    // A type-bound call names a StructMethodDeclaration rather than the
    // procedure itself.  `insert_self_arg` has already put the passed
    // object at its declared position, so the call's actual arguments
    // stand in 1:1 correspondence with the bound procedure's dummies and
    // the binding can simply be stepped through.  Without this every
    // actual argument of every type-bound call reads as written.
    static ASR::Variable_t* dummy_of(ASR::symbol_t *name, size_t i) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
        if (!s) return nullptr;
        s = ASRUtils::symbol_get_past_StructMethodDeclaration(s);
        if (!s) return nullptr;
        s = ASRUtils::symbol_get_past_external(s);
        if (!s || !ASR::is_a<ASR::Function_t>(*s)) return nullptr;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
        if (i >= fn->n_args) return nullptr;
        if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) return nullptr;
        ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
        if (!d || !ASR::is_a<ASR::Variable_t>(*d)) return nullptr;
        return ASR::down_cast<ASR::Variable_t>(d);
    }

    void note_call_args(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            ASR::Variable_t *d = dummy_of(name, i);
            if (d != nullptr && d->m_intent == ASR::intentType::In) continue;
            note_call_written(args[i].m_value);
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        note(x.m_target);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        note(x.m_target);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_Associate(x);
    }

    void visit_Allocate(const ASR::Allocate_t &x) {
        for (size_t i = 0; i < x.n_args; i++) note(x.m_args[i].m_a);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_Allocate(x);
    }

    void visit_ReAlloc(const ASR::ReAlloc_t &x) {
        for (size_t i = 0; i < x.n_args; i++) note(x.m_args[i].m_a);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_ReAlloc(x);
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) note(x.m_vars[i]);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>
            ::visit_ExplicitDeallocate(x);
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) note(x.m_vars[i]);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>
            ::visit_ImplicitDeallocate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        note(x.m_head.m_v);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) note(x.m_head[i].m_v);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>
            ::visit_DoConcurrentLoop(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        note_call_args(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        note_call_args(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_FunctionCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
    }
};

// `x%c_(k)`: one element of an array of derived type that is itself a
// component of something the kernel is handed.  A Metal kernel receives
// such a component as flat data plus per-element offsets and extents,
// and nothing on the device carries the extents of the array components
// hanging off the element that was selected -- so an expression as
// ordinary as `size(x%c_(k)%upper_, 1)` cannot be evaluated there and
// the loop is declined.
//
// Copying the element into a temporary on the host before the launch
// removes the whole difficulty: the temporary is an ordinary derived-type
// kernel argument, and `size(t%upper_, 1)` is the shape the existing
// per-component extent machinery already resolves.
//
// An element reached directly from a variable, `c(k)`, is deliberately
// not collected: that shape is marshalled correctly today.
class GpuStructElementGatherCollector :
        public ASR::BaseWalkVisitor<GpuStructElementGatherCollector> {
public:
    std::vector<ASR::ArrayItem_t*> found;

    static bool is_gatherable(const ASR::ArrayItem_t &x) {
        ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
            const_cast<ASR::ttype_t*>(x.m_type));
        if (!t || ASR::is_a<ASR::Array_t>(*t)) return false;
        if (!ASR::is_a<ASR::StructType_t>(*t)) return false;
        // A polymorphic element carries a dynamic type the copy would
        // not reproduce.
        if (ASRUtils::is_class_type(t)) return false;
        ASR::expr_t *base = gpu_past_array_physical_cast(x.m_v);
        if (!base || !ASR::is_a<ASR::StructInstanceMember_t>(*base)) {
            return false;
        }
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_left || x.m_args[i].m_step) return false;
            if (!x.m_args[i].m_right) return false;
        }
        return gpu_designator_base(x.m_v).is_known();
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        if (is_gatherable(x)) {
            found.push_back(const_cast<ASR::ArrayItem_t*>(&x));
        }
        ASR::BaseWalkVisitor<GpuStructElementGatherCollector>
            ::visit_ArrayItem(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
    }
};

// Every symbol an expression mentions.
class GpuExprSymbolCollector :
        public ASR::BaseWalkVisitor<GpuExprSymbolCollector> {
public:
    std::set<ASR::symbol_t*> syms;

    void visit_Var(const ASR::Var_t &x) {
        syms.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }
};

// One gathered element: the designator that was copied and the host
// temporary that now stands for it.
struct GpuStructElementGather {
    ASR::expr_t *chain = nullptr;
    ASR::symbol_t *temp = nullptr;
    // The loop writes into this element, so the temporary has to be
    // copied back over it once the kernel has finished.
    bool scatter = false;
};

// Replaces each collected designator with a reference to its temporary,
// recording every slot it overwrites so the substitution can be undone
// when the loop turns out not to be offloadable after all.
class GpuStructElementGatherReplacer :
        public ASR::BaseExprReplacer<GpuStructElementGatherReplacer> {
public:
    Allocator &al;
    const std::vector<GpuStructElementGather> &gathers;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo;

    GpuStructElementGatherReplacer(Allocator &al_,
            const std::vector<GpuStructElementGather> &gathers_,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo_)
        : al(al_), gathers(gathers_), undo(undo_) {}

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        ASR::expr_t *e = ASRUtils::EXPR((ASR::asr_t*)x);
        for (const GpuStructElementGather &g : gathers) {
            if (!gpu_same_designator(g.chain, e)) continue;
            undo.push_back({current_expr, *current_expr});
            *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
                x->base.base.loc, g.temp));
            return;
        }
        ASR::BaseExprReplacer<GpuStructElementGatherReplacer>
            ::replace_ArrayItem(x);
    }
};

class GpuStructElementGatherVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<
            GpuStructElementGatherVisitor> {
public:
    GpuStructElementGatherReplacer replacer;

    GpuStructElementGatherVisitor(Allocator &al,
            const std::vector<GpuStructElementGather> &gathers,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo)
        : replacer(al, gathers, undo) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// Undoes the gather substitution when the loop turns out not to be
// offloadable.  The pass must leave a declined loop exactly as it found
// it, and the substitution is made before the eligibility checks so that
// they judge the shape the kernel would actually be built from.
class GpuGatherGuard {
public:
    GpuGatherGuard(SymbolTable *scope,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo,
            std::vector<std::string> &names)
        : scope_(scope), undo_(undo), names_(names) {}

    ~GpuGatherGuard() {
        if (committed_) return;
        for (size_t i = undo_.size(); i > 0; i--) {
            *undo_[i - 1].first = undo_[i - 1].second;
        }
        undo_.clear();
        for (const std::string &n : names_) {
            if (scope_ != nullptr) scope_->erase_symbol(n);
        }
        names_.clear();
    }

    void commit() { committed_ = true; }

private:
    SymbolTable *scope_;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo_;
    std::vector<std::string> &names_;
    bool committed_ = false;
};

class GpuOffloadVisitor : public ASR::StatementWalkVisitor<GpuOffloadVisitor>
{
public:
    PassOptions pass_options;
    ASR::TranslationUnit_t &tu;
    // Scalar variables that receive the result of an inlined all()
    // reduction. These need to be passed back from the GPU kernel.
    std::set<std::string> all_reduction_targets;

    GpuOffloadVisitor(Allocator &al, PassOptions pass_options_,
                      ASR::TranslationUnit_t &tu_)
        : StatementWalkVisitor(al), pass_options(pass_options_), tu(tu_) {
        GpuOffloadReport::configure(pass_options);
    }

    // Name of the procedure the loop being visited sits in, for
    // --gpu-offload-report. BLOCK and ASSOCIATE scopes are skipped: they
    // are not what a user calls the enclosing procedure.
    std::string report_enclosing_proc() const {
        SymbolTable *scope = current_scope;
        while (scope && scope->asr_owner
                && scope->asr_owner->type == ASR::asrType::symbol) {
            ASR::symbol_t *owner = down_cast<ASR::symbol_t>(scope->asr_owner);
            if (!ASR::is_a<ASR::Block_t>(*owner)
                    && !ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
                return ASRUtils::symbol_name(owner);
            }
            scope = scope->parent;
        }
        return "<global>";
    }

    // Load any module dependencies of a loaded submodule TU into
    // the main TU's symbol table so that fix_external_symbols can
    // resolve them.  This handles transitive dependencies: if the
    // submodule uses module A which in turn uses module B, both A
    // and B are loaded.  After loading, fix_external_symbols is
    // called on the main TU to resolve any null m_external pointers
    // in the newly loaded modules.
    void load_submodule_deps(ASR::TranslationUnit_t &sub_tu) {
        std::vector<std::string> pending =
            ASRUtils::determine_module_dependencies(sub_tu);
        std::set<std::string> seen;
        bool loaded_any = false;

        while (!pending.empty()) {
            std::string dep_name = pending.back();
            pending.pop_back();
            if (seen.count(dep_name)) continue;
            seen.insert(dep_name);
            if (tu.m_symtab->get_symbol(dep_name) != nullptr)
                continue;
            if (sub_tu.m_symtab->get_symbol(dep_name) != nullptr)
                continue;
            bool is_intrinsic =
                startswith(dep_name, "lfortran_intrinsic");
            LocationManager lm_dep;
            auto dep_res = ASRUtils::find_and_load_module(
                al, dep_name, *tu.m_symtab, is_intrinsic,
                pass_options, lm_dep);
            if (!dep_res.ok && !is_intrinsic) {
                if (dep_name == "iso_c_binding" ||
                        dep_name == "iso_fortran_env") {
                    LocationManager lm_dep2;
                    auto dep_res2 =
                        ASRUtils::find_and_load_module(
                            al,
                            "lfortran_intrinsic_" + dep_name,
                            *tu.m_symtab, true, pass_options,
                            lm_dep2);
                    if (dep_res2.ok) {
                        ASR::Module_t *dep_mod =
                            ASRUtils::extract_module(
                                *dep_res2.result);
                        tu.m_symtab->add_symbol(dep_name,
                            (ASR::symbol_t*)dep_mod);
                        dep_mod->m_symtab->parent =
                            tu.m_symtab;
                        dep_mod->m_loaded_from_mod = true;
                        loaded_any = true;
                        for (size_t i = 0;
                                i < dep_mod->n_dependencies; i++) {
                            pending.push_back(
                                dep_mod->m_dependencies[i]);
                        }
                    }
                    continue;
                }
            }
            if (!dep_res.ok) continue;
            ASR::Module_t *dep_mod =
                ASRUtils::extract_module(*dep_res.result);
            tu.m_symtab->add_symbol(dep_name,
                (ASR::symbol_t*)dep_mod);
            dep_mod->m_symtab->parent = tu.m_symtab;
            dep_mod->m_loaded_from_mod = true;
            loaded_any = true;
            for (size_t i = 0; i < dep_mod->n_dependencies; i++) {
                pending.push_back(dep_mod->m_dependencies[i]);
            }
        }

        if (loaded_any) {
            fix_external_symbols(tu, *tu.m_symtab);
        }
    }

    // A submodule read back from its `.smod` file carries the array
    // constructors it was written with: the implied-do lowering ran over
    // this translation unit before this pass, so it never saw this body.
    // Splicing it into a kernel as it stands would carry an implied-do
    // into everything downstream -- the temporary extraction that runs
    // after this pass hoists a loop-variant element out of one, which
    // evaluates it once instead of once per iteration, and the Metal code
    // generator has no rendering for what is left. Lower them here, so a
    // body loaded from disk is in the same shape as one compiled
    // alongside its caller. Run only once the external symbols of the
    // loaded unit are resolved, since the lowering reads their types.
    void lower_loaded_implied_do_loops(ASR::TranslationUnit_t &sub_tu) {
        pass_replace_implied_do_loops(al, sub_tu, pass_options);
    }

    // Duplicate an expression, remapping all Var references to point to the
    // given scope. Used to create kernel-scope copies of head expressions.
    ASR::expr_t* dup_expr_to_scope(ASR::expr_t *expr, SymbolTable *scope) {
        if (!expr) return nullptr;
        ASRUtils::ExprStmtDuplicator duplicator(al);
        duplicator.success = true;
        ASR::expr_t *copy = duplicator.duplicate_expr(expr);
        if (!copy) return expr;
        GpuReplaceSymbols replacer(*scope);
        replacer.current_expr = &copy;
        replacer.replace_expr(copy);
        return copy;
    }

    // Recursively remap ExternalSymbol targets and Variable
    // m_type_declarations inside `scope` (and all nested child scopes)
    // so they reference the kernel-scope struct copies instead of the
    // original module definitions.  Also redirect ExternalSymbols that
    // point to functions already duplicated into the kernel scope.
    void fixup_struct_refs_in_scope(SymbolTable *scope,
            SymbolTable *kernel_scope,
            char *kernel_fn_name = nullptr) {
        for (auto &item : scope->get_scope()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                ASR::ExternalSymbol_t *es =
                    ASR::down_cast<ASR::ExternalSymbol_t>(item.second);
                if (!es->m_external) continue;
                ASR::symbol_t *target =
                    ASRUtils::symbol_get_past_external(es->m_external);
                if (!target) continue;
                SymbolTable *tp =
                    ASRUtils::symbol_parent_symtab(target);
                if (tp->asr_owner &&
                        tp->asr_owner->type == ASR::asrType::symbol) {
                    ASR::symbol_t *os =
                        ASR::down_cast<ASR::symbol_t>(tp->asr_owner);
                    if (ASR::is_a<ASR::Struct_t>(*os)) {
                        std::string sn =
                            ASR::down_cast<ASR::Struct_t>(os)->m_name;
                        ASR::symbol_t *ks =
                            kernel_scope->get_symbol(sn);
                        if (ks && ASR::is_a<ASR::Struct_t>(*ks)) {
                            ASR::symbol_t *nt =
                                ASR::down_cast<ASR::Struct_t>(ks)
                                    ->m_symtab->get_symbol(
                                        es->m_original_name);
                            if (nt) {
                                es->m_external = nt;
                                // The member now lives in the kernel's
                                // struct copy; update m_module_name to
                                // the struct name (the verifier checks
                                // that it matches the containing scope).
                                es->m_module_name = ASR::down_cast<
                                    ASR::Struct_t>(ks)->m_name;
                            }
                        }
                    }
                }
                // If the ExternalSymbol references a function that has
                // been duplicated into the kernel scope, redirect to the
                // kernel-scope copy so that Call_t_body checks use the
                // fixed-up formal parameter types.
                if (ASR::is_a<ASR::Function_t>(*target)) {
                    std::string fn =
                        ASRUtils::symbol_name(target);
                    ASR::symbol_t *ks =
                        kernel_scope->get_symbol(fn);
                    if (ks && ASR::is_a<ASR::Function_t>(*ks)) {
                        es->m_external = ks;
                        if (kernel_fn_name) {
                            es->m_module_name = kernel_fn_name;
                        }
                    }
                }
            } else if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                ASR::symbol_t *tdecl_resolved =
                    var->m_type_declaration
                        ? ASRUtils::symbol_get_past_external(
                              var->m_type_declaration)
                        : nullptr;
                if (tdecl_resolved &&
                        ASR::is_a<ASR::Struct_t>(*tdecl_resolved)) {
                    std::string sn = ASRUtils::symbol_name(
                        tdecl_resolved);
                    ASR::symbol_t *ks =
                        kernel_scope->get_symbol(sn);
                    if (ks) var->m_type_declaration = ks;
                }
            }
            // Recurse into nested scopes
            SymbolTable *nested = nullptr;
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                nested = ASR::down_cast<ASR::Function_t>(
                    item.second)->m_symtab;
            } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
                nested = ASR::down_cast<ASR::Block_t>(
                    item.second)->m_symtab;
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                nested = ASR::down_cast<ASR::AssociateBlock_t>(
                    item.second)->m_symtab;
            }
            if (nested) {
                fixup_struct_refs_in_scope(nested, kernel_scope,
                    kernel_fn_name);
            }
        }
    }

    // Find a Struct in kernel_scope by name, with PDT fallback.
    // If the exact name is not found (e.g., "network_t"), look for a
    // PDT instantiation (e.g., "network_t_4") that has a member named
    // member_name. Returns the Struct symbol or nullptr.
    ASR::symbol_t* find_kernel_struct(SymbolTable *kernel_scope,
            const std::string &struct_name,
            const std::string &member_name) {
        ASR::symbol_t *sym = kernel_scope->get_symbol(struct_name);
        if (sym && is_a<ASR::Struct_t>(*sym)) return sym;
        for (auto &item : kernel_scope->get_scope()) {
            if (!is_a<ASR::Struct_t>(*item.second)) continue;
            ASR::Struct_t *s = down_cast<ASR::Struct_t>(item.second);
            if (get_struct_member_recursive(s, member_name)) {
                return item.second;
            }
        }
        return nullptr;
    }

    // Import a Struct definition into kernel scope, recursively handling
    // nested struct-typed members. Also creates ExternalSymbol entries
    // in kernel_scope for members referenced from orig_scope.
    ASR::symbol_t* import_struct_def(ASR::Struct_t *orig_struct,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const Location &loc) {
        std::string struct_name = orig_struct->m_name;

        // If already imported, return existing
        ASR::symbol_t *existing = kernel_scope->get_symbol(struct_name);
        if (existing) return existing;

        // Import the parent type first, so the extending type keeps its
        // inheritance chain in the kernel scope. Members inherited from
        // the parent are declared in the parent's symtab, so without this
        // they would be lost entirely. The parent chain is acyclic, so
        // this terminates; and since the parent is added to kernel_scope
        // before this struct, the early-return guard above stays correct.
        ASR::symbol_t *new_parent = nullptr;
        if (orig_struct->m_parent) {
            ASR::symbol_t *parent_sym = ASRUtils::symbol_get_past_external(
                orig_struct->m_parent);
            if (is_a<ASR::Struct_t>(*parent_sym)) {
                new_parent = import_struct_def(
                    down_cast<ASR::Struct_t>(parent_sym),
                    orig_scope, kernel_scope, loc);
            }
        }

        // Deep-copy the Struct into kernel scope
        SymbolTable *new_st = al.make_new<SymbolTable>(kernel_scope);
        for (auto &item : orig_struct->m_symtab->get_scope()) {
            ASR::symbol_t *member = item.second;
            if (is_a<ASR::Variable_t>(*member)) {
                ASR::Variable_t *mv = down_cast<ASR::Variable_t>(member);
                // If the member itself has StructType, recursively import
                // the inner struct so we can set its type_declaration
                ASR::symbol_t *member_type_decl = nullptr;
                if (ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(mv->m_type)) &&
                        mv->m_type_declaration) {
                    ASR::symbol_t *inner_sym =
                        ASRUtils::symbol_get_past_external(
                            mv->m_type_declaration);
                    if (is_a<ASR::Struct_t>(*inner_sym)) {
                        member_type_decl = import_struct_def(
                            down_cast<ASR::Struct_t>(inner_sym),
                            orig_scope, kernel_scope, loc);
                    }
                }
                ASR::symbol_t *new_member = down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, new_st,
                        s2c(al, item.first), nullptr, 0,
                        mv->m_intent, nullptr, nullptr,
                        mv->m_storage, ASRUtils::duplicate_type(al, mv->m_type),
                        member_type_decl, mv->m_abi, mv->m_access,
                        mv->m_presence, false));
                new_st->add_symbol(item.first, new_member);
            } else if (is_a<ASR::StructMethodDeclaration_t>(*member)) {
                ASR::StructMethodDeclaration_t *smd =
                    down_cast<ASR::StructMethodDeclaration_t>(member);
                ASR::asr_t *new_smd = ASR::make_StructMethodDeclaration_t(
                    al, loc, new_st, s2c(al, item.first),
                    smd->m_self_argument, smd->m_proc_name,
                    smd->m_proc, smd->m_abi,
                    smd->m_is_deferred, smd->m_is_nopass);
                new_st->add_symbol(item.first,
                    down_cast<ASR::symbol_t>(new_smd));
            }
        }

        // Duplicate the struct signature type
        ASR::ttype_t *new_sig = ASRUtils::duplicate_type(al, orig_struct->m_struct_signature);

        // Copy member names
        char **new_members = al.allocate<char*>(orig_struct->n_members);
        for (size_t i = 0; i < orig_struct->n_members; i++) {
            new_members[i] = orig_struct->m_members[i];
        }

        ASR::asr_t *new_struct = ASR::make_Struct_t(al, loc,
            new_st, s2c(al, struct_name), new_sig,
            nullptr, 0,
            new_members, orig_struct->n_members,
            nullptr, 0,
            orig_struct->m_abi, orig_struct->m_access,
            orig_struct->m_is_packed, orig_struct->m_is_abstract,
            orig_struct->m_is_sequence,
            nullptr, 0, nullptr, new_parent, nullptr, 0);
        ASR::symbol_t *kernel_struct = down_cast<ASR::symbol_t>(new_struct);
        kernel_scope->add_symbol(struct_name, kernel_struct);

        // Create ExternalSymbol entries in kernel scope for each member,
        // so that StructInstanceMember can reference them.
        // Search orig_scope and walk up through AssociateBlock/Block
        // parent scopes, because when the do concurrent is inside an
        // AssociateBlock the ExternalSymbol entries for struct members
        // live in the enclosing function scope, not in the
        // AssociateBlock's scope.
        SymbolTable *search_scope = orig_scope;
        while (search_scope) {
            for (auto &item : search_scope->get_scope()) {
                if (!is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
                ASR::ExternalSymbol_t *es = down_cast<ASR::ExternalSymbol_t>(item.second);
                ASR::symbol_t *es_external = ASRUtils::symbol_get_past_external(es->m_external);
                // Check if this ExternalSymbol refers to a member of our struct.
                // For PDT instantiations (e.g., network_t_4), also match
                // ExternalSymbols pointing to the PDT template struct
                // (e.g., network_t) when the instantiated struct has a
                // member with the same original name.
                SymbolTable *es_parent_st =
                    ASRUtils::symbol_parent_symtab(es_external);
                bool is_member = (es_parent_st == orig_struct->m_symtab);
                if (!is_member && es_parent_st->asr_owner &&
                        es_parent_st->asr_owner->type == ASR::asrType::symbol) {
                    ASR::symbol_t *es_struct_owner =
                        down_cast<ASR::symbol_t>(es_parent_st->asr_owner);
                    if (is_a<ASR::Struct_t>(*es_struct_owner) &&
                            get_struct_member_recursive(
                                down_cast<ASR::Struct_t>(kernel_struct),
                                es->m_original_name)) {
                        is_member = true;
                    }
                }
                if (is_member) {
                    std::string es_name = item.first;
                    if (kernel_scope->get_symbol(es_name)) continue;
                    ASR::symbol_t *new_member_in_struct =
                        get_struct_member_recursive(
                            down_cast<ASR::Struct_t>(kernel_struct),
                            es->m_original_name);
                    if (!new_member_in_struct) continue;
                    std::string owner_name = struct_member_owner_name(
                        new_member_in_struct, struct_name);
                    ASR::asr_t *new_es = ASR::make_ExternalSymbol_t(al, loc,
                        kernel_scope, s2c(al, es_name),
                        new_member_in_struct, s2c(al, owner_name),
                        nullptr, 0, s2c(al, es->m_original_name),
                        es->m_access);
                    kernel_scope->add_symbol(es_name,
                        down_cast<ASR::symbol_t>(new_es));
                }
            }
            if (search_scope->asr_owner &&
                    search_scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner = down_cast<ASR::symbol_t>(
                    search_scope->asr_owner);
                if (is_a<ASR::AssociateBlock_t>(*owner) ||
                        is_a<ASR::Block_t>(*owner)) {
                    search_scope = search_scope->parent;
                    continue;
                }
            }
            break;
        }

        return kernel_struct;
    }

    // For a struct-typed variable, get the type_declaration symbol
    // from the original scope and ensure the Struct (and its member
    // ExternalSymbols) exist in the kernel scope.
    ASR::symbol_t* import_struct_type(ASR::symbol_t *orig_sym,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const Location &loc) {
        if (!is_a<ASR::Variable_t>(*orig_sym)) return nullptr;
        ASR::Variable_t *var = down_cast<ASR::Variable_t>(orig_sym);
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(var->m_type))) return nullptr;
        ASR::symbol_t *type_decl = var->m_type_declaration;
        if (!type_decl) return nullptr;
        ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(type_decl);
        if (!is_a<ASR::Struct_t>(*struct_sym)) return nullptr;
        // Use orig_scope (the do concurrent's enclosing scope) rather
        // than var->m_parent_symtab. When the do concurrent is inside
        // an AssociateBlock, ExternalSymbol entries for struct members
        // (e.g., type-bound procedure references) are migrated from
        // inner associate scopes into orig_scope during associate
        // resolution. The variable's declaring scope may be a parent
        // of orig_scope and would not contain these migrated symbols.
        return import_struct_def(down_cast<ASR::Struct_t>(struct_sym),
            orig_scope, kernel_scope, loc);
    }

    // Walk a mask expression to find the first ArraySection and extract
    // its loop bounds (start, end).
    void find_array_section_bounds(ASR::expr_t *e,
            ASR::expr_t *&loop_start, ASR::expr_t *&loop_end) {
        if (loop_start) return;
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
            if (as->n_args > 0 && as->m_args[0].m_left && as->m_args[0].m_right) {
                loop_start = as->m_args[0].m_left;
                loop_end = as->m_args[0].m_right;
            }
        } else if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(e));
            if (ASR::is_a<ASR::Array_t>(*type)) {
                ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, e->base.loc, 4));
                ASR::expr_t *dim1 = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, e->base.loc, 1,
                        int_type, ASR::integerbozType::Decimal));
                loop_start = ASRUtils::EXPR(
                    ASR::make_ArrayBound_t(al, e->base.loc,
                        e, dim1, int_type,
                        ASR::arrayboundType::LBound, nullptr));
                loop_end = ASRUtils::EXPR(
                    ASR::make_ArrayBound_t(al, e->base.loc,
                        e, dim1, int_type,
                        ASR::arrayboundType::UBound, nullptr));
            }
        } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
            ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
            find_array_section_bounds(rc->m_left, loop_start, loop_end);
            find_array_section_bounds(rc->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
            ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
            find_array_section_bounds(ic->m_left, loop_start, loop_end);
            find_array_section_bounds(ic->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            find_array_section_bounds(lb->m_left, loop_start, loop_end);
            find_array_section_bounds(lb->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
            ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
            find_array_section_bounds(rb->m_left, loop_start, loop_end);
            find_array_section_bounds(rb->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            find_array_section_bounds(ib->m_left, loop_start, loop_end);
            find_array_section_bounds(ib->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
            ASR::IntrinsicElementalFunction_t *ief =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            for (size_t i = 0; i < ief->n_args; i++) {
                if (ief->m_args[i])
                    find_array_section_bounds(ief->m_args[i],
                        loop_start, loop_end);
            }
        }
    }

    // Collect per-dimension bounds from array sections in an expression.
    // Returns bounds for all dimensions of the first ArraySection found.
    void find_array_section_all_bounds(ASR::expr_t *e,
            std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> &dim_bounds) {
        if (!dim_bounds.empty()) return;
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right) {
                    dim_bounds.push_back({as->m_args[i].m_left,
                        as->m_args[i].m_right});
                }
            }
        } else if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(e));
            if (ASR::is_a<ASR::Array_t>(*type)) {
                ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, e->base.loc, 4));
                ASR::dimension_t *dims = nullptr;
                int rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
                for (int d = 0; d < rank; d++) {
                    ASR::expr_t *dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, e->base.loc, d + 1,
                            int_type, ASR::integerbozType::Decimal));
                    ASR::expr_t *lb = ASRUtils::EXPR(
                        ASR::make_ArrayBound_t(al, e->base.loc,
                            e, dim_expr, int_type,
                            ASR::arrayboundType::LBound, nullptr));
                    ASR::expr_t *ub = ASRUtils::EXPR(
                        ASR::make_ArrayBound_t(al, e->base.loc,
                            e, dim_expr, int_type,
                            ASR::arrayboundType::UBound, nullptr));
                    dim_bounds.push_back({lb, ub});
                }
            }
        } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
            ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
            find_array_section_all_bounds(rc->m_left, dim_bounds);
            find_array_section_all_bounds(rc->m_right, dim_bounds);
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
            ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
            find_array_section_all_bounds(ic->m_left, dim_bounds);
            find_array_section_all_bounds(ic->m_right, dim_bounds);
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            find_array_section_all_bounds(lb->m_left, dim_bounds);
            find_array_section_all_bounds(lb->m_right, dim_bounds);
        } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
            ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
            find_array_section_all_bounds(rb->m_left, dim_bounds);
            find_array_section_all_bounds(rb->m_right, dim_bounds);
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            find_array_section_all_bounds(ib->m_left, dim_bounds);
            find_array_section_all_bounds(ib->m_right, dim_bounds);
        } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
            ASR::IntrinsicElementalFunction_t *ief =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            for (size_t i = 0; i < ief->n_args; i++) {
                if (ief->m_args[i])
                    find_array_section_all_bounds(ief->m_args[i], dim_bounds);
            }
        }
    }

    // Build an element-wise expression by replacing ArraySection and
    // whole-array Var nodes with ArrayItem nodes indexed by loop_var.
    ASR::expr_t* elementize_mask(ASR::expr_t *e, ASR::expr_t *loop_var,
            ASR::ttype_t *logical_type, const Location &loc) {
        std::vector<ASR::expr_t*> vars = {loop_var};
        return elementize_mask_multi(e, vars, logical_type, loc);
    }

    // Build an element-wise expression by replacing ArraySection and
    // whole-array Var nodes with ArrayItem nodes indexed by per-dimension
    // loop variables.
    ASR::expr_t* elementize_mask_multi(ASR::expr_t *e,
            std::vector<ASR::expr_t*> &loop_vars,
            ASR::ttype_t *logical_type, const Location &loc) {
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
            Vec<ASR::array_index_t> new_args;
            new_args.reserve(al, as->n_args);
            size_t lv_idx = 0;
            for (size_t i = 0; i < as->n_args; i++) {
                ASR::array_index_t idx;
                idx.loc = as->m_args[i].loc;
                if (as->m_args[i].m_left && as->m_args[i].m_right) {
                    idx.m_left = nullptr;
                    idx.m_right = (lv_idx < loop_vars.size())
                        ? loop_vars[lv_idx++] : loop_vars[0];
                    idx.m_step = nullptr;
                } else {
                    idx.m_left = as->m_args[i].m_left;
                    idx.m_right = as->m_args[i].m_right;
                    idx.m_step = as->m_args[i].m_step;
                }
                new_args.push_back(al, idx);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(as->m_v));
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                as->m_v, new_args.p, new_args.n,
                elem_type, ASR::arraystorageType::ColMajor, nullptr));
        } else if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(e));
            if (ASR::is_a<ASR::Array_t>(*type)) {
                ASR::dimension_t *dims = nullptr;
                int rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
                Vec<ASR::array_index_t> new_args;
                new_args.reserve(al, rank);
                for (int d = 0; d < rank; d++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = nullptr;
                    idx.m_right = (d < (int)loop_vars.size())
                        ? loop_vars[d] : loop_vars[0];
                    idx.m_step = nullptr;
                    new_args.push_back(al, idx);
                }
                ASR::ttype_t *elem_type = ASRUtils::extract_type(type);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                    e, new_args.p, new_args.n,
                    elem_type, ASR::arraystorageType::ColMajor, nullptr));
            }
            return e;
        } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
            ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
            return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
                elementize_mask_multi(rc->m_left, loop_vars, logical_type, loc),
                rc->m_op,
                elementize_mask_multi(rc->m_right, loop_vars, logical_type, loc),
                logical_type, nullptr));
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
            ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
            return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                elementize_mask_multi(ic->m_left, loop_vars, logical_type, loc),
                ic->m_op,
                elementize_mask_multi(ic->m_right, loop_vars, logical_type, loc),
                logical_type, nullptr));
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                elementize_mask_multi(lb->m_left, loop_vars, logical_type, loc),
                lb->m_op,
                elementize_mask_multi(lb->m_right, loop_vars, logical_type, loc),
                logical_type, nullptr));
        } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
            ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
            ASR::ttype_t *real_type = ASRUtils::extract_type(
                ASRUtils::expr_type(e));
            return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                elementize_mask_multi(rb->m_left, loop_vars, logical_type, loc),
                rb->m_op,
                elementize_mask_multi(rb->m_right, loop_vars, logical_type, loc),
                real_type, nullptr));
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            ASR::ttype_t *int_elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(e));
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                elementize_mask_multi(ib->m_left, loop_vars, logical_type, loc),
                ib->m_op,
                elementize_mask_multi(ib->m_right, loop_vars, logical_type, loc),
                int_elem_type, nullptr));
        } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
            ASR::IntrinsicElementalFunction_t *ief =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            Vec<ASR::expr_t*> new_args;
            new_args.reserve(al, ief->n_args);
            for (size_t i = 0; i < ief->n_args; i++) {
                new_args.push_back(al, ief->m_args[i]
                    ? elementize_mask_multi(ief->m_args[i], loop_vars,
                          logical_type, loc)
                    : nullptr);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(e));
            return ASRUtils::EXPR(
                ASR::make_IntrinsicElementalFunction_t(al, loc,
                    ief->m_intrinsic_id, new_args.p, new_args.n,
                    ief->m_overload_id, elem_type, nullptr));
        }
        return e;
    }

    // Inline a single IntrinsicArrayFunction All into preamble statements
    // and return a Var expression referencing the result. Returns nullptr
    // if the All cannot be inlined.
    ASR::expr_t* inline_single_all(ASR::IntrinsicArrayFunction_t *iaf,
            const Location &loc, Vec<ASR::stmt_t*> &preamble) {
        if (iaf->n_args < 1 || !iaf->m_args[0]) return nullptr;
        ASR::expr_t *mask = iaf->m_args[0];

        std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> dim_bounds;
        find_array_section_all_bounds(mask, dim_bounds);
        if (dim_bounds.empty()) return nullptr;

        ASR::ttype_t *logical_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        // Create loop variables for each dimension
        std::vector<ASR::expr_t*> loop_vars;
        for (size_t d = 0; d < dim_bounds.size(); d++) {
            std::string loop_var_name = var_scope->get_unique_name(
                "__gpu_all_i" + std::to_string(d));
            ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, var_scope,
                    s2c(al, loop_var_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            var_scope->add_symbol(loop_var_name, loop_var_sym);
            loop_vars.push_back(ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, loop_var_sym)));
        }

        // Create result variable
        std::string res_var_name = var_scope->get_unique_name("__gpu_all_res");
        ASR::symbol_t *res_var_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, var_scope,
                s2c(al, res_var_name), nullptr, 0,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default,
                ASRUtils::duplicate_type(al, logical_type),
                nullptr, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required, false));
        var_scope->add_symbol(res_var_name, res_var_sym);
        ASR::expr_t *res_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, res_var_sym));

        // __gpu_all_res = .true.
        preamble.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var,
                ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                    true, logical_type)),
                nullptr, false, false)));

        ASR::expr_t *elem_mask = elementize_mask_multi(mask, loop_vars,
            logical_type, loc);

        // Build innermost body: if (.not. elem_mask) __gpu_all_res = .false.
        Vec<ASR::stmt_t*> if_body;
        if_body.reserve(al, 1);
        if_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var,
                ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                    false, logical_type)),
                nullptr, false, false)));
        Vec<ASR::stmt_t*> if_else;
        if_else.reserve(al, 0);
        ASR::expr_t *not_mask = ASRUtils::EXPR(
            ASR::make_LogicalNot_t(al, loc, elem_mask, logical_type, nullptr));
        ASR::stmt_t *inner_stmt = ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, not_mask,
                if_body.p, if_body.n, if_else.p, if_else.n));

        // Build nested loops from innermost dimension outward
        ASR::stmt_t *loop_nest = inner_stmt;
        for (int d = (int)dim_bounds.size() - 1; d >= 0; d--) {
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = loop_vars[d];
            head.m_start = dim_bounds[d].first;
            head.m_end = dim_bounds[d].second;
            head.m_increment = nullptr;
            Vec<ASR::stmt_t*> loop_body;
            loop_body.reserve(al, 1);
            loop_body.push_back(al, loop_nest);
            loop_nest = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                head, loop_body.p, loop_body.n, nullptr, 0));
        }
        preamble.push_back(al, loop_nest);

        return res_var;
    }

    // Check if an expression tree contains any IntrinsicArrayFunction All.
    bool contains_intrinsic_all(ASR::expr_t *e) {
        if (!e) return false;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) {
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::All) {
                return true;
            }
        }
        if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            return contains_intrinsic_all(lb->m_left) ||
                   contains_intrinsic_all(lb->m_right);
        }
        if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
            return contains_intrinsic_all(
                ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg);
        }
        return false;
    }

    // Recursively replace IntrinsicArrayFunction All nodes in an expression
    // with temporary variables, emitting inline loops into preamble.
    ASR::expr_t* replace_all_in_expr(ASR::expr_t *e, const Location &loc,
            Vec<ASR::stmt_t*> &preamble) {
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) {
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::All) {
                ASR::expr_t *res = inline_single_all(iaf, loc, preamble);
                if (res) return res;
            }
            return e;
        }
        if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            ASR::expr_t *new_left = replace_all_in_expr(lb->m_left, loc,
                preamble);
            ASR::expr_t *new_right = replace_all_in_expr(lb->m_right, loc,
                preamble);
            if (new_left != lb->m_left || new_right != lb->m_right) {
                return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                    new_left, lb->m_op, new_right, lb->m_type, nullptr));
            }
            return e;
        }
        if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
            ASR::LogicalNot_t *ln = ASR::down_cast<ASR::LogicalNot_t>(e);
            ASR::expr_t *new_arg = replace_all_in_expr(ln->m_arg, loc,
                preamble);
            if (new_arg != ln->m_arg) {
                return ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc,
                    new_arg, ln->m_type, nullptr));
            }
            return e;
        }
        return e;
    }

    // Inline IntrinsicArrayFunction All inside a DoConcurrentLoop body.
    // Replaces:
    //   eq(l) = all(a(:,l) == b(:,l))
    // or:
    //   eq(l) = all(a(1:l) > 0) .and. all(b(1:l) > 0)
    // With inlined loops that compute the All result into temporaries.
    // This avoids complex lowered code (Associate, Allocate, FunctionCall)
    // that the Metal backend cannot handle inside GPU kernels.
    // ---------------------------------------------------------------
    // Inlining device functions into the kernel body
    //
    // Metal shaders have neither variable-length arrays nor a heap, so
    // an `inline` device function cannot declare a local whose extent is
    // only known at run time. A kernel *can* have one: the extent is
    // evaluated on the host at launch and a device buffer is bound for
    // it (`analyze_gpu_vla_workspaces`). So instead of teaching the
    // device-function boundary to carry such a workspace through every
    // address-space overload, splice the callee's body into the loop
    // body: its locals become kernel-scope locals and the existing
    // workspace machinery applies unchanged, while its dummy arguments
    // are replaced by the actual arguments -- which often makes the
    // extent a compile-time constant outright.
    // ---------------------------------------------------------------

    // Functions whose bodies must be spliced into the loop body for this
    // loop to be offloadable. Filled by plan_device_function_inlining()
    // during the (non-destructive) eligibility decision and consumed by
    // inline_device_function_calls() afterwards.
    std::set<ASR::Function_t*> functions_to_inline;

    // The implementation already found for an interface declaration.
    // Loading a submodule from disk builds a fresh copy of its symbol
    // table every time, so without this the same `module procedure`
    // would resolve to a different Function_t at every call. The
    // inliner identifies a planned callee by pointer, so the plan and
    // the splice would then disagree and the callee would silently stay
    // an out-of-line device function.
    std::map<ASR::Function_t*, ASR::Function_t*> function_implementations;

    // A submodule `module procedure` reaches its callers through the
    // parent module's interface declaration, whose body is empty. Return
    // the Implementation function that actually carries the body,
    // loading the submodule from its `.smod` file when it is not already
    // part of this translation unit (--separate-compilation). Returns
    // `fn` unchanged when it already is an implementation, or when no
    // implementation can be found.
    ASR::Function_t* resolve_function_implementation(ASR::Function_t *fn) {
        if (!fn) return nullptr;
        ASR::FunctionType_t *fn_ft = ASR::down_cast<ASR::FunctionType_t>(
            fn->m_function_signature);
        if (fn_ft->m_deftype != ASR::deftypeType::Interface) return fn;
        auto cached = function_implementations.find(fn);
        if (cached != function_implementations.end()) return cached->second;
        ASR::Function_t *impl = find_function_implementation(fn);
        function_implementations[fn] = impl;
        return impl;
    }

    // The search behind resolve_function_implementation(), which caches
    // its result.
    ASR::Function_t* find_function_implementation(ASR::Function_t *fn) {
        std::string pname = fn->m_name;
        for (auto &tu_item : tu.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Module_t>(*tu_item.second)) continue;
            ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(
                tu_item.second);
            ASR::symbol_t *impl_sym = mod->m_symtab->get_symbol(pname);
            if (!impl_sym || !ASR::is_a<ASR::Function_t>(*impl_sym)) continue;
            ASR::Function_t *impl_func = ASR::down_cast<ASR::Function_t>(
                impl_sym);
            if (ASR::down_cast<ASR::FunctionType_t>(
                    impl_func->m_function_signature)->m_deftype ==
                    ASR::deftypeType::Implementation) {
                return impl_func;
            }
        }
        // Not in this translation unit: load the submodule from disk.
        SymbolTable *parent_st = fn->m_symtab->parent;
        if (!parent_st || !parent_st->asr_owner ||
                parent_st->asr_owner->type != ASR::asrType::symbol) {
            return fn;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            parent_st->asr_owner);
        if (!ASR::is_a<ASR::Module_t>(*owner)) return fn;
        std::string smod_prefix = std::string(
            ASR::down_cast<ASR::Module_t>(owner)->m_name) + "@";
        std::vector<std::filesystem::path> mod_dirs;
        mod_dirs.push_back(pass_options.runtime_library_dir);
        mod_dirs.push_back(pass_options.mod_files_dir);
        mod_dirs.insert(mod_dirs.end(), pass_options.include_dirs.begin(),
            pass_options.include_dirs.end());
        for (auto &dir : mod_dirs) {
            if (dir.empty()) dir = ".";
            if (!std::filesystem::is_directory(dir)) continue;
            for (auto &file : std::filesystem::directory_iterator(dir)) {
                std::string fname = file.path().filename().string();
                if (!startswith(fname, smod_prefix) ||
                        !endswith(fname, ".smod")) continue;
                std::string content;
                if (!read_file(file.path().string(), content) ||
                        content.empty()) continue;
                LocationManager lm_tmp;
                auto res = load_modfile(al, content, false, *tu.m_symtab,
                    lm_tmp);
                if (!res.ok) continue;
                load_submodule_deps(*res.result);
                fix_external_symbols(*res.result, *tu.m_symtab);
                lower_loaded_implied_do_loops(*res.result);
                ASR::Module_t *submod = ASRUtils::extract_module(
                    *res.result);
                ASR::symbol_t *impl_sym = submod->m_symtab->get_symbol(
                    pname);
                if (!impl_sym || !ASR::is_a<ASR::Function_t>(*impl_sym)) {
                    continue;
                }
                ASR::Function_t *impl_func = ASR::down_cast<ASR::Function_t>(
                    impl_sym);
                if (ASR::down_cast<ASR::FunctionType_t>(
                        impl_func->m_function_signature)->m_deftype !=
                        ASR::deftypeType::Implementation) continue;
                return impl_func;
            }
        }
        return fn;
    }

    // The function the inliner must reason about for a call: its
    // implementation, never the interface declaration that stands in for
    // a submodule `module procedure` at the call site. The interface has
    // an empty body, so reasoning about it would silently conclude that
    // the callee needs nothing.
    ASR::Function_t* resolve_device_function(ASR::symbol_t *sym) {
        if (!sym) return nullptr;
        ASR::symbol_t *r = ASRUtils::symbol_get_past_external(sym);
        if (!r) return nullptr;
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*r)) {
            r = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(r)->m_proc);
        }
        if (!r || !ASR::is_a<ASR::Function_t>(*r)) return nullptr;
        return resolve_function_implementation(
            ASR::down_cast<ASR::Function_t>(r));
    }

    // A `do concurrent` inside a procedure that device code can reach
    // has to stay an ordinary sequential loop. Offloading it rewrites it
    // into a host-side kernel launch, and a kernel launch has no meaning
    // inside a kernel: the device copy of that procedure is then emitted
    // with an empty body -- silently, because the GPU backends have no
    // lowering for a launch -- and the caller reads uninitialised
    // memory. The same holds for a loop already lifted into a kernel.
    // Both are sequentialized here, before this round rewrites anything,
    // so the decision is made on intact ASR.
    std::set<const ASR::DoConcurrentLoop_t*> host_only_loops;
    std::set<ASR::Function_t*> device_reachable_functions;

    void collect_host_only_loops() {
        GpuDoConcurrentCollector all_loops;
        all_loops.visit_TranslationUnit(tu);
        std::vector<ASR::Function_t*> pending;
        auto add_callees = [&](ASR::stmt_t **body, size_t n_body) {
            GpuFunctionCollector fc;
            for (size_t i = 0; i < n_body; i++) {
                fc.visit_stmt(*body[i]);
            }
            for (auto &item : fc.functions) {
                ASR::Function_t *callee = resolve_device_function(
                    item.second);
                if (callee &&
                        device_reachable_functions.insert(callee).second) {
                    pending.push_back(callee);
                }
            }
        };
        GpuDoConcurrentCollector blocked;
        // Owner of each blocked loop, for --gpu-offload-report only.
        std::map<const ASR::DoConcurrentLoop_t*, std::string> blocked_owner;
        auto note_owner = [&](const GpuDoConcurrentCollector &c,
                const std::string &owner) {
            if (!GpuOffloadReport::enabled) return;
            for (const ASR::DoConcurrentLoop_t *loop : c.loops) {
                blocked_owner.emplace(loop, owner);
            }
        };
        for (auto &item : tu.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::GpuKernelFunction_t>(*item.second)) continue;
            ASR::GpuKernelFunction_t *k =
                ASR::down_cast<ASR::GpuKernelFunction_t>(item.second);
            add_callees(k->m_body, k->n_body);
            GpuDoConcurrentCollector in_kernel;
            for (size_t i = 0; i < k->n_body; i++) {
                in_kernel.visit_stmt(*k->m_body[i]);
            }
            note_owner(in_kernel, item.first);
            blocked.loops.insert(in_kernel.loops.begin(),
                in_kernel.loops.end());
        }
        for (const ASR::DoConcurrentLoop_t *loop : all_loops.loops) {
            add_callees(loop->m_body, loop->n_body);
        }
        // A procedure stays device-reachable once its caller's loop has
        // been rewritten into a launch, so the set accumulates across
        // rounds rather than being rebuilt from the loops still present.
        for (ASR::Function_t *fn : device_reachable_functions) {
            pending.push_back(fn);
        }
        while (!pending.empty()) {
            ASR::Function_t *fn = pending.back();
            pending.pop_back();
            add_callees(fn->m_body, fn->n_body);
        }
        for (ASR::Function_t *fn : device_reachable_functions) {
            GpuDoConcurrentCollector in_fn;
            for (size_t i = 0; i < fn->n_body; i++) {
                in_fn.visit_stmt(*fn->m_body[i]);
            }
            note_owner(in_fn, fn->m_name);
            blocked.loops.insert(in_fn.loops.begin(), in_fn.loops.end());
        }
        for (const ASR::DoConcurrentLoop_t *loop : blocked.loops) {
            if (!host_only_loops.insert(loop).second) continue;
            auto owner = blocked_owner.find(loop);
            GpuOffloadReport::emit(loop->base.base.loc,
                owner == blocked_owner.end() ? "<unknown>" : owner->second,
                "sequentialized-for-device", true);
        }
        GpuHostOnlyLoopSequentializer seq(al, host_only_loops);
        seq.asr_changed = true;
        while (seq.asr_changed) {
            seq.asr_changed = false;
            seq.visit_TranslationUnit(tu);
        }
    }

    // Strip the physical-type casts wrapping an actual argument. The
    // uncast expression keeps its declared shape, which is what makes
    // `size(dummy,1)` fold to a constant after substitution.
    static ASR::expr_t* strip_array_casts(ASR::expr_t *e) {
        while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        }
        return e;
    }

    // The right-hand side of `target = f(...)` -- the only position a
    // call can be spliced from without inventing a temporary.
    static const ASR::FunctionCall_t* spliceable_call(ASR::stmt_t *stmt) {
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) return nullptr;
        ASR::expr_t *value = strip_array_casts(
            ASR::down_cast<ASR::Assignment_t>(stmt)->m_value);
        if (!value || !ASR::is_a<ASR::FunctionCall_t>(*value)) return nullptr;
        return ASR::down_cast<ASR::FunctionCall_t>(value);
    }

    // The BLOCK or ASSOCIATE construct `stmt` enters, or nullptr when
    // the statement does not enter one.
    static ASR::symbol_t* nested_scope_entered(ASR::stmt_t *stmt) {
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) return b;
        } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) return b;
        }
        return nullptr;
    }

    // The symbol table and body of a BLOCK or ASSOCIATE construct.
    static void nested_scope_contents(ASR::symbol_t *b, SymbolTable *&st,
            ASR::stmt_t **&body, size_t &n_body) {
        if (ASR::is_a<ASR::Block_t>(*b)) {
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            st = blk->m_symtab;
            body = blk->m_body;
            n_body = blk->n_body;
        } else {
            ASR::AssociateBlock_t *blk =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            st = blk->m_symtab;
            body = blk->m_body;
            n_body = blk->n_body;
        }
    }

    // Collect, in body order, the nested BLOCK and ASSOCIATE scopes that
    // the splice will flatten into the single kernel-level block.
    // Returns false when one is entered from a position the flattening
    // walk cannot rebuild -- inside an IF or a loop, where the ASR holds
    // a single statement rather than a statement list.
    static bool collect_flattened_scopes(ASR::stmt_t **stmts, size_t n,
            std::vector<ASR::symbol_t*> &scopes) {
        for (size_t i = 0; i < n; i++) {
            ASR::symbol_t *b = nested_scope_entered(stmts[i]);
            if (b) {
                scopes.push_back(b);
                SymbolTable *st = nullptr;
                ASR::stmt_t **body = nullptr;
                size_t n_body = 0;
                nested_scope_contents(b, st, body, n_body);
                if (!collect_flattened_scopes(body, n_body, scopes)) {
                    return false;
                }
                continue;
            }
            GpuNestedScopeCounter nc;
            nc.visit_stmt(*stmts[i]);
            if (nc.count > 0) return false;
        }
        return true;
    }

    // Can this callee's body be spliced verbatim into the caller?
    static bool can_inline_device_function(ASR::Function_t *fn,
            const ASR::FunctionCall_t *fc) {
        std::string callee = fn ? std::string(fn->m_name) : std::string("?");
        auto decline = [&](const char *sub) {
            GpuOffloadReport::set_detail(std::string("sub=") + sub
                + " callee=" + callee);
            return false;
        };
        if (!fn || !fn->m_return_var || fn->n_body == 0) {
            return decline("callee-no-return-var-or-empty-body");
        }
        ASR::FunctionType_t *ft = ASR::down_cast<ASR::FunctionType_t>(
            fn->m_function_signature);
        if (ft->m_abi != ASR::abiType::Source) {
            return decline("callee-abi-not-source");
        }
        if (ft->m_deftype != ASR::deftypeType::Implementation) {
            return decline("callee-not-implementation");
        }
        if (fn->n_args != fc->n_args) {
            return decline("callee-arg-count-mismatch");
        }
        for (size_t i = 0; i < fc->n_args; i++) {
            // An absent optional actual has no expression to substitute.
            if (!fc->m_args[i].m_value) {
                return decline("callee-absent-optional-actual");
            }
        }
        for (size_t i = 0; i < fn->n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                return decline("callee-dummy-not-var");
            }
        }
        std::vector<ASR::symbol_t*> nested;
        if (!collect_flattened_scopes(fn->m_body, fn->n_body, nested)) {
            return decline("callee-nested-scope-not-flattenable");
        }
        std::set<ASR::symbol_t*> flattened(nested.begin(), nested.end());
        // Every symbol the callee owns, in its own scope and in each
        // nested scope, must be something the splice can carry over.
        std::vector<SymbolTable*> scopes;
        scopes.push_back(fn->m_symtab);
        for (ASR::symbol_t *b : nested) {
            SymbolTable *st = nullptr;
            ASR::stmt_t **body = nullptr;
            size_t n_body = 0;
            nested_scope_contents(b, st, body, n_body);
            scopes.push_back(st);
        }
        for (SymbolTable *st : scopes) {
            for (auto &item : st->get_scope()) {
                // An ExternalSymbol only names an entity owned by another
                // module -- a derived-type component, a type, a
                // procedure. It resolves through that module from
                // wherever the cloned body ends up, so it needs no
                // re-homing.
                if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
                // A nested scope is re-homed by flattening its variables
                // and statements into the spliced block, but only when
                // the walk above reached it.
                if (ASR::is_a<ASR::Block_t>(*item.second) ||
                        ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                    if (!flattened.count(item.second)) {
                        GpuOffloadReport::set_detail(
                            "sub=callee-nested-scope-unreached callee="
                            + callee + " sym=" + item.first);
                        return false;
                    }
                    continue;
                }
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
                    GpuOffloadReport::set_detail(
                        "sub=callee-symtab-has-non-variable callee=" + callee
                        + " sym=" + item.first);
                    return false;
                }
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                // SAVE state must persist across calls; inlining would
                // give every call site its own copy.
                if (v->m_storage == ASR::storage_typeType::Save) {
                    GpuOffloadReport::set_detail(
                        "sub=callee-save-local callee=" + callee
                        + " sym=" + item.first);
                    return false;
                }
            }
        }
        // A `return` anywhere but as the final statement needs control
        // flow the splice cannot express. A `return` inside a nested
        // scope is one such place: flattening would drop it silently.
        GpuReturnCounter rc;
        for (size_t i = 0; i < fn->n_body; i++) {
            rc.visit_stmt(*fn->m_body[i]);
        }
        for (ASR::symbol_t *b : nested) {
            SymbolTable *st = nullptr;
            ASR::stmt_t **body = nullptr;
            size_t n_body = 0;
            nested_scope_contents(b, st, body, n_body);
            for (size_t i = 0; i < n_body; i++) {
                rc.visit_stmt(*body[i]);
            }
        }
        if (rc.count > 1) return decline("callee-multiple-returns");
        if (rc.count == 1 &&
                !ASR::is_a<ASR::Return_t>(*fn->m_body[fn->n_body - 1])) {
            return decline("callee-return-not-last");
        }
        return true;
    }

    // True when `fn` itself needs a run-time sized temporary, or reaches
    // a function that does. Memoized; `visiting` breaks call cycles.
    bool device_function_needs_inlining(ASR::Function_t *fn,
            std::map<ASR::Function_t*, bool> &memo,
            std::set<ASR::Function_t*> &visiting) {
        auto it = memo.find(fn);
        if (it != memo.end()) return it->second;
        if (visiting.count(fn)) return false;
        visiting.insert(fn);
        GpuDeviceFunctionArrayTempChecker checker;
        checker.check_function(fn);
        bool result = checker.has_runtime_sized_temp;
        if (!result) {
            GpuFunctionCollector fc;
            for (size_t i = 0; i < fn->n_body; i++) {
                fc.visit_stmt(*fn->m_body[i]);
            }
            for (auto &[name, sym] : fc.functions) {
                ASR::Function_t *callee = resolve_device_function(sym);
                if (callee && callee != fn &&
                        device_function_needs_inlining(callee, memo,
                            visiting)) {
                    result = true;
                    break;
                }
            }
        }
        visiting.erase(fn);
        memo[fn] = result;
        return result;
    }

    // Walk the statements that will become the kernel body and work out
    // which callees have to be spliced in. Purely analytical: nothing is
    // rewritten here, so the offload decision stays ahead of any
    // destructive change. Returns false when some callee that must be
    // inlined cannot be, in which case the loop is not offloaded.
    bool plan_device_function_inlining(ASR::stmt_t **stmts, size_t n_stmts,
            std::map<ASR::Function_t*, bool> &memo,
            std::set<ASR::Function_t*> &on_stack) {
        for (size_t si = 0; si < n_stmts; si++) {
            ASR::stmt_t *stmt = stmts[si];
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    if (!plan_device_function_inlining(blk->m_body,
                            blk->n_body, memo, on_stack)) return false;
                }
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    ASR::AssociateBlock_t *blk =
                        ASR::down_cast<ASR::AssociateBlock_t>(b);
                    if (!plan_device_function_inlining(blk->m_body,
                            blk->n_body, memo, on_stack)) return false;
                }
                continue;
            }
            const ASR::FunctionCall_t *top = spliceable_call(stmt);
            GpuCallSiteCollector csc;
            csc.visit_stmt(*stmt);
            for (const ASR::FunctionCall_t *call : csc.calls) {
                ASR::Function_t *callee = resolve_device_function(
                    call->m_name);
                if (!callee) continue;
                if (!device_function_needs_inlining(callee, memo,
                        on_stack)) continue;
                // Only a call that *is* the assignment's value can be
                // spliced; one nested inside a larger expression would
                // need a temporary the caller does not have.
                if (call != top) {
                    GpuOffloadReport::set_detail(
                        "sub=call-not-top-level-value callee="
                        + std::string(callee->m_name));
                    return false;
                }
                if (on_stack.count(callee)) {
                    GpuOffloadReport::set_detail("sub=callee-recursive callee="
                        + std::string(callee->m_name));
                    return false;
                }
                if (!can_inline_device_function(callee, call)) return false;
                functions_to_inline.insert(callee);
                on_stack.insert(callee);
                bool ok = plan_device_function_inlining(callee->m_body,
                    callee->n_body, memo, on_stack);
                on_stack.erase(callee);
                if (!ok) return false;
            }
        }
        return true;
    }

    // Rewrite the dimension expressions of a cloned local's type through
    // `subst`, so an extent written in terms of the callee's dummies is
    // expressed in terms of the actual arguments instead.
    void substitute_in_type(ASR::ttype_t *t,
            std::map<ASR::symbol_t*, ASR::expr_t*> &subst) {
        if (!t) return;
        ASR::ttype_t *bare = ASRUtils::type_get_past_allocatable_pointer(t);
        if (!bare || !ASR::is_a<ASR::Array_t>(*bare)) return;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(bare);
        AssociateVarResolver resolver(al, subst);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_start) {
                resolver.current_expr = &(arr->m_dims[d].m_start);
                resolver.replace_expr(arr->m_dims[d].m_start);
            }
            if (arr->m_dims[d].m_length) {
                resolver.current_expr = &(arr->m_dims[d].m_length);
                resolver.replace_expr(arr->m_dims[d].m_length);
            }
        }
    }

    // Clone `stmts` into `out`, flattening every BLOCK and ASSOCIATE it
    // enters into the same statement list. An ASSOCIATE construct opens
    // its body with plain assignments that define its associate names,
    // so once those names are cloned as ordinary locals of the spliced
    // block the body needs no further rewriting.
    bool flatten_device_function_body(ASR::stmt_t **stmts, size_t n_stmts,
            ASRUtils::ExprStmtDuplicator &dup, Vec<ASR::stmt_t*> &out) {
        for (size_t i = 0; i < n_stmts; i++) {
            ASR::symbol_t *b = nested_scope_entered(stmts[i]);
            if (b) {
                SymbolTable *st = nullptr;
                ASR::stmt_t **body = nullptr;
                size_t n_body = 0;
                nested_scope_contents(b, st, body, n_body);
                if (!flatten_device_function_body(body, n_body, dup, out)) {
                    return false;
                }
                continue;
            }
            if (ASR::is_a<ASR::Return_t>(*stmts[i])) continue;
            dup.success = true;
            ASR::stmt_t *c = dup.duplicate_stmt(stmts[i]);
            if (!c || !dup.success) return false;
            out.push_back(al, c);
        }
        return true;
    }

    // Copy a sectioned actual argument into a contiguous array owned by
    // the spliced block, and return that array; nullptr when the actual is
    // not a section, when the callee never sections the dummy, or when the
    // section is not one the copy loops can walk.
    //
    // The callee's own sections of the dummy are what force this: splicing
    // substitutes the actual for the dummy, so `c(1:k)` over an actual
    // `a(:,j)` becomes a section of a section.  A device pointer carries a
    // base and a count and nothing else, so the inner section's stride
    // would simply be dropped.  Copying first leaves the dummy standing for
    // an ordinary contiguous array, which the callee may section freely.
    //
    // The array lives in the spliced block, so kernel extraction gives it a
    // per-thread workspace buffer rather than one buffer shared by every
    // thread; an extent the host cannot work out is caught by the workspace
    // pre-flight, which declines the loop.
    ASR::expr_t* gather_section_actual(const Location &loc,
            SymbolTable *block_scope, ASR::Function_t *fn,
            ASR::symbol_t *dummy, ASR::expr_t *actual, bool writable,
            std::vector<ASR::stmt_t*> &gathers,
            std::vector<ASR::stmt_t*> &scatters) {
        if (!actual || !ASR::is_a<ASR::ArraySection_t>(*actual)) {
            return nullptr;
        }
        ASR::ArraySection_t *as =
            ASR::down_cast<ASR::ArraySection_t>(actual);
        // The copy loops index the base as a designator, so it has to be
        // one they can write down.
        if (!ASR::is_a<ASR::Var_t>(*as->m_v)
                && !ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
            return nullptr;
        }
        std::vector<int> range_dims;
        for (size_t d = 0; d < as->n_args; d++) {
            if (as->m_args[d].m_left && as->m_args[d].m_right
                    && as->m_args[d].m_step) {
                range_dims.push_back((int)d);
            }
        }
        if (range_dims.empty()) return nullptr;
        GpuDummySectionFinder finder(dummy);
        for (size_t i = 0; i < fn->n_body; i++) {
            finder.visit_stmt(*fn->m_body[i]);
        }
        if (!finder.found) return nullptr;
        // Gathering is what lets this callee be spliced at all, so it is
        // also where the shader has to be judged buildable. A callee that
        // holds an implied-do reaches the Metal code generator with no
        // rendering for it, and the driver is handed a shader that will
        // not compile -- worse than leaving the loop on the host. Declining
        // to gather leaves the nested section standing, and the loop is
        // then declined further down exactly as before.
        GpuImpliedDoFinder implied_do;
        for (size_t i = 0; i < fn->n_body; i++) {
            implied_do.visit_stmt(*fn->m_body[i]);
        }
        if (implied_do.found) return nullptr;

        Vec<ASR::expr_t*> extents;
        extents.reserve(al, range_dims.size());
        for (int d : range_dims) {
            extents.push_back(al, section_extent(loc, as->m_args[d]));
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(as->m_v));
        ASR::expr_t *tmp = declare_temp_array(loc, block_scope, elem_type,
            extents.p, extents.n, "__gpu_arg");
        gathers.push_back(build_section_copy_loops(loc, block_scope, as,
            range_dims, tmp, true));
        if (writable) {
            scatters.push_back(build_section_copy_loops(loc, block_scope,
                as, range_dims, tmp, false));
        }
        return tmp;
    }

    // Splice `fn`'s body into a BLOCK, rewritten for this call site,
    // and assign its result to `target` inside that block.
    //
    // The BLOCK is what makes this work: the callee's locals land in the
    // block's own symbol table, so after kernel extraction they are
    // block-scope locals of the kernel -- exactly where
    // analyze_gpu_vla_workspaces() looks for run-time sized arrays and
    // binds a device buffer for each. Putting them in the enclosing
    // scope instead would make them kernel *arguments*, and an ALLOCATE
    // of a kernel argument is not valid ASR.
    //
    // That machinery only inspects the symbol table of a top-level
    // block, so the callee's own nested BLOCK and ASSOCIATE scopes are
    // flattened into this one block rather than rebuilt inside it: a
    // run-time sized temporary left one level down would be invisible to
    // it and reach the shader as a variable-length array.
    ASR::stmt_t* splice_device_function(ASR::Function_t *fn,
            const ASR::FunctionCall_t *fc, ASR::expr_t *target,
            const Location &loc) {
        SymbolTable *block_scope = al.make_new<SymbolTable>(current_scope);

        std::map<ASR::symbol_t*, ASR::expr_t*> subst;
        std::set<ASR::symbol_t*> dummies;
        std::vector<ASR::stmt_t*> arg_gathers, arg_scatters;
        for (size_t i = 0; i < fn->n_args; i++) {
            ASR::symbol_t *d = ASR::down_cast<ASR::Var_t>(
                fn->m_args[i])->m_v;
            ASR::expr_t *actual = strip_array_casts(fc->m_args[i].m_value);
            if (!actual) return nullptr;
            // A sectioned actual whose dummy the callee sections in turn
            // would leave a section of a section behind, which no device
            // pointer can express. Copy it into a contiguous array of the
            // spliced block first and let the dummy stand for that.
            ASR::expr_t *gathered = gather_section_actual(loc, block_scope,
                fn, d, actual, dummy_is_written(fn, i), arg_gathers,
                arg_scatters);
            subst[d] = gathered ? gathered : actual;
            dummies.insert(d);
        }

        // The callee's own scope plus every nested BLOCK and ASSOCIATE
        // scope, all of which are flattened into this one block.
        std::vector<ASR::symbol_t*> nested;
        if (!collect_flattened_scopes(fn->m_body, fn->n_body, nested)) {
            return nullptr;
        }
        std::vector<SymbolTable*> scopes;
        scopes.push_back(fn->m_symtab);
        for (ASR::symbol_t *b : nested) {
            SymbolTable *st = nullptr;
            ASR::stmt_t **body = nullptr;
            size_t n_body = 0;
            nested_scope_contents(b, st, body, n_body);
            scopes.push_back(st);
        }

        // Clone the callee's locals (its result variable included) into
        // the block. Two phases, so that an extent written in terms of
        // another local is substituted too.
        ASR::symbol_t *ret_sym = ASR::down_cast<ASR::Var_t>(
            fn->m_return_var)->m_v;
        std::vector<ASR::symbol_t*> cloned_locals;
        for (SymbolTable *st : scopes) {
            for (auto &item : st->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (dummies.count(sym)) continue;
                // ExternalSymbols keep resolving through their owning
                // module; the cloned body may reference them as they are.
                if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) continue;
                // The nested scopes themselves are dissolved by the
                // flattening, so nothing stands in for them here.
                if (ASR::is_a<ASR::Block_t>(*sym) ||
                        ASR::is_a<ASR::AssociateBlock_t>(*sym)) continue;
                if (!ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                // A named constant carries its value on the declaration
                // rather than at every reference, so a clone that drops
                // it leaves the name standing for nothing -- neither the
                // shape resolver nor the backend can say what it is. A
                // value that is not a self-contained constant would name
                // the callee's own symbols, so only a folded one is
                // carried over.
                ASR::expr_t *param_value = nullptr;
                if (v->m_storage == ASR::storage_typeType::Parameter &&
                        v->m_value != nullptr &&
                        ASRUtils::is_value_constant(v->m_value)) {
                    ASRUtils::ExprStmtDuplicator value_dup(al);
                    param_value = value_dup.duplicate_expr(v->m_value);
                }
                std::string name = block_scope->get_unique_name(v->m_name);
                ASR::symbol_t *ns = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, block_scope,
                        s2c(al, name), nullptr, 0, ASR::intentType::Local,
                        param_value, param_value, v->m_storage,
                        ASRUtils::duplicate_type(al, v->m_type),
                        v->m_type_declaration, ASR::abiType::Source,
                        ASR::accessType::Public, ASR::presenceType::Required,
                        false));
                block_scope->add_symbol(name, ns);
                subst[sym] = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ns));
                cloned_locals.push_back(ns);
            }
        }
        for (ASR::symbol_t *ns : cloned_locals) {
            substitute_in_type(
                ASR::down_cast<ASR::Variable_t>(ns)->m_type, subst);
        }

        ASRUtils::ExprStmtDuplicator dup(al);
        Vec<ASR::stmt_t*> cloned;
        cloned.reserve(al, fn->n_body + arg_gathers.size()
            + arg_scatters.size() + 1);
        // The gathers read the caller's own expressions, so they are not
        // subject to the dummy substitution and go in ahead of it.
        for (ASR::stmt_t *g : arg_gathers) cloned.push_back(al, g);
        size_t body_start = cloned.n;
        if (!flatten_device_function_body(fn->m_body, fn->n_body, dup,
                cloned)) {
            return nullptr;
        }
        AssociateVarResolverVisitor resolver(al, subst);
        for (size_t i = body_start; i < cloned.n; i++) {
            resolver.visit_stmt(*cloned[i]);
        }
        for (ASR::stmt_t *sc : arg_scatters) cloned.push_back(al, sc);

        auto rit = subst.find(ret_sym);
        if (rit == subst.end()) return nullptr;
        cloned.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
            al, loc, target, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                ASR::down_cast<ASR::Var_t>(rit->second)->m_v)),
            nullptr, false, false)));

        std::string block_name = current_scope->get_unique_name(
            "__gpu_inl_" + std::string(fn->m_name));
        ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
            s2c(al, block_name), cloned.p, cloned.n);
        block_scope->asr_owner = block;
        ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
        current_scope->add_symbol(block_name, block_sym);
        return ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1,
            block_sym));
    }

    // Splice every planned callee into `stmts`, repeating until no call
    // is left to inline (a callee's own calls surface only once its body
    // has been spliced in). The planner has already proved this
    // terminates: it rejects any call cycle.
    void inline_device_function_calls(ASR::stmt_t **&stmts,
            size_t &n_stmts) {
        if (functions_to_inline.empty()) return;
        for (size_t round = 0; round < functions_to_inline.size() + 1;
                round++) {
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, n_stmts * 4);
            bool changed = false;
            for (size_t si = 0; si < n_stmts; si++) {
                ASR::stmt_t *stmt = stmts[si];
                if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                    ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                    if (b && ASR::is_a<ASR::Block_t>(*b)) {
                        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                        inline_device_function_calls(blk->m_body,
                            blk->n_body);
                    }
                    new_body.push_back(al, stmt);
                    continue;
                }
                const ASR::FunctionCall_t *call = spliceable_call(stmt);
                ASR::Function_t *callee = call
                    ? resolve_device_function(call->m_name) : nullptr;
                if (!callee || !functions_to_inline.count(callee)) {
                    new_body.push_back(al, stmt);
                    continue;
                }
                ASR::Assignment_t *asgn =
                    ASR::down_cast<ASR::Assignment_t>(stmt);
                ASR::stmt_t *spliced = splice_device_function(callee,
                    call, asgn->m_target, stmt->base.loc);
                if (!spliced) {
                    new_body.push_back(al, stmt);
                    continue;
                }
                new_body.push_back(al, spliced);
                changed = true;
            }
            if (!changed) break;
            stmts = new_body.p;
            n_stmts = new_body.n;
        }
    }

    void inline_intrinsic_all(ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body * 3);
        bool changed = false;

        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

            if (!contains_intrinsic_all(asgn->m_value)) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            Vec<ASR::stmt_t*> preamble;
            preamble.reserve(al, 8);

            ASR::expr_t *new_value = replace_all_in_expr(asgn->m_value,
                loc, preamble);

            if (preamble.n > 0) {
                changed = true;
                for (size_t pi = 0; pi < preamble.n; pi++) {
                    new_body.push_back(al, preamble[pi]);
                }
                new_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, asgn->m_target,
                        new_value, nullptr, false, false)));
                // Track non-array scalar targets as reduction liveouts
                if (ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                    ASR::ttype_t *tgt_type =
                        ASRUtils::expr_type(asgn->m_target);
                    if (!ASRUtils::is_array(tgt_type)) {
                        all_reduction_targets.insert(
                            ASRUtils::symbol_name(
                                ASR::down_cast<ASR::Var_t>(
                                    asgn->m_target)->m_v));
                    }
                }
            } else {
                new_body.push_back(al, stmt);
            }
        }

        if (changed) {
            x.m_body = new_body.p;
            x.n_body = new_body.n;
        }
    }

    // Inline IntrinsicArrayFunction MatMul inside a DoConcurrentLoop body.
    // Replaces:
    //   c = matmul(a, b)
    // With nested DoLoops that compute the matrix multiplication directly.
    // This avoids generating a call to _lcompilers_matmul which is not
    // available inside Metal GPU kernels.
    // The MatMul shapes `inline_matmul_stmts` lowers on an Assignment:
    // the matmul is either the whole right-hand side, or a direct operand
    // of a RealBinOp on the right-hand side (`z = matmul(w, a) + b`). On a
    // match of the second shape the other operand, the operator and the
    // side of the matmul are reported back to the caller.
    ASR::IntrinsicArrayFunction_t* match_statement_matmul(
            ASR::expr_t *value, ASR::expr_t *&binop_other,
            ASR::binopType &binop_op, bool &matmul_is_left) {
        auto is_matmul = [](ASR::expr_t *e) -> ASR::IntrinsicArrayFunction_t* {
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) return nullptr;
            auto *f = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    f->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::MatMul) {
                return nullptr;
            }
            return f;
        };
        if (!value) return nullptr;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*value)) {
            return is_matmul(value);
        }
        if (!ASR::is_a<ASR::RealBinOp_t>(*value)) return nullptr;
        ASR::RealBinOp_t *rbop = ASR::down_cast<ASR::RealBinOp_t>(value);
        ASR::expr_t *left = rbop->m_left;
        ASR::expr_t *right = rbop->m_right;
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*left))
            left = ASR::down_cast<ASR::ArrayPhysicalCast_t>(left)->m_arg;
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*right))
            right = ASR::down_cast<ASR::ArrayPhysicalCast_t>(right)->m_arg;
        if (auto *f = is_matmul(left)) {
            binop_other = rbop->m_right;
            binop_op = rbop->m_op;
            matmul_is_left = true;
            return f;
        }
        if (auto *f = is_matmul(right)) {
            binop_other = rbop->m_left;
            binop_op = rbop->m_op;
            matmul_is_left = false;
            return f;
        }
        return nullptr;
    }

    void inline_matmul_stmts(ASR::stmt_t** &body, size_t &n_body) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 4);
        bool changed = false;

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t &dl = *ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_matmul_stmts(dl.m_body, dl.n_body);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t &bc = *ASR::down_cast<ASR::BlockCall_t>(stmt);
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc.m_m);
                inline_matmul_stmts(block->m_body, block->n_body);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                inline_matmul_stmts(ab->m_body, ab->n_body);
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

            ASR::expr_t *binop_other = nullptr;
            ASR::binopType binop_op = ASR::binopType::Add;
            bool matmul_is_left = true;
            ASR::IntrinsicArrayFunction_t *iaf = match_statement_matmul(
                asgn->m_value, binop_other, binop_op, matmul_is_left);
            if (!iaf) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            // Strip ArrayPhysicalCast from arguments
            ASR::expr_t *arg_a = iaf->m_args[0];
            ASR::expr_t *arg_b = iaf->m_args[1];
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_a)) {
                arg_a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_a)->m_arg;
            }
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_b)) {
                arg_b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_b)->m_arg;
            }

            // Detect and unwrap Transpose on matmul arguments so the
            // inlined loops index into the original array with swapped
            // indices instead of calling _lcompilers_transpose (which
            // is unavailable inside Metal GPU kernels).
            bool transpose_a = false, transpose_b = false;
            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg_a)) {
                auto *iaf_a = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(arg_a);
                if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                        iaf_a->m_arr_intrinsic_id)
                            == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                    arg_a = iaf_a->m_args[0];
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_a)) {
                        arg_a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_a)->m_arg;
                    }
                    transpose_a = true;
                }
            }
            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg_b)) {
                auto *iaf_b = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(arg_b);
                if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                        iaf_b->m_arr_intrinsic_id)
                            == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                    arg_b = iaf_b->m_args[0];
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_b)) {
                        arg_b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_b)->m_arg;
                    }
                    transpose_b = true;
                }
            }

            ASR::ttype_t *type_a = ASRUtils::expr_type(arg_a);
            ASR::ttype_t *type_b = ASRUtils::expr_type(arg_b);
            ASR::dimension_t *dims_a = nullptr, *dims_b = nullptr;
            int rank_a = ASRUtils::extract_dimensions_from_ttype(type_a, dims_a);
            int rank_b = ASRUtils::extract_dimensions_from_ttype(type_b, dims_b);

            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(asgn->m_target));
            ASR::dimension_t *dims_c = nullptr;
            ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(asgn->m_target), dims_c);

            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_loop_var = [&](const std::string &prefix) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            auto make_array_item_1d = [&](ASR::expr_t *arr,
                    ASR::expr_t *idx) -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 1);
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_right = idx;
                ai.m_step = nullptr;
                args.push_back(al, ai);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                    args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            };

            auto make_array_item_2d = [&](ASR::expr_t *arr,
                    ASR::expr_t *idx1, ASR::expr_t *idx2) -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 2);
                ASR::array_index_t ai1;
                ai1.loc = loc;
                ai1.m_left = nullptr;
                ai1.m_right = idx1;
                ai1.m_step = nullptr;
                args.push_back(al, ai1);
                ASR::array_index_t ai2;
                ai2.loc = loc;
                ai2.m_left = nullptr;
                ai2.m_right = idx2;
                ai2.m_step = nullptr;
                args.push_back(al, ai2);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                    args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            };

            auto make_do_loop = [&](ASR::expr_t *var, ASR::expr_t *start,
                    ASR::expr_t *end, Vec<ASR::stmt_t*> &body) -> ASR::stmt_t* {
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = var;
                head.m_start = start;
                head.m_end = end;
                head.m_increment = nullptr;
                return ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                    head, body.p, body.n, nullptr, 0));
            };

            // When an argument is an ArraySection, extract loop bounds
            // from the section's range specs rather than from the type
            // dimensions (which may be null for section result types).
            auto get_loop_bounds = [&](ASR::expr_t *arg,
                    ASR::dimension_t *dims,
                    int dim_idx) -> std::pair<ASR::expr_t*, ASR::expr_t*> {
                if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
                    ASR::ArraySection_t *sec =
                        ASR::down_cast<ASR::ArraySection_t>(arg);
                    int range_idx = 0;
                    for (size_t d = 0; d < sec->n_args; d++) {
                        if (sec->m_args[d].m_left != nullptr) {
                            if (range_idx == dim_idx) {
                                return {sec->m_args[d].m_left,
                                        sec->m_args[d].m_right};
                            }
                            range_idx++;
                        }
                    }
                }
                return get_dim_bounds(al, arg->base.loc, dims,
                    (size_t)dim_idx, arg);
            };

            // matmul pairs its operands by position: the k-th column of
            // `a` multiplies the k-th element of `b` whatever lower bound
            // either operand declares and wherever an operand that is an
            // array section starts inside its parent array.  The loop
            // variables run over the index space of one chosen operand,
            // so a variable used to index a different operand is first
            // rebased onto that operand's own first index.  When both
            // spaces are known to start at the same index the variable is
            // used as it is, which leaves the usual lower-bound-of-one
            // case exactly as it was.
            auto rebase_index = [&](ASR::expr_t *operand,
                    ASR::dimension_t *operand_dims, int dim_idx,
                    ASR::expr_t *var,
                    ASR::expr_t *ref_start) -> ASR::expr_t* {
                ASR::expr_t *start = get_loop_bounds(operand, operand_dims,
                    dim_idx).first;
                if (start == nullptr || ref_start == nullptr) return var;
                if (start == ref_start) return var;
                if (is_int_literal(start, 1) && is_int_literal(ref_start, 1))
                    return var;
                ASR::expr_t *offset = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc, var,
                        ASR::binopType::Sub, to_int32(loc, ref_start),
                        int_type, nullptr));
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    to_int32(loc, start), ASR::binopType::Add, offset,
                    int_type, nullptr));
            };

            // When an argument or target is an ArraySection (e.g. v(:,i)),
            // expand it into an ArrayItem on the base array by replacing
            // each range dimension with the corresponding loop variable
            // and keeping fixed dimensions as-is.
            // When the expression is an elemental FunctionCall with array
            // arguments (e.g. f(z(1:n))), elementize by converting each
            // array argument to a scalar indexed by the loop variable,
            // producing f(z(i)) instead of f(z(1:n))[i].
            std::function<ASR::expr_t*(ASR::expr_t*,
                std::vector<ASR::expr_t*>)> make_section_item;
            make_section_item = [&](ASR::expr_t *arr_expr,
                    std::vector<ASR::expr_t*> loop_vars) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*arr_expr)) {
                    ASR::ArraySection_t *sec =
                        ASR::down_cast<ASR::ArraySection_t>(arr_expr);
                    Vec<ASR::array_index_t> args;
                    args.reserve(al, sec->n_args);
                    size_t lv_idx = 0;
                    for (size_t d = 0; d < sec->n_args; d++) {
                        ASR::array_index_t ai;
                        ai.loc = loc;
                        if (sec->m_args[d].m_left != nullptr) {
                            ai.m_left = nullptr;
                            ai.m_right = loop_vars[lv_idx++];
                            ai.m_step = nullptr;
                        } else {
                            ai.m_left = nullptr;
                            ai.m_right = sec->m_args[d].m_right;
                            ai.m_step = nullptr;
                        }
                        args.push_back(al, ai);
                    }
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        sec->m_v, args.p, args.n, elem_type,
                        ASR::arraystorageType::ColMajor, nullptr));
                }
                if (ASR::is_a<ASR::FunctionCall_t>(*arr_expr)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(arr_expr);
                    if (ASRUtils::is_elemental(fc->m_name)) {
                        Vec<ASR::call_arg_t> new_args;
                        new_args.reserve(al, fc->n_args);
                        for (size_t i = 0; i < fc->n_args; i++) {
                            ASR::call_arg_t arg;
                            arg.loc = fc->m_args[i].loc;
                            if (fc->m_args[i].m_value &&
                                    ASRUtils::is_array(
                                        ASRUtils::expr_type(
                                            fc->m_args[i].m_value))) {
                                arg.m_value = make_section_item(
                                    fc->m_args[i].m_value, loop_vars);
                            } else {
                                arg.m_value = fc->m_args[i].m_value;
                            }
                            new_args.push_back(al, arg);
                        }
                        ASR::ttype_t *ret_type = elem_type;
                        return ASRUtils::EXPR(
                            ASR::make_FunctionCall_t(al, fc->base.base.loc,
                                fc->m_name, fc->m_original_name,
                                new_args.p, new_args.n, ret_type,
                                nullptr, fc->m_dt));
                    }
                }
                if (loop_vars.size() == 1)
                    return make_array_item_1d(arr_expr, loop_vars[0]);
                return make_array_item_2d(arr_expr, loop_vars[0],
                    loop_vars[1]);
            };

            // The other operand of `z = matmul(w, a) <op> b` is combined
            // with the matmul result element by element. A scalar operand
            // is the same for every element, so it is used as it is; only
            // an array operand is indexed by the loop variables. A scalar
            // reaches here wrapped in an ArrayBroadcast, whose type is an
            // array, so the wrapper is stripped before the rank is
            // checked.
            auto make_binop_other_item = [&](ASR::expr_t *other,
                    std::vector<ASR::expr_t*> loop_vars,
                    std::vector<ASR::expr_t*> ref_starts) -> ASR::expr_t* {
                while (true) {
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*other)) {
                        other = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            other)->m_arg;
                    } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*other)) {
                        other = ASR::down_cast<ASR::ArrayBroadcast_t>(
                            other)->m_array;
                    } else {
                        break;
                    }
                }
                if (!ASRUtils::is_array(ASRUtils::expr_type(other)))
                    return other;
                ASR::dimension_t *other_dims = nullptr;
                ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::expr_type(other), other_dims);
                std::vector<ASR::expr_t*> idx;
                for (size_t d = 0; d < loop_vars.size(); d++) {
                    idx.push_back(rebase_index(other, other_dims, (int)d,
                        loop_vars[d], ref_starts[d]));
                }
                return make_section_item(other, idx);
            };

            ASR::expr_t *zero;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                    0.0, elem_type));
            } else {
                zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                    0, elem_type, ASR::integerbozType::Decimal));
            }

            int64_t overload_id = iaf->m_overload_id;

            if (overload_id == 2 && rank_a == 2 && rank_b == 1) {
                // c(i) = sum_k a(i,k) * b(k)
                // With transpose_a: c(i) = sum_k a(k,i) * b(k)
                ASR::expr_t *var_i = make_loop_var("__gpu_mm_i");
                ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

                int i_dim = transpose_a ? 1 : 0;
                int k_dim = transpose_a ? 0 : 1;
                auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, k_dim);
                auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, i_dim);

                ASR::expr_t *c_i = make_section_item(asgn->m_target,
                    {rebase_index(asgn->m_target, dims_c, 0, var_i,
                        i_start)});
                ASR::expr_t *a_ik = transpose_a
                    ? make_section_item(arg_a, {var_k, var_i})
                    : make_section_item(arg_a, {var_i, var_k});
                ASR::expr_t *b_k = make_section_item(arg_b,
                    {rebase_index(arg_b, dims_b, 0, var_k, k_start)});

                // k-loop body: c(i) = c(i) + a(i,k) * b(k)
                Vec<ASR::stmt_t*> k_body;
                k_body.reserve(al, 1);
                ASR::expr_t *prod = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, a_ik,
                        ASR::binopType::Mul, b_k, elem_type, nullptr));
                ASR::expr_t *sum = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, c_i,
                        ASR::binopType::Add, prod, elem_type, nullptr));
                k_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_i, sum,
                        nullptr, false, false)));

                // i-loop body: c(i) = 0; do k ...; [c(i) = c(i) OP other(i)]
                Vec<ASR::stmt_t*> i_body;
                i_body.reserve(al, binop_other ? 3 : 2);
                i_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_i, zero,
                        nullptr, false, false)));
                i_body.push_back(al,
                    make_do_loop(var_k, k_start, k_end, k_body));

                if (binop_other) {
                    ASR::expr_t *other_i = make_binop_other_item(
                        binop_other, {var_i}, {i_start});
                    ASR::expr_t *lhs = matmul_is_left ? c_i : other_i;
                    ASR::expr_t *rhs = matmul_is_left ? other_i : c_i;
                    ASR::expr_t *combined = ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc, lhs, binop_op,
                            rhs, elem_type, nullptr));
                    i_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, c_i, combined,
                            nullptr, false, false)));
                }

                new_body.push_back(al,
                    make_do_loop(var_i, i_start, i_end, i_body));
            } else if (overload_id == 1 && rank_a == 1 && rank_b == 2) {
                // c(j) = sum_k a(k) * b(k, j)
                // With transpose_b: c(j) = sum_k a(k) * b(j, k)
                ASR::expr_t *var_j = make_loop_var("__gpu_mm_j");
                ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

                int k_dim = transpose_b ? 1 : 0;
                int j_dim = transpose_b ? 0 : 1;
                auto [k_start, k_end] = get_loop_bounds(arg_b, dims_b, k_dim);
                auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, j_dim);

                ASR::expr_t *c_j = make_section_item(asgn->m_target,
                    {rebase_index(asgn->m_target, dims_c, 0, var_j,
                        j_start)});
                ASR::expr_t *a_k = make_section_item(arg_a,
                    {rebase_index(arg_a, dims_a, 0, var_k, k_start)});
                ASR::expr_t *b_kj = transpose_b
                    ? make_section_item(arg_b, {var_j, var_k})
                    : make_section_item(arg_b, {var_k, var_j});

                Vec<ASR::stmt_t*> k_body;
                k_body.reserve(al, 1);
                ASR::expr_t *prod = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, a_k,
                        ASR::binopType::Mul, b_kj, elem_type, nullptr));
                ASR::expr_t *sum = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, c_j,
                        ASR::binopType::Add, prod, elem_type, nullptr));
                k_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_j, sum,
                        nullptr, false, false)));

                Vec<ASR::stmt_t*> j_body;
                j_body.reserve(al, binop_other ? 3 : 2);
                j_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_j, zero,
                        nullptr, false, false)));
                j_body.push_back(al,
                    make_do_loop(var_k, k_start, k_end, k_body));

                if (binop_other) {
                    ASR::expr_t *other_j = make_binop_other_item(
                        binop_other, {var_j}, {j_start});
                    ASR::expr_t *lhs = matmul_is_left ? c_j : other_j;
                    ASR::expr_t *rhs = matmul_is_left ? other_j : c_j;
                    ASR::expr_t *combined = ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc, lhs, binop_op,
                            rhs, elem_type, nullptr));
                    j_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, c_j, combined,
                            nullptr, false, false)));
                }

                new_body.push_back(al,
                    make_do_loop(var_j, j_start, j_end, j_body));
            } else if (overload_id == 3 && rank_a == 2 && rank_b == 2) {
                // c(i,j) = sum_k a(i,k) * b(k,j)
                // With transpose_a: a(i,k) becomes a(k,i)
                // With transpose_b: b(k,j) becomes b(j,k)
                ASR::expr_t *var_i = make_loop_var("__gpu_mm_i");
                ASR::expr_t *var_j = make_loop_var("__gpu_mm_j");
                ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

                int a_k_dim = transpose_a ? 0 : 1;
                int a_i_dim = transpose_a ? 1 : 0;
                int b_j_dim = transpose_b ? 0 : 1;
                int b_k_dim = transpose_b ? 1 : 0;
                auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, a_k_dim);
                auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, b_j_dim);
                auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, a_i_dim);

                // `k` runs over `a`'s contraction dimension, so only
                // `b`'s copy of it is rebased; `j` already runs over
                // `b`'s own dimension.
                ASR::expr_t *var_k_b = rebase_index(arg_b, dims_b, b_k_dim,
                    var_k, k_start);
                ASR::expr_t *c_ij = make_section_item(asgn->m_target,
                    {rebase_index(asgn->m_target, dims_c, 0, var_i, i_start),
                     rebase_index(asgn->m_target, dims_c, 1, var_j, j_start)});
                ASR::expr_t *a_ik = transpose_a
                    ? make_section_item(arg_a, {var_k, var_i})
                    : make_section_item(arg_a, {var_i, var_k});
                ASR::expr_t *b_kj = transpose_b
                    ? make_section_item(arg_b, {var_j, var_k_b})
                    : make_section_item(arg_b, {var_k_b, var_j});

                Vec<ASR::stmt_t*> k_body;
                k_body.reserve(al, 1);
                ASR::expr_t *prod = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, a_ik,
                        ASR::binopType::Mul, b_kj, elem_type, nullptr));
                ASR::expr_t *sum = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, c_ij,
                        ASR::binopType::Add, prod, elem_type, nullptr));
                k_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_ij, sum,
                        nullptr, false, false)));

                Vec<ASR::stmt_t*> j_body;
                j_body.reserve(al, binop_other ? 3 : 2);
                j_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_ij, zero,
                        nullptr, false, false)));
                j_body.push_back(al,
                    make_do_loop(var_k, k_start, k_end, k_body));

                if (binop_other) {
                    ASR::expr_t *other_ij = make_binop_other_item(
                        binop_other, {var_i, var_j}, {i_start, j_start});
                    ASR::expr_t *lhs = matmul_is_left ? c_ij : other_ij;
                    ASR::expr_t *rhs = matmul_is_left ? other_ij : c_ij;
                    ASR::expr_t *combined = ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc, lhs, binop_op,
                            rhs, elem_type, nullptr));
                    j_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, c_ij, combined,
                            nullptr, false, false)));
                }

                Vec<ASR::stmt_t*> i_body;
                i_body.reserve(al, 1);
                i_body.push_back(al,
                    make_do_loop(var_j, j_start, j_end, j_body));

                new_body.push_back(al,
                    make_do_loop(var_i, i_start, i_end, i_body));
            } else {
                new_body.push_back(al, stmt);
                continue;
            }
            changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    // A matmul that `inline_matmul_stmts` does not match -- one nested
    // inside a unary minus, inside another intrinsic, inside a call
    // argument, inside an array constructor (`r = [0.0, matmul(a, b)]`)
    // or as an argument of another matmul -- survives into the shader as
    // a call to the host runtime helper `_lcompilers_matmul*`, which does
    // not exist on the device. Hoist every such matmul into its own
    // temporary first, so the existing whole-right-hand-side lowering
    // applies to it and the enclosing expression is left with a plain
    // array variable (a shape the Metal backend already handles).
    void hoist_nested_matmuls(ASR::DoConcurrentLoop_t &x) {
        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }
        hoist_nested_matmuls_in_body(x.m_body, x.n_body, var_scope,
            false, true);
    }

    // `scope_has_workspaces` says whether a run-time sized temporary put
    // into `var_scope` will be given a per-thread VLA workspace buffer.
    // Only a BLOCK that is a direct statement of the loop body is scanned
    // for those; a temporary at kernel scope would be a single buffer
    // shared by every thread.  `at_loop_top` tracks whether this
    // statement list is that loop body itself.
    void hoist_nested_matmuls_in_body(ASR::stmt_t** &body, size_t &n_body,
            SymbolTable *var_scope, bool scope_has_workspaces,
            bool at_loop_top) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 2);
        bool changed = false;

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                hoist_nested_matmuls_in_body(dl->m_body, dl->n_body,
                    var_scope, scope_has_workspaces, false);
                new_body.push_back(al, stmt);
                continue;
            }
            // A spliced-in device function body lives in its own BLOCK;
            // hoist inside it too, into that block's scope.
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    hoist_nested_matmuls_in_body(blk->m_body, blk->n_body,
                        blk->m_symtab, at_loop_top, false);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
                hoist_nested_matmuls_in_body(ab->m_body, ab->n_body,
                    var_scope, scope_has_workspaces, false);
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            size_t before = new_body.size();
            hoist_matmuls_from_assignment(
                ASR::down_cast<ASR::Assignment_t>(stmt), new_body, var_scope,
                scope_has_workspaces);
            new_body.push_back(al, stmt);
            if (new_body.size() != before + 1) changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    // Hoist every matmul in the value of `asgn` that the statement-level
    // lowering cannot see into a temporary, appending the temporaries'
    // assignments to `out`. The matmul the lowering does match is left in
    // place; its arguments are still searched, so a nested matmul such as
    // `matmul(a, matmul(a, b))` has its inner operand hoisted.
    void hoist_matmuls_from_assignment(ASR::Assignment_t *asgn,
            Vec<ASR::stmt_t*> &out, SymbolTable *var_scope,
            bool scope_has_workspaces) {
        ASR::expr_t *binop_other = nullptr;
        ASR::binopType binop_op = ASR::binopType::Add;
        bool matmul_is_left = true;
        ASR::IntrinsicArrayFunction_t *handled = match_statement_matmul(
            asgn->m_value, binop_other, binop_op, matmul_is_left);
        Location loc = asgn->base.base.loc;
        while (true) {
            ASR::IntrinsicArrayFunction_t *mm = find_array_intrinsic_in_expr(
                asgn->m_value, ASRUtils::IntrinsicArrayFunctions::MatMul,
                handled);
            if (!mm) break;
            ASR::expr_t *tmp_var = make_matmul_result_temp(mm, loc,
                var_scope, scope_has_workspaces);
            if (!tmp_var) break;
            ASR::stmt_t *tmp_asgn = ASRUtils::STMT(ASR::make_Assignment_t(
                al, loc, tmp_var, (ASR::expr_t*)mm, nullptr, false, false));
            if (!replace_array_intrinsic_in_expr(asgn->m_value, mm,
                    tmp_var)) {
                break;
            }
            hoist_matmuls_from_assignment(
                ASR::down_cast<ASR::Assignment_t>(tmp_asgn), out, var_scope,
                scope_has_workspaces);
            out.push_back(al, tmp_asgn);
        }
    }

    // A local temporary holding the result of `mm`, or nullptr if the
    // result shape cannot be determined from the operands.
    ASR::expr_t* make_matmul_result_temp(ASR::IntrinsicArrayFunction_t *mm,
            const Location &loc, SymbolTable *var_scope,
            bool scope_has_workspaces) {
        ASR::expr_t *e = (ASR::expr_t*)mm;
        if (!ASRUtils::is_array(ASRUtils::expr_type(e))) return nullptr;
        Vec<ASR::dimension_t> dims;
        if (!intrinsic_array_result_dims(e, dims)) return nullptr;
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        bool all_const = true;
        for (size_t d = 0; d < dims.n; d++) {
            if (!dims[d].m_length ||
                    !ASRUtils::expr_value(dims[d].m_length)) {
                all_const = false;
            }
        }
        // A run-time sized temporary is only correct where each thread
        // gets its own workspace slice; anywhere else it would be one
        // buffer written by every thread at once.
        if (!all_const && !scope_has_workspaces) return nullptr;
        ASR::ttype_t *tmp_type = ASRUtils::TYPE(
            ASR::make_Array_t(al, loc, elem_type, dims.p, dims.n,
                all_const
                    ? ASR::array_physical_typeType::FixedSizeArray
                    : ASR::array_physical_typeType::DescriptorArray));
        std::string name = var_scope->get_unique_name("__gpu_matmul_tmp");
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, var_scope,
                s2c(al, name), nullptr, 0, ASR::intentType::Local,
                nullptr, nullptr, ASR::storage_typeType::Default,
                tmp_type, nullptr, ASR::abiType::Source,
                ASR::accessType::Public,
                ASR::presenceType::Required, false));
        var_scope->add_symbol(name, sym);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }

    // Shape of an array-valued intrinsic's result, taken from its
    // operands' declared dimensions.
    bool intrinsic_array_result_dims(ASR::expr_t *e,
            Vec<ASR::dimension_t> &dims) {
        ASR::IntrinsicArrayFunction_t *iaf =
            ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                iaf->m_arr_intrinsic_id)
                    != ASRUtils::IntrinsicArrayFunctions::MatMul) {
            return false;
        }
        if (iaf->n_args < 2) return false;
        ASR::expr_t *a = iaf->m_args[0];
        ASR::expr_t *b = iaf->m_args[1];
        while (a && ASR::is_a<ASR::ArrayPhysicalCast_t>(*a))
            a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a)->m_arg;
        while (b && ASR::is_a<ASR::ArrayPhysicalCast_t>(*b))
            b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(b)->m_arg;
        if (!a || !b) return false;
        ASR::dimension_t *da = nullptr, *db = nullptr;
        int ra = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(a)), da);
        int rb = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(b)), db);
        dims.reserve(al, 2);
        if (ra == 2 && rb == 1) {
            dims.push_back(al, dim_or_runtime_extent(a, da, 0));
        } else if (ra == 1 && rb == 2) {
            dims.push_back(al, dim_or_runtime_extent(b, db, 1));
        } else if (ra == 2 && rb == 2) {
            dims.push_back(al, dim_or_runtime_extent(a, da, 0));
            dims.push_back(al, dim_or_runtime_extent(b, db, 1));
        } else {
            return false;
        }
        return true;
    }

    // Dimension `d` of `operand`, described so that the extent is
    // available wherever the shape is needed.  A deferred-shape operand
    // -- an allocatable, a pointer or an assumed-shape dummy -- carries no
    // declared length, so its extent is the operand's own run-time
    // `size(operand, d + 1)` instead.  That expression is what the VLA
    // workspace machinery resolves back to the extents the host already
    // passes to the kernel.
    ASR::dimension_t dim_or_runtime_extent(ASR::expr_t *operand,
            ASR::dimension_t *dims, size_t d) {
        if (dims && dims[d].m_length) return dims[d];
        const Location &loc = operand->base.loc;
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        ASR::dimension_t res;
        res.loc = loc;
        res.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            1, int_type, ASR::integerbozType::Decimal));
        ASR::expr_t *dim_expr = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, (int64_t)d + 1, int_type,
                ASR::integerbozType::Decimal));
        res.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
            operand, dim_expr, int_type, nullptr));
        return res;
    }

    void inline_intrinsic_matmul(ASR::DoConcurrentLoop_t &x) {
        inline_matmul_stmts(x.m_body, x.n_body);
    }

    // Distribute ArrayItem indexing through an array expression tree
    // to produce a scalar expression. For example:
    //   sum(a + b) with index k  -->  a(k) + b(k)
    // instead of the incorrect (a + b)[k] which would be pointer arithmetic.
    ASR::expr_t* index_array_expr(ASR::expr_t *expr,
            ASR::array_index_t *idx_p, size_t idx_n,
            ASR::ttype_t *elem_type, const Location &loc) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
        }
        if (!ASRUtils::is_array(ASRUtils::expr_type(expr))) {
            return expr;
        }
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, expr,
                idx_p, idx_n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr));
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            ASR::RealBinOp_t *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
            ASR::expr_t *left = index_array_expr(op->m_left,
                idx_p, idx_n, elem_type, loc);
            ASR::expr_t *right = index_array_expr(op->m_right,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                left, op->m_op, right, elem_type, nullptr));
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
            ASR::IntegerBinOp_t *op =
                ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            ASR::expr_t *left = index_array_expr(op->m_left,
                idx_p, idx_n, elem_type, loc);
            ASR::expr_t *right = index_array_expr(op->m_right,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                left, op->m_op, right, elem_type, nullptr));
        }
        if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
            ASR::RealUnaryMinus_t *u =
                ASR::down_cast<ASR::RealUnaryMinus_t>(expr);
            ASR::expr_t *arg = index_array_expr(u->m_arg,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                arg, elem_type, nullptr));
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
            ASR::IntegerUnaryMinus_t *u =
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
            ASR::expr_t *arg = index_array_expr(u->m_arg,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                arg, elem_type, nullptr));
        }
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, expr,
            idx_p, idx_n, elem_type,
            ASR::arraystorageType::ColMajor, nullptr));
    }

    // Inline IntrinsicArrayFunction Sum inside a DoConcurrentLoop body.
    // Replaces:
    //   results(i) = sum(a)
    // With:
    //   __gpu_sum_res = 0.0
    //   do __gpu_sum_k = 1, n
    //     __gpu_sum_res = __gpu_sum_res + a(__gpu_sum_k)
    //   end do
    //   results(i) = __gpu_sum_res
    // This avoids generating a call to _lcompilers_Sum which is not
    // available inside Metal GPU kernels.
    // Search an expression tree for an IntrinsicArrayFunction node of the
    // given kind. `skip` names a node the caller already handles: it is
    // not reported, but its arguments are still searched.
    ASR::IntrinsicArrayFunction_t* find_array_intrinsic_in_expr(
            ASR::expr_t *expr, ASRUtils::IntrinsicArrayFunctions which,
            ASR::IntrinsicArrayFunction_t *skip = nullptr) {
        if (!expr) return nullptr;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
            auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
            if (iaf != skip &&
                    static_cast<ASRUtils::IntrinsicArrayFunctions>(
                        iaf->m_arr_intrinsic_id) == which) {
                return iaf;
            }
            for (size_t i = 0; i < iaf->n_args; i++) {
                auto *found = find_array_intrinsic_in_expr(iaf->m_args[i],
                    which, skip);
                if (found) return found;
            }
            return nullptr;
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
            auto *found = find_array_intrinsic_in_expr(op->m_left, which,
                skip);
            if (found) return found;
            return find_array_intrinsic_in_expr(op->m_right, which, skip);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            auto *found = find_array_intrinsic_in_expr(op->m_left, which,
                skip);
            if (found) return found;
            return find_array_intrinsic_in_expr(op->m_right, which, skip);
        }
        if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
            return find_array_intrinsic_in_expr(
                ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_arg, which,
                skip);
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
            return find_array_intrinsic_in_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg, which,
                skip);
        }
        if (ASR::is_a<ASR::Cast_t>(*expr)) {
            return find_array_intrinsic_in_expr(
                ASR::down_cast<ASR::Cast_t>(expr)->m_arg, which, skip);
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            return find_array_intrinsic_in_expr(
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg, which,
                skip);
        }
        if (ASR::is_a<ASR::RealCompare_t>(*expr)) {
            auto *cmp = ASR::down_cast<ASR::RealCompare_t>(expr);
            auto *found = find_array_intrinsic_in_expr(cmp->m_left, which,
                skip);
            if (found) return found;
            return find_array_intrinsic_in_expr(cmp->m_right, which, skip);
        }
        if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
            auto *cmp = ASR::down_cast<ASR::IntegerCompare_t>(expr);
            auto *found = find_array_intrinsic_in_expr(cmp->m_left, which,
                skip);
            if (found) return found;
            return find_array_intrinsic_in_expr(cmp->m_right, which, skip);
        }
        if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
            auto *ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            for (size_t i = 0; i < ief->n_args; i++) {
                auto *found = find_array_intrinsic_in_expr(ief->m_args[i],
                    which, skip);
                if (found) return found;
            }
        }
        if (ASR::is_a<ASR::FunctionCall_t>(*expr)) {
            auto *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
            for (size_t i = 0; i < fc->n_args; i++) {
                auto *found = find_array_intrinsic_in_expr(
                    fc->m_args[i].m_value, which, skip);
                if (found) return found;
            }
        }
        if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
            auto *ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
            for (size_t i = 0; i < ac->n_args; i++) {
                auto *found = find_array_intrinsic_in_expr(ac->m_args[i],
                    which, skip);
                if (found) return found;
            }
        }
        return nullptr;
    }

    // Replace a specific IntrinsicArrayFunction node in an expression tree
    // with a replacement expression.
    bool replace_array_intrinsic_in_expr(ASR::expr_t* &expr,
            ASR::IntrinsicArrayFunction_t *target,
            ASR::expr_t *replacement) {
        if (!expr) return false;
        if (expr == (ASR::expr_t*)target) {
            expr = replacement;
            return true;
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            auto *c = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
            if (c->m_arg == (ASR::expr_t*)target) {
                c->m_arg = replacement;
                c->m_old = ASRUtils::extract_physical_type(
                    ASRUtils::expr_type(replacement));
                return true;
            }
            return replace_array_intrinsic_in_expr(c->m_arg, target,
                replacement);
        }
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
            auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
            for (size_t i = 0; i < iaf->n_args; i++) {
                if (replace_array_intrinsic_in_expr(iaf->m_args[i], target,
                        replacement))
                    return true;
            }
            return false;
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
            if (replace_array_intrinsic_in_expr(op->m_left, target,
                    replacement))
                return true;
            return replace_array_intrinsic_in_expr(op->m_right, target,
                replacement);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            if (replace_array_intrinsic_in_expr(op->m_left, target,
                    replacement))
                return true;
            return replace_array_intrinsic_in_expr(op->m_right, target,
                replacement);
        }
        if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
            return replace_array_intrinsic_in_expr(
                ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_arg, target,
                replacement);
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
            return replace_array_intrinsic_in_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg, target,
                replacement);
        }
        if (ASR::is_a<ASR::Cast_t>(*expr)) {
            return replace_array_intrinsic_in_expr(
                ASR::down_cast<ASR::Cast_t>(expr)->m_arg, target, replacement);
        }
        if (ASR::is_a<ASR::RealCompare_t>(*expr)) {
            auto *cmp = ASR::down_cast<ASR::RealCompare_t>(expr);
            if (replace_array_intrinsic_in_expr(cmp->m_left, target,
                    replacement))
                return true;
            return replace_array_intrinsic_in_expr(cmp->m_right, target,
                replacement);
        }
        if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
            auto *cmp = ASR::down_cast<ASR::IntegerCompare_t>(expr);
            if (replace_array_intrinsic_in_expr(cmp->m_left, target,
                    replacement))
                return true;
            return replace_array_intrinsic_in_expr(cmp->m_right, target,
                replacement);
        }
        if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
            auto *ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            for (size_t i = 0; i < ief->n_args; i++) {
                if (replace_array_intrinsic_in_expr(ief->m_args[i], target,
                        replacement))
                    return true;
            }
        }
        if (ASR::is_a<ASR::FunctionCall_t>(*expr)) {
            auto *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
            for (size_t i = 0; i < fc->n_args; i++) {
                if (replace_array_intrinsic_in_expr(fc->m_args[i].m_value,
                        target, replacement))
                    return true;
            }
        }
        if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
            auto *ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
            for (size_t i = 0; i < ac->n_args; i++) {
                if (replace_array_intrinsic_in_expr(ac->m_args[i], target,
                        replacement))
                    return true;
            }
        }
        return false;
    }

    // Extract nested Sum calls from assignment values into separate
    // temporary assignments so the main Sum inlining logic can handle them.
    // E.g., "cost = cost + sum(a)" becomes:
    //   "__gpu_sum_tmp = sum(a)"
    //   "cost = cost + __gpu_sum_tmp"
    void extract_nested_sums(ASR::stmt_t** &stmts, size_t &n_stmts,
                             SymbolTable *scope) {
        Vec<ASR::stmt_t*> expanded;
        expanded.reserve(al, n_stmts * 2);
        bool changed = false;

        for (size_t i = 0; i < n_stmts; i++) {
            ASR::stmt_t *stmt = stmts[i];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                expanded.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmt);

            // Skip if value is already a direct Sum
            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                expanded.push_back(al, stmt);
                continue;
            }

            ASR::IntrinsicArrayFunction_t *sum_node =
                find_array_intrinsic_in_expr(asgn->m_value,
                    ASRUtils::IntrinsicArrayFunctions::Sum);
            if (!sum_node) {
                expanded.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *sum_type = sum_node->m_type;

            SymbolTable *var_scope = scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            std::string tmp_name =
                var_scope->get_unique_name("__gpu_sum_tmp");
            ASR::symbol_t *tmp_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, var_scope,
                    s2c(al, tmp_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, sum_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            var_scope->add_symbol(tmp_name, tmp_sym);
            ASR::expr_t *tmp_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, tmp_sym));

            // Create: __gpu_sum_tmp = sum(a)
            ASR::expr_t *sum_expr = (ASR::expr_t*)sum_node;
            expanded.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp_var, sum_expr,
                    nullptr, false, false)));

            // Replace sum node in original expression with tmp_var
            replace_array_intrinsic_in_expr(asgn->m_value, sum_node,
                tmp_var);

            // Add modified original assignment
            expanded.push_back(al, stmt);
            changed = true;
        }

        if (changed) {
            stmts = expanded.p;
            n_stmts = expanded.n;
        }
    }

    void inline_sum_in_stmts(ASR::stmt_t** &stmts, size_t &n_stmts,
                             SymbolTable *scope) {
        // Pre-pass: extract nested Sum calls into separate assignments
        extract_nested_sums(stmts, n_stmts, scope);

        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_stmts * 4);
        bool changed = false;

        for (size_t si = 0; si < n_stmts; si++) {
            ASR::stmt_t *stmt = stmts[si];

            // Recurse into DoLoop bodies
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_sum_in_stmts(dl->m_body, dl->n_body, scope);
                new_body.push_back(al, stmt);
                continue;
            }

            // Recurse into Block bodies
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_sum_in_stmts(block->m_body, block->n_body,
                        block->m_symtab);
                }
                new_body.push_back(al, stmt);
                continue;
            }

            // Recurse into AssociateBlock bodies
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                inline_sum_in_stmts(ab->m_body, ab->n_body,
                    ab->m_symtab);
                new_body.push_back(al, stmt);
                continue;
            }

            // Recurse into If bodies
            if (ASR::is_a<ASR::If_t>(*stmt)) {
                ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(stmt);
                inline_sum_in_stmts(if_stmt->m_body, if_stmt->n_body,
                    scope);
                inline_sum_in_stmts(if_stmt->m_orelse, if_stmt->n_orelse,
                    scope);
                new_body.push_back(al, stmt);
                continue;
            }

            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(asgn->m_value);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::Sum) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            ASR::expr_t *arr_arg = iaf->m_args[0];
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arr_arg)) {
                arr_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    arr_arg)->m_arg;
            }

            ASR::ttype_t *elem_type = iaf->m_type;

            SymbolTable *var_scope = scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_var = [&](const std::string &prefix,
                    ASR::ttype_t *type) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            ASR::expr_t *res_var = make_var("__gpu_sum_res", elem_type);
            ASR::expr_t *zero;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                    0.0, elem_type));
            } else {
                zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                    0, elem_type, ASR::integerbozType::Decimal));
            }

            // __gpu_sum_res = 0
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var, zero,
                    nullptr, false, false)));

            std::vector<ASR::expr_t*> loop_vars;
            std::vector<ASR::expr_t*> loop_starts;
            std::vector<ASR::expr_t*> loop_ends;
            std::vector<ASR::expr_t*> loop_steps;
            Vec<ASR::array_index_t> idx_args = {};
            ASR::expr_t *base_arr = nullptr;
            ASR::expr_t *arr_elem = nullptr;

            if (ASR::is_a<ASR::ArraySection_t>(*arr_arg)) {
                // ArraySection (e.g., x(:,i)): loop over range dimensions,
                // use scalar indices directly
                ASR::ArraySection_t *section =
                    ASR::down_cast<ASR::ArraySection_t>(arr_arg);
                base_arr = section->m_v;
                std::vector<size_t> range_dims;
                for (size_t d = 0; d < section->n_args; d++) {
                    if (section->m_args[d].m_left != nullptr) {
                        range_dims.push_back(d);
                    }
                }
                if (range_dims.empty()) {
                    new_body.push_back(al, stmt);
                    continue;
                }
                for (size_t ri = 0; ri < range_dims.size(); ri++) {
                    size_t d = range_dims[ri];
                    loop_vars.push_back(make_var("__gpu_sum_k", int_type));
                    loop_starts.push_back(section->m_args[d].m_left);
                    loop_ends.push_back(section->m_args[d].m_right);
                    loop_steps.push_back(section->m_args[d].m_step);
                }
                idx_args.reserve(al, section->n_args);
                size_t lv_idx = 0;
                for (size_t d = 0; d < section->n_args; d++) {
                    ASR::array_index_t ai;
                    ai.loc = loc;
                    ai.m_left = nullptr;
                    ai.m_step = nullptr;
                    if (section->m_args[d].m_left != nullptr) {
                        ai.m_right = loop_vars[lv_idx++];
                    } else {
                        ai.m_right = section->m_args[d].m_right;
                    }
                    idx_args.push_back(al, ai);
                }
            } else {
                // Check if arr_arg is an expression containing
                // ArraySection nodes (e.g., a(1:n) + b(1:n))
                ASR::expr_t *sec_start = nullptr, *sec_end = nullptr;
                find_array_section_bounds(arr_arg, sec_start, sec_end);
                if (sec_start && sec_end) {
                    loop_vars.push_back(
                        make_var("__gpu_sum_k", int_type));
                    loop_starts.push_back(sec_start);
                    loop_ends.push_back(sec_end);
                    loop_steps.push_back(nullptr);
                    arr_elem = elementize_mask(arr_arg, loop_vars[0],
                        elem_type, loc);
                } else {
                    // Whole array: loop over all dimensions
                    ASR::ttype_t *arr_type =
                        ASRUtils::expr_type(arr_arg);
                    ASR::dimension_t *dims = nullptr;
                    int rank =
                        ASRUtils::extract_dimensions_from_ttype(
                            arr_type, dims);
                    if (rank < 1) {
                        new_body.push_back(al, stmt);
                        continue;
                    }

                    // If arr_arg is a FunctionCall returning an
                    // allocatable copy of a struct member (e.g.,
                    // sum(vals(x)) where vals returns x%v), resolve
                    // to the actual struct member access (x%v) so the
                    // sum loop iterates directly over the member data
                    // without allocating a temporary array.
                    if (ASR::is_a<ASR::FunctionCall_t>(*arr_arg)) {
                        ASR::FunctionCall_t *fc2 =
                            ASR::down_cast<ASR::FunctionCall_t>(
                                arr_arg);
                        ASR::symbol_t *fn_sym2 =
                            ASRUtils::symbol_get_past_external(
                                fc2->m_name);
                        if (ASR::is_a<ASR::Function_t>(*fn_sym2)) {
                            ASR::Function_t *fn2 =
                                ASR::down_cast<ASR::Function_t>(
                                    fn_sym2);
                            if (fn2->m_return_var &&
                                    ASR::is_a<ASR::Var_t>(
                                        *fn2->m_return_var)) {
                                std::string ret_name2 =
                                    ASRUtils::symbol_name(
                                        ASR::down_cast<ASR::Var_t>(
                                            fn2->m_return_var)->m_v);
                                for (size_t bi = 0;
                                        bi < fn2->n_body; bi++) {
                                    if (!ASR::is_a<ASR::Assignment_t>(
                                            *fn2->m_body[bi]))
                                        continue;
                                    ASR::Assignment_t *ba =
                                        ASR::down_cast<
                                            ASR::Assignment_t>(
                                                fn2->m_body[bi]);
                                    if (!ASR::is_a<ASR::Var_t>(
                                            *ba->m_target))
                                        continue;
                                    std::string tname =
                                        ASRUtils::symbol_name(
                                            ASR::down_cast<
                                                ASR::Var_t>(
                                                    ba->m_target)
                                                ->m_v);
                                    if (tname != ret_name2) continue;
                                    if (!ASR::is_a<
                                            ASR::StructInstanceMember_t>(
                                                *ba->m_value))
                                        continue;
                                    ASR::StructInstanceMember_t *sim =
                                        ASR::down_cast<
                                            ASR::StructInstanceMember_t>(
                                                ba->m_value);
                                    if (!ASR::is_a<ASR::Var_t>(
                                            *sim->m_v))
                                        continue;
                                    ASR::symbol_t *param_sym2 =
                                        ASR::down_cast<ASR::Var_t>(
                                            sim->m_v)->m_v;
                                    int pidx = -1;
                                    for (size_t pi = 0;
                                            pi < fn2->n_args; pi++) {
                                        if (ASR::is_a<ASR::Var_t>(
                                                *fn2->m_args[pi]) &&
                                            ASR::down_cast<ASR::Var_t>(
                                                fn2->m_args[pi])
                                                ->m_v == param_sym2) {
                                            pidx = (int)pi;
                                            break;
                                        }
                                    }
                                    if (pidx < 0 ||
                                        (size_t)pidx >= fc2->n_args ||
                                        !fc2->m_args[pidx].m_value)
                                        break;
                                    ASR::expr_t *actual =
                                        fc2->m_args[pidx].m_value;
                                    // Create ExternalSymbol for the
                                    // struct member in the caller scope
                                    ASR::symbol_t *orig_mem =
                                        ASRUtils::
                                            symbol_get_past_external(
                                                sim->m_m);
                                    std::string mem_name =
                                        ASRUtils::symbol_name(orig_mem);
                                    SymbolTable *mem_st =
                                        ASRUtils::
                                            symbol_parent_symtab(
                                                orig_mem);
                                    ASR::symbol_t *struct_sym2 =
                                        ASR::down_cast<ASR::symbol_t>(
                                            mem_st->asr_owner);
                                    std::string sname =
                                        ASRUtils::symbol_name(
                                            struct_sym2);
                                    std::string ext_name =
                                        var_scope->get_unique_name(
                                            "1_" + sname + "_"
                                            + mem_name);
                                    ASR::symbol_t *ext_sym =
                                        ASR::down_cast<ASR::symbol_t>(
                                            ASR::make_ExternalSymbol_t(
                                                al, loc, var_scope,
                                                s2c(al, ext_name),
                                                orig_mem,
                                                s2c(al, sname),
                                                nullptr, 0,
                                                s2c(al, mem_name),
                                                ASR::accessType::
                                                    Public));
                                    var_scope->add_symbol(
                                        ext_name, ext_sym);
                                    arr_arg = ASRUtils::EXPR(
                                        ASR::make_StructInstanceMember_t(
                                            al, loc, actual, ext_sym,
                                            sim->m_type, nullptr));
                                    arr_type =
                                        ASRUtils::
                                            type_get_past_allocatable_pointer(
                                                ASRUtils::expr_type(
                                                    arr_arg));
                                    dims = nullptr;
                                    rank =
                                        ASRUtils::
                                            extract_dimensions_from_ttype(
                                                arr_type, dims);
                                    break;
                                }
                            }
                        }
                    }

                    base_arr = arr_arg;
                    for (int d = 0; d < rank; d++) {
                        loop_vars.push_back(
                            make_var("__gpu_sum_k", int_type));
                        if (dims[d].m_start && dims[d].m_length) {
                            loop_starts.push_back(dims[d].m_start);
                            loop_ends.push_back(dims[d].m_length);
                        } else if (ASR::is_a<ASR::FunctionCall_t>(
                                *arr_arg)) {
                            // FunctionCall returns allocatable with
                            // unknown dims. Extract allocation bounds
                            // from the function body to avoid emitting
                            // ArrayBound on a FunctionCall (unsupported
                            // by Metal codegen).
                            ASR::FunctionCall_t *fc =
                                ASR::down_cast<ASR::FunctionCall_t>(
                                    arr_arg);
                            ASR::symbol_t *fn_sym =
                                ASRUtils::symbol_get_past_external(
                                    fc->m_name);
                            bool found = false;
                            if (ASR::is_a<ASR::Function_t>(*fn_sym)) {
                                ASR::Function_t *fn =
                                    ASR::down_cast<ASR::Function_t>(
                                        fn_sym);
                                std::string ret_name;
                                if (fn->m_return_var &&
                                        ASR::is_a<ASR::Var_t>(
                                            *fn->m_return_var)) {
                                    ret_name =
                                        ASRUtils::symbol_name(
                                            ASR::down_cast<
                                                ASR::Var_t>(
                                                fn->m_return_var)
                                                ->m_v);
                                }
                                for (size_t bi = 0;
                                        bi < fn->n_body &&
                                        !ret_name.empty() && !found;
                                        bi++) {
                                    if (!ASR::is_a<ASR::Allocate_t>(
                                            *fn->m_body[bi]))
                                        continue;
                                    ASR::Allocate_t *al_stmt =
                                        ASR::down_cast<
                                            ASR::Allocate_t>(
                                                fn->m_body[bi]);
                                    for (size_t ai2 = 0;
                                            ai2 < al_stmt->n_args;
                                            ai2++) {
                                        if (!al_stmt->m_args[ai2].m_a
                                            || !ASR::is_a<ASR::Var_t>(
                                                *al_stmt->m_args[ai2]
                                                    .m_a))
                                            continue;
                                        std::string aname =
                                            ASRUtils::symbol_name(
                                                ASR::down_cast<
                                                    ASR::Var_t>(
                                                    al_stmt->m_args
                                                        [ai2].m_a)
                                                    ->m_v);
                                        if (aname != ret_name)
                                            continue;
                                        if ((size_t)d <
                                                al_stmt->m_args[ai2]
                                                    .n_dims) {
                                            ASR::dimension_t &adim =
                                                al_stmt->m_args[ai2]
                                                    .m_dims[d];
                                            if (adim.m_start) {
                                                loop_starts.push_back(
                                                    adim.m_start);
                                            } else {
                                                loop_starts.push_back(
                                                    ASRUtils::EXPR(
                                                        ASR::make_IntegerConstant_t(
                                                            al, loc,
                                                            1,
                                                            int_type,
                                                            ASR::integerbozType::Decimal)));
                                            }
                                            if (adim.m_length) {
                                                loop_ends.push_back(
                                                    adim.m_length);
                                            }
                                            found = true;
                                        }
                                        break;
                                    }
                                }
                            }
                            if (!found) {
                                // No Allocate found in the function
                                // body.  Fall back to the actual call
                                // arguments: use the first array
                                // actual argument's bounds (the
                                // return shape typically matches the
                                // input shape for element-wise
                                // functions like r = a).
                                for (size_t ai3 = 0;
                                        ai3 < fc->n_args && !found;
                                        ai3++) {
                                    if (!fc->m_args[ai3].m_value)
                                        continue;
                                    ASR::expr_t *actual =
                                        fc->m_args[ai3].m_value;
                                    if (ASR::is_a<
                                            ASR::ArrayPhysicalCast_t>(
                                                *actual)) {
                                        actual = ASR::down_cast<
                                            ASR::ArrayPhysicalCast_t>(
                                                actual)->m_arg;
                                    }
                                    ASR::ttype_t *atype =
                                        ASRUtils::type_get_past_allocatable_pointer(
                                            ASRUtils::expr_type(
                                                actual));
                                    ASR::dimension_t *adims = nullptr;
                                    int arank =
                                        ASRUtils::extract_dimensions_from_ttype(
                                            atype, adims);
                                    if (arank < 1 ||
                                            (size_t)d >= (size_t)arank)
                                        continue;
                                    if (adims[d].m_start &&
                                            adims[d].m_length) {
                                        loop_starts.push_back(
                                            adims[d].m_start);
                                        loop_ends.push_back(
                                            adims[d].m_length);
                                        found = true;
                                    }
                                }
                            }
                            if (!found) {
                                ASR::expr_t *dim_expr =
                                    ASRUtils::EXPR(
                                        ASR::make_IntegerConstant_t(
                                            al, loc, d + 1,
                                            int_type,
                                            ASR::integerbozType::Decimal));
                                loop_starts.push_back(ASRUtils::EXPR(
                                    ASR::make_ArrayBound_t(al, loc,
                                        arr_arg, dim_expr, int_type,
                                        ASR::arrayboundType::LBound,
                                        nullptr)));
                                loop_ends.push_back(ASRUtils::EXPR(
                                    ASR::make_ArrayBound_t(al, loc,
                                        arr_arg, dim_expr, int_type,
                                        ASR::arrayboundType::UBound,
                                        nullptr)));
                            }
                        } else {
                            ASR::expr_t *dim_expr = ASRUtils::EXPR(
                                ASR::make_IntegerConstant_t(al, loc,
                                    d + 1, int_type,
                                    ASR::integerbozType::Decimal));
                            loop_starts.push_back(ASRUtils::EXPR(
                                ASR::make_ArrayBound_t(al, loc,
                                    arr_arg, dim_expr, int_type,
                                    ASR::arrayboundType::LBound,
                                    nullptr)));
                            loop_ends.push_back(ASRUtils::EXPR(
                                ASR::make_ArrayBound_t(al, loc,
                                    arr_arg, dim_expr, int_type,
                                    ASR::arrayboundType::UBound,
                                    nullptr)));
                        }
                        loop_steps.push_back(nullptr);
                    }
                    idx_args.reserve(al, rank);
                    for (int d = 0; d < rank; d++) {
                        ASR::array_index_t ai;
                        ai.loc = loc;
                        ai.m_left = nullptr;
                        ai.m_right = loop_vars[d];
                        ai.m_step = nullptr;
                        idx_args.push_back(al, ai);
                    }
                }
            }

            if (!arr_elem) {
                arr_elem = index_array_expr(base_arr,
                        idx_args.p, idx_args.n, elem_type, loc);
            }

            // res = res + a(k1, k2, ...) or a(k1, i, ...)
            ASR::expr_t *add_expr;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                add_expr = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                    res_var, ASR::binopType::Add, arr_elem,
                    elem_type, nullptr));
            } else {
                add_expr = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    res_var, ASR::binopType::Add, arr_elem,
                    elem_type, nullptr));
            }
            ASR::stmt_t *accum_stmt = ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var, add_expr,
                    nullptr, false, false));

            // Build nested loops from innermost to outermost
            int n_loops = (int)loop_vars.size();
            Vec<ASR::stmt_t*> innermost_body;
            innermost_body.reserve(al, 1);
            innermost_body.push_back(al, accum_stmt);

            ASR::stmt_t *loop_nest = nullptr;
            for (int d = n_loops - 1; d >= 0; d--) {
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = loop_vars[d];
                head.m_start = loop_starts[d];
                head.m_end = loop_ends[d];
                head.m_increment = loop_steps[d];
                if (d == n_loops - 1) {
                    loop_nest = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
                        nullptr, head, innermost_body.p, innermost_body.n,
                        nullptr, 0));
                } else {
                    Vec<ASR::stmt_t*> outer_body;
                    outer_body.reserve(al, 1);
                    outer_body.push_back(al, loop_nest);
                    loop_nest = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
                        nullptr, head, outer_body.p, outer_body.n,
                        nullptr, 0));
                }
            }
            new_body.push_back(al, loop_nest);

            // target = __gpu_sum_res
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, asgn->m_target, res_var,
                    nullptr, false, false)));

            changed = true;
        }

        if (changed) {
            stmts = new_body.p;
            n_stmts = new_body.n;
        }
    }

    void inline_intrinsic_sum(ASR::DoConcurrentLoop_t &x) {
        inline_sum_in_stmts(x.m_body, x.n_body, current_scope);
    }

    // Build the `k`-th element (k is 1-based within the dot_product) of a
    // rank-1 dot_product argument. Returns nullptr when the argument's
    // shape cannot be indexed directly.
    ASR::expr_t* dot_product_operand_element(ASR::expr_t *arg,
            ASR::expr_t *k, ASR::ttype_t *elem_type, const Location &loc) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
            arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
        }
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        auto mk_int = [&](int64_t v) -> ASR::expr_t* {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, v,
                int_type, ASR::integerbozType::Decimal));
        };
        auto binop = [&](ASR::expr_t *l, ASR::binopType op,
                ASR::expr_t *r) -> ASR::expr_t* {
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l, op, r,
                int_type, nullptr));
        };
        // k - 1
        ASR::expr_t *km1 = binop(k, ASR::binopType::Sub, mk_int(1));
        if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
            ASR::ArraySection_t *sec =
                ASR::down_cast<ASR::ArraySection_t>(arg);
            int range_dim = -1;
            for (size_t d = 0; d < sec->n_args; d++) {
                if (sec->m_args[d].m_left != nullptr) {
                    if (range_dim >= 0) return nullptr;
                    range_dim = (int)d;
                }
            }
            if (range_dim < 0) return nullptr;
            Vec<ASR::array_index_t> idx;
            idx.reserve(al, sec->n_args);
            for (size_t d = 0; d < sec->n_args; d++) {
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_step = nullptr;
                if ((int)d == range_dim) {
                    ASR::expr_t *delta = km1;
                    if (sec->m_args[d].m_step != nullptr) {
                        delta = binop(km1, ASR::binopType::Mul,
                            sec->m_args[d].m_step);
                    }
                    ai.m_right = binop(sec->m_args[d].m_left,
                        ASR::binopType::Add, delta);
                } else {
                    ai.m_right = sec->m_args[d].m_right;
                }
                idx.push_back(al, ai);
            }
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, sec->m_v,
                idx.p, idx.n, elem_type, ASR::arraystorageType::ColMajor,
                nullptr));
        }
        ASR::ttype_t *arr_type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(arg));
        ASR::dimension_t *dims = nullptr;
        int rank = ASRUtils::extract_dimensions_from_ttype(arr_type, dims);
        if (rank != 1) return nullptr;
        ASR::expr_t *lbound = dims[0].m_start;
        ASR::expr_t *index = nullptr;
        if (lbound == nullptr) {
            lbound = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arg,
                mk_int(1), int_type, ASR::arrayboundType::LBound, nullptr));
            index = binop(lbound, ASR::binopType::Add, km1);
        } else if (ASR::is_a<ASR::IntegerConstant_t>(*lbound) &&
                ASR::down_cast<ASR::IntegerConstant_t>(lbound)->m_n == 1) {
            index = k;
        } else {
            index = binop(lbound, ASR::binopType::Add, km1);
        }
        Vec<ASR::array_index_t> idx;
        idx.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_step = nullptr;
        ai.m_right = index;
        idx.push_back(al, ai);
        return index_array_expr(arg, idx.p, idx.n, elem_type, loc);
    }

    // Number of elements of a rank-1 dot_product argument. When
    // `allow_bound` is false, only shapes whose extent is available from
    // the type (or from an explicit section range) are accepted, so that
    // an ArrayBound on an allocatable is used only as a last resort.
    ASR::expr_t* dot_product_extent(ASR::expr_t *arg, const Location &loc,
            bool allow_bound) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
            arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
        }
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        auto mk_int = [&](int64_t v) -> ASR::expr_t* {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, v,
                int_type, ASR::integerbozType::Decimal));
        };
        auto binop = [&](ASR::expr_t *l, ASR::binopType op,
                ASR::expr_t *r) -> ASR::expr_t* {
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l, op, r,
                int_type, nullptr));
        };
        if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
            ASR::ArraySection_t *sec =
                ASR::down_cast<ASR::ArraySection_t>(arg);
            int range_dim = -1;
            for (size_t d = 0; d < sec->n_args; d++) {
                if (sec->m_args[d].m_left != nullptr) {
                    if (range_dim >= 0) return nullptr;
                    range_dim = (int)d;
                }
            }
            if (range_dim < 0 || sec->m_args[range_dim].m_right == nullptr) {
                return nullptr;
            }
            ASR::expr_t *span = binop(sec->m_args[range_dim].m_right,
                ASR::binopType::Sub, sec->m_args[range_dim].m_left);
            if (sec->m_args[range_dim].m_step != nullptr) {
                span = binop(span, ASR::binopType::Div,
                    sec->m_args[range_dim].m_step);
            }
            return binop(span, ASR::binopType::Add, mk_int(1));
        }
        ASR::ttype_t *arr_type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(arg));
        ASR::dimension_t *dims = nullptr;
        int rank = ASRUtils::extract_dimensions_from_ttype(arr_type, dims);
        if (rank != 1) return nullptr;
        if (dims[0].m_length != nullptr) {
            return dims[0].m_length;
        }
        if (!allow_bound) return nullptr;
        ASR::expr_t *ub = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arg,
            mk_int(1), int_type, ASR::arrayboundType::UBound, nullptr));
        ASR::expr_t *lb = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arg,
            mk_int(1), int_type, ASR::arrayboundType::LBound, nullptr));
        return binop(binop(ub, ASR::binopType::Sub, lb),
            ASR::binopType::Add, mk_int(1));
    }

    // Inline IntrinsicArrayFunction DotProduct inside a DoConcurrentLoop
    // body. Replaces:
    //   r(i) = dot_product(a, b)
    // With:
    //   __gpu_dot_res = 0
    //   do __gpu_dot_k = 1, n
    //     __gpu_dot_res = __gpu_dot_res + a(...) * b(...)
    //   end do
    //   r(i) = __gpu_dot_res
    // Unlike matmul, dot_product survives array lowering as a call to the
    // generated helper `_lcompilers_dot_product_*`, whose definition is
    // never emitted into the Metal shader. Expanding it here keeps the
    // kernel self-contained.
    void inline_dot_product_in_stmts(ASR::stmt_t** &stmts, size_t &n_stmts,
                                     SymbolTable *scope) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_stmts * 4);
        bool changed = false;

        for (size_t si = 0; si < n_stmts; si++) {
            ASR::stmt_t *stmt = stmts[si];

            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_dot_product_in_stmts(dl->m_body, dl->n_body, scope);
                new_body.push_back(al, stmt);
                continue;
            }
            // A `do concurrent` nested in the loop being offloaded runs
            // serially inside the kernel, so its body is device code too
            // and its dot products have to be expanded as well. It is
            // still a DoConcurrentLoop at this point: a spliced callee's
            // own loops are only sequentialized on the next round.
            if (ASR::is_a<ASR::DoConcurrentLoop_t>(*stmt)) {
                ASR::DoConcurrentLoop_t *dcl =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(stmt);
                inline_dot_product_in_stmts(dcl->m_body, dcl->n_body,
                    scope);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_dot_product_in_stmts(block->m_body, block->n_body,
                        block->m_symtab);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                inline_dot_product_in_stmts(ab->m_body, ab->n_body,
                    ab->m_symtab);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::If_t>(*stmt)) {
                ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(stmt);
                inline_dot_product_in_stmts(if_stmt->m_body,
                    if_stmt->n_body, scope);
                inline_dot_product_in_stmts(if_stmt->m_orelse,
                    if_stmt->n_orelse, scope);
                new_body.push_back(al, stmt);
                continue;
            }

            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(asgn->m_value);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::DotProduct) {
                new_body.push_back(al, stmt);
                continue;
            }
            if (iaf->n_args < 2) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *elem_type = iaf->m_type;
            // complex dot_product conjugates its first argument and the
            // logical form is a masked any(); neither is handled here.
            if (!ASR::is_a<ASR::Real_t>(*elem_type) &&
                    !ASR::is_a<ASR::Integer_t>(*elem_type)) {
                new_body.push_back(al, stmt);
                continue;
            }

            ASR::expr_t *n_elems = dot_product_extent(iaf->m_args[0], loc,
                false);
            if (n_elems == nullptr) {
                n_elems = dot_product_extent(iaf->m_args[1], loc, false);
            }
            if (n_elems == nullptr) {
                n_elems = dot_product_extent(iaf->m_args[0], loc, true);
            }
            if (n_elems == nullptr) {
                new_body.push_back(al, stmt);
                continue;
            }

            SymbolTable *var_scope = scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            auto make_var = [&](const std::string &prefix,
                    ASR::ttype_t *type) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            ASR::expr_t *k_var = make_var("__gpu_dot_k", int_type);
            ASR::expr_t *lhs_elem = dot_product_operand_element(
                iaf->m_args[0], k_var, elem_type, loc);
            ASR::expr_t *rhs_elem = dot_product_operand_element(
                iaf->m_args[1], k_var, elem_type, loc);
            if (lhs_elem == nullptr || rhs_elem == nullptr) {
                new_body.push_back(al, stmt);
                continue;
            }

            ASR::expr_t *res_var = make_var("__gpu_dot_res", elem_type);
            ASR::expr_t *zero;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                    0.0, elem_type));
            } else {
                zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                    0, elem_type, ASR::integerbozType::Decimal));
            }
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var, zero,
                    nullptr, false, false)));

            ASR::expr_t *prod, *acc;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                prod = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                    lhs_elem, ASR::binopType::Mul, rhs_elem, elem_type,
                    nullptr));
                acc = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                    res_var, ASR::binopType::Add, prod, elem_type, nullptr));
            } else {
                prod = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    lhs_elem, ASR::binopType::Mul, rhs_elem, elem_type,
                    nullptr));
                acc = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    res_var, ASR::binopType::Add, prod, elem_type, nullptr));
            }
            Vec<ASR::stmt_t*> loop_body;
            loop_body.reserve(al, 1);
            loop_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var, acc,
                    nullptr, false, false)));

            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = k_var;
            head.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                1, int_type, ASR::integerbozType::Decimal));
            head.m_end = n_elems;
            head.m_increment = nullptr;
            new_body.push_back(al, ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
                nullptr, head, loop_body.p, loop_body.n, nullptr, 0)));

            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, asgn->m_target, res_var,
                    nullptr, false, false)));
            changed = true;
        }

        if (changed) {
            stmts = new_body.p;
            n_stmts = new_body.n;
        }
    }

    void inline_intrinsic_dot_product(ASR::DoConcurrentLoop_t &x) {
        inline_dot_product_in_stmts(x.m_body, x.n_body, current_scope);
    }

    // Inline IntrinsicArrayFunction Transpose inside a DoConcurrentLoop body.
    // Replaces:
    //   b = transpose(a)
    // With:
    //   do __gpu_tr_j = 1, n
    //     do __gpu_tr_i = 1, m
    //       b(__gpu_tr_i, __gpu_tr_j) = a(__gpu_tr_j, __gpu_tr_i)
    //     end do
    //   end do
    // This avoids generating a call to _lcompilers_transpose which is not
    // available inside Metal GPU kernels.
    void inline_intrinsic_transpose(ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body * 4);
        bool changed = false;

        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            ASR::expr_t *value = asgn->m_value;
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*value)) {
                value = ASR::down_cast<ASR::ArrayPhysicalCast_t>(value)->m_arg;
            }
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*value)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(value);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::Transpose) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            ASR::expr_t *arr_arg = iaf->m_args[0];
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arr_arg)) {
                arr_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    arr_arg)->m_arg;
            }

            ASR::ttype_t *arr_type = ASRUtils::expr_type(arr_arg);
            ASR::dimension_t *dims = nullptr;
            int rank = ASRUtils::extract_dimensions_from_ttype(arr_type, dims);
            if (rank != 2) {
                new_body.push_back(al, stmt);
                continue;
            }

            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(asgn->m_target));

            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_var = [&](const std::string &prefix) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            auto make_array_item_2d = [&](ASR::expr_t *arr,
                    ASR::expr_t *idx1, ASR::expr_t *idx2) -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 2);
                ASR::array_index_t ai1;
                ai1.loc = loc;
                ai1.m_left = nullptr;
                ai1.m_right = idx1;
                ai1.m_step = nullptr;
                args.push_back(al, ai1);
                ASR::array_index_t ai2;
                ai2.loc = loc;
                ai2.m_left = nullptr;
                ai2.m_right = idx2;
                ai2.m_step = nullptr;
                args.push_back(al, ai2);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                    args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            };

            // a is (m, n) => b = transpose(a) is (n, m)
            // b(i, j) = a(j, i) for i=1..n, j=1..m
            ASR::expr_t *var_i = make_var("__gpu_tr_i");
            ASR::expr_t *var_j = make_var("__gpu_tr_j");

            ASR::expr_t *b_ij = make_array_item_2d(asgn->m_target,
                var_i, var_j);
            ASR::expr_t *a_ji = make_array_item_2d(arr_arg,
                var_j, var_i);

            // Inner loop body: b(i, j) = a(j, i)
            Vec<ASR::stmt_t*> inner_body;
            inner_body.reserve(al, 1);
            inner_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, b_ij, a_ji,
                    nullptr, false, false)));

            // i loops over rows of b = columns of a (dim 1 of result)
            // j loops over columns of b = rows of a (dim 0 of result)
            // a(m, n): dims[0] = m, dims[1] = n
            // b(n, m): i = 1..n, j = 1..m
            ASR::do_loop_head_t inner_head;
            inner_head.loc = loc;
            inner_head.m_v = var_i;
            set_loop_head_bounds(al, loc, inner_head, dims, 1, arr_arg);
            inner_head.m_increment = nullptr;
            ASR::stmt_t *inner_loop = ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr, inner_head,
                    inner_body.p, inner_body.n, nullptr, 0));

            Vec<ASR::stmt_t*> outer_body;
            outer_body.reserve(al, 1);
            outer_body.push_back(al, inner_loop);

            ASR::do_loop_head_t outer_head;
            outer_head.loc = loc;
            outer_head.m_v = var_j;
            set_loop_head_bounds(al, loc, outer_head, dims, 0, arr_arg);
            outer_head.m_increment = nullptr;
            ASR::stmt_t *outer_loop = ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr, outer_head,
                    outer_body.p, outer_body.n, nullptr, 0));

            new_body.push_back(al, outer_loop);
            changed = true;
        }

        if (changed) {
            x.m_body = new_body.p;
            x.n_body = new_body.n;
        }
    }

    // Is `e` the integer literal `value`?
    static bool is_int_literal(ASR::expr_t *e, int64_t value) {
        if (!e) return false;
        ASR::expr_t *v = ASRUtils::expr_value(e);
        if (!v) v = e;
        if (!ASR::is_a<ASR::IntegerConstant_t>(*v)) return false;
        return ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n == value;
    }

    ASR::expr_t *int32_const(const Location &loc, int64_t n) {
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, n,
            int_type, ASR::integerbozType::Decimal));
    }

    // The loop counters generated below are integer(4); a section bound
    // of another integer kind has to be converted before it can be
    // combined with them.
    ASR::expr_t *to_int32(const Location &loc, ASR::expr_t *e) {
        ASR::ttype_t *t = ASRUtils::extract_type(ASRUtils::expr_type(e));
        if (ASR::is_a<ASR::Integer_t>(*t)
                && ASR::down_cast<ASR::Integer_t>(t)->m_kind == 4) {
            return e;
        }
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        return ASRUtils::EXPR(ASR::make_Cast_t(al, loc, e,
            ASR::cast_kindType::IntegerToInteger, int_type, nullptr,
            nullptr));
    }

    // Number of elements of a section dimension `lo:hi:step`, which is
    // (hi - lo)/step + 1. Truncating integer division gives the right
    // answer for a negative step too, since numerator and denominator
    // then have the same sign.
    ASR::expr_t *section_extent(const Location &loc,
            const ASR::array_index_t &d) {
        if (is_int_literal(d.m_left, 1) && is_int_literal(d.m_step, 1)) {
            return d.m_right;
        }
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        ASR::expr_t *span = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
            loc, to_int32(loc, d.m_right), ASR::binopType::Sub,
            to_int32(loc, d.m_left), int_type, nullptr));
        if (!is_int_literal(d.m_step, 1)) {
            span = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, span,
                ASR::binopType::Div, to_int32(loc, d.m_step), int_type,
                nullptr));
        }
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, span,
            ASR::binopType::Add, int32_const(loc, 1), int_type, nullptr));
    }

    // Array index of the `counter`-th element (counter = 1..extent) of a
    // section dimension `lo:hi:step`, which is lo + (counter - 1)*step.
    ASR::expr_t *section_index(const Location &loc,
            const ASR::array_index_t &d, ASR::expr_t *counter) {
        if (is_int_literal(d.m_left, 1) && is_int_literal(d.m_step, 1)) {
            return counter;
        }
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        ASR::expr_t *offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
            loc, counter, ASR::binopType::Sub, int32_const(loc, 1),
            int_type, nullptr));
        if (!is_int_literal(d.m_step, 1)) {
            offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                offset, ASR::binopType::Mul, to_int32(loc, d.m_step),
                int_type, nullptr));
        }
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            to_int32(loc, d.m_left), ASR::binopType::Add, offset,
            int_type, nullptr));
    }

    // Inline ArraySection assignments inside a DoConcurrentLoop body.
    // Replaces:
    //   b(1:n(l), l) = 1.0   (ArraySection = ArrayBroadcast)
    // With:
    //   do __gpu_sec_i = 1, n(l)
    //     b(__gpu_sec_i, l) = 1.0
    //   end do
    // This avoids complex lowered code (descriptor temps, ArrayBound)
    // that the Metal backend cannot handle inside GPU kernels.
    // Evaluate an integer expression made up entirely of literals.
    // `section_extent` builds its result unfolded (`(6 - 3) + 1`), and an
    // unfolded extent would make the temporary below a descriptor array
    // even though its size is known, so fold it here.
    bool eval_int_literal(ASR::expr_t *e, int64_t &out) {
        if (!e) return false;
        ASR::expr_t *v = ASRUtils::expr_value(e);
        if (v) e = v;
        if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
            out = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
            return true;
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
            int64_t a;
            if (!eval_int_literal(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, a)) {
                return false;
            }
            out = -a;
            return true;
        }
        if (ASR::is_a<ASR::Cast_t>(*e)) {
            return eval_int_literal(
                ASR::down_cast<ASR::Cast_t>(e)->m_arg, out);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            int64_t l, r;
            if (!eval_int_literal(b->m_left, l)
                    || !eval_int_literal(b->m_right, r)) {
                return false;
            }
            switch (b->m_op) {
                case ASR::binopType::Add: out = l + r; return true;
                case ASR::binopType::Sub: out = l - r; return true;
                case ASR::binopType::Mul: out = l * r; return true;
                case ASR::binopType::Div: {
                    if (r == 0) return false;
                    out = l / r;
                    return true;
                }
                default: return false;
            }
        }
        return false;
    }

    // Declare a temporary array with `n_extents` dimensions of the given
    // extents in `var_scope` and return a reference to it. A dimension
    // whose extent is not a compile-time constant makes the temporary a
    // descriptor array, exactly as the array-constructor hoisting above
    // does, so the same run-time sizing machinery applies.
    ASR::expr_t *declare_temp_array(const Location &loc,
            SymbolTable *var_scope, ASR::ttype_t *elem_type,
            ASR::expr_t **extents, size_t n_extents,
            const std::string &prefix) {
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, n_extents);
        bool all_const = true;
        for (size_t i = 0; i < n_extents; i++) {
            ASR::dimension_t d;
            d.loc = loc;
            d.m_start = int32_const(loc, 1);
            d.m_length = extents[i];
            int64_t n;
            if (extents[i] && eval_int_literal(extents[i], n)) {
                d.m_length = int32_const(loc, (int)n);
            } else {
                all_const = false;
            }
            dims.push_back(al, d);
        }
        ASR::ttype_t *tmp_type = ASRUtils::TYPE(
            ASR::make_Array_t(al, loc, elem_type, dims.p, dims.n,
                all_const
                    ? ASR::array_physical_typeType::FixedSizeArray
                    : ASR::array_physical_typeType::DescriptorArray));
        std::string name = var_scope->get_unique_name(prefix);
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, var_scope,
                s2c(al, name), nullptr, 0, ASR::intentType::Local,
                nullptr, nullptr, ASR::storage_typeType::Default,
                tmp_type, nullptr, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required,
                false));
        var_scope->add_symbol(name, sym);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }

    // The target of `asgn` when its value reads the target's storage
    // through a designator that is not element-for-element identical to
    // it, and nullptr otherwise. See the comment above
    // gpu_designator_base for why such an assignment needs a temporary.
    ASR::expr_t *self_aliasing_target(ASR::Assignment_t *asgn) {
        ASR::expr_t *target = asgn->m_target;
        while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*target)) {
            target = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                target)->m_arg;
        }
        if (!ASRUtils::is_array(ASRUtils::expr_type(target))) {
            return nullptr;
        }
        GpuDesignatorBase base = gpu_designator_base(target);
        if (!base.is_known()) return nullptr;
        GpuSelfAliasChecker checker;
        checker.base = base;
        checker.target = target;
        checker.visit_expr(*asgn->m_value);
        return checker.aliased ? target : nullptr;
    }

    // Whether the temporary such an assignment needs can be given
    // compile-time constant extents. Metal has no variable-length
    // arrays, and a run-time sized kernel temporary would have to become
    // a device buffer shared by every thread of the kernel, so a loop
    // that would need one is not offloaded at all and runs on the host.
    bool alias_temp_is_fixed_size(ASR::expr_t *target) {
        const Location &loc = target->base.loc;
        int64_t n;
        if (ASR::is_a<ASR::ArraySection_t>(*target)) {
            ASR::ArraySection_t *as =
                ASR::down_cast<ASR::ArraySection_t>(target);
            size_t n_ranges = 0;
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right
                        && as->m_args[i].m_step) {
                    n_ranges++;
                    if (!eval_int_literal(
                            section_extent(loc, as->m_args[i]), n)) {
                        return false;
                    }
                }
            }
            return n_ranges > 0;
        }
        ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(target)));
        if (!ASR::is_a<ASR::Array_t>(*tt)) return false;
        ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
        for (size_t i = 0; i < at->n_dims; i++) {
            if (!eval_int_literal(at->m_dims[i].m_length, n)) return false;
        }
        return at->n_dims > 0;
    }

    // The extents of the temporary an aliased assignment to `target`
    // needs, one per dimension, in order. False when the shape is not
    // written anywhere the temporary could be sized from, so nothing
    // could give the temporary the right extent.
    bool alias_temp_extents(ASR::expr_t *target,
            Vec<ASR::expr_t*> &extents) {
        const Location &loc = target->base.loc;
        if (ASR::is_a<ASR::ArraySection_t>(*target)) {
            ASR::ArraySection_t *as =
                ASR::down_cast<ASR::ArraySection_t>(target);
            extents.reserve(al, as->n_args);
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right
                        && as->m_args[i].m_step) {
                    extents.push_back(al,
                        section_extent(loc, as->m_args[i]));
                }
            }
            return extents.n > 0;
        }
        ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(target)));
        if (!ASR::is_a<ASR::Array_t>(*tt)) return false;
        ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
        if (at->n_dims == 0) return false;
        extents.reserve(al, at->n_dims);
        for (size_t i = 0; i < at->n_dims; i++) {
            if (!at->m_dims[i].m_length) return false;
            extents.push_back(al, at->m_dims[i].m_length);
        }
        return true;
    }

    // Reports a self-aliasing array assignment whose temporary this pass
    // cannot give a per-thread home. Called before any of the destructive
    // inline_* helpers, so the loop can still be left on the host.
    //
    // A fixed-size temporary is a kernel-scope stack array, private to
    // the thread by construction. A run-time sized one has to be a
    // BLOCK local instead, so that the workspace machinery binds it to a
    // per-thread slice of a device buffer -- and only a BLOCK at the top
    // level of the loop body is scanned for those, so only a top-level
    // assignment can have one. Whether the host can then evaluate the
    // extents is settled by the workspace pre-flight further down.
    bool body_needs_unsupported_alias_temp(ASR::stmt_t **body,
            size_t n_body, bool top_level) {
        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                if (body_needs_unsupported_alias_temp(dl->m_body,
                        dl->n_body, false)) return true;
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    if (body_needs_unsupported_alias_temp(blk->m_body,
                            blk->n_body, false)) return true;
                }
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        ASR::down_cast<ASR::AssociateBlockCall_t>(
                            stmt)->m_m);
                if (body_needs_unsupported_alias_temp(ab->m_body,
                        ab->n_body, false)) return true;
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) continue;
            ASR::expr_t *target = self_aliasing_target(
                ASR::down_cast<ASR::Assignment_t>(stmt));
            if (!target) continue;
            if (alias_temp_is_fixed_size(target)) continue;
            Vec<ASR::expr_t*> extents;
            if (top_level && alias_temp_extents(target, extents)) continue;
            return true;
        }
        return false;
    }

    // Give a materialised ASSOCIATE array temporary the shape of its
    // selector, in its own type.
    //
    // `associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))` over an
    // assumed-shape `x` becomes an allocatable local of the ASSOCIATE's
    // symbol table with deferred extents and no ALLOCATE; the shape lives
    // only in the expression assigned to it. In a kernel that local
    // becomes a per-thread workspace, which has to be sized -- and the
    // rewrites further down lower the whole-array assignment into an
    // element loop bounded by `ubound(r)`, after which the shape is gone.
    // So write it into the type here, while the assignment it can be read
    // from is still whole-array. Every replaced dimension list is
    // recorded in `undo` so the loop can still be left untouched if a
    // later check declines the offload.
    void size_scope_array_temporaries(ASR::stmt_t **body, size_t n_body,
            std::vector<ScopeArrayDims> &undo) {
        for (size_t si = 0; si < n_body; si++) {
            SymbolTable *symtab = nullptr;
            ASR::stmt_t **inner_body = nullptr;
            size_t inner_n_body = 0;
            if (ASR::is_a<ASR::BlockCall_t>(*body[si])) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(body[si])->m_m);
                if (!b || !ASR::is_a<ASR::Block_t>(*b)) continue;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                symtab = blk->m_symtab;
                inner_body = blk->m_body;
                inner_n_body = blk->n_body;
            } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*body[si])) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        body[si])->m_m);
                if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) continue;
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                symtab = ab->m_symtab;
                inner_body = ab->m_body;
                inner_n_body = ab->n_body;
            } else if (ASR::is_a<ASR::DoLoop_t>(*body[si])) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(body[si]);
                size_scope_array_temporaries(dl->m_body, dl->n_body, undo);
                continue;
            } else {
                continue;
            }
            for (auto &item : symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                ASR::expr_t *shape_src = gpu_scope_array_shape_source(
                    var, inner_body, inner_n_body);
                if (!shape_src) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable(var->m_type));
                const Location &vloc = var->base.base.loc;
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, arr->n_dims);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    ASR::dimension_t dim;
                    dim.loc = vloc;
                    dim.m_start = int32_const(vloc, 1);
                    dim.m_length = ASRUtils::get_size(shape_src,
                        (int)d + 1, al);
                    dims.push_back(al, dim);
                }
                // An allocatable must keep deferred extents, so the
                // temporary becomes an automatic array of the same
                // shape -- which is what the workspace machinery binds.
                undo.push_back({var, var->m_type});
                var->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, vloc,
                    arr->m_type, dims.p, dims.n,
                    ASR::array_physical_typeType::DescriptorArray));
            }
            size_scope_array_temporaries(inner_body, inner_n_body, undo);
        }
    }

    // Give the temporary of a run-time sized aliased assignment a BLOCK
    // of its own, at the top level of the loop body.
    //
    //   a(:,c) = a(n:1:-1,c)
    // becomes
    //   block
    //     real :: __gpu_alias(n)
    //     __gpu_alias(1:n) = a(n:1:-1,c)
    //     a(:,c)           = __gpu_alias(1:n)
    //   end block
    //
    // The BLOCK is what makes the temporary safe: a run-time sized
    // kernel-scope local becomes one device buffer shared by every
    // thread, and every thread would write it -- a race. A BLOCK local is
    // bound to a per-thread slice of a workspace buffer instead
    // (analyze_gpu_vla_workspaces), which is private to the iteration.
    // Only a top-level BLOCK of the kernel body is scanned for
    // workspaces, which is why only a top-level assignment is rewritten
    // here; body_needs_unsupported_alias_temp has already declined the
    // rest.
    void materialize_runtime_alias_blocks(ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body);
        bool changed = false;
        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmt);
            ASR::expr_t *target = self_aliasing_target(asgn);
            if (!target || alias_temp_is_fixed_size(target)) {
                new_body.push_back(al, stmt);
                continue;
            }
            Vec<ASR::expr_t*> extents;
            if (!alias_temp_extents(target, extents)) {
                new_body.push_back(al, stmt);
                continue;
            }
            Location loc = stmt->base.loc;
            SymbolTable *block_scope =
                al.make_new<SymbolTable>(current_scope);
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(target));
            ASR::expr_t *tmp = declare_temp_array(loc, block_scope,
                elem_type, extents.p, extents.n, "__gpu_alias");
            bool sectioned = ASR::is_a<ASR::ArraySection_t>(*target);
            // A full 1:extent:1 section over the temporary, built once
            // per use so the two statements do not share nodes.
            auto tmp_ref = [&]() -> ASR::expr_t* {
                if (!sectioned) return tmp;
                Vec<ASR::array_index_t> args;
                args.reserve(al, extents.n);
                for (size_t i = 0; i < extents.n; i++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = int32_const(loc, 1);
                    idx.m_right = extents[i];
                    idx.m_step = int32_const(loc, 1);
                    args.push_back(al, idx);
                }
                return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                    tmp, args.p, args.n, ASRUtils::expr_type(tmp),
                    nullptr));
            };
            Vec<ASR::stmt_t*> block_body;
            block_body.reserve(al, 2);
            block_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp_ref(),
                    asgn->m_value, nullptr, false, false)));
            asgn->m_value = tmp_ref();
            block_body.push_back(al, stmt);
            std::string block_name = current_scope->get_unique_name(
                "__gpu_alias_scope");
            ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
                s2c(al, block_name), block_body.p, block_body.n);
            block_scope->asr_owner = block;
            ASR::symbol_t *block_sym =
                ASR::down_cast<ASR::symbol_t>(block);
            current_scope->add_symbol(block_name, block_sym);
            new_body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
                al, loc, -1, block_sym)));
            changed = true;
        }
        if (changed) {
            x.m_body = new_body.p;
            x.n_body = new_body.n;
        }
    }

    // Materialise a temporary for an array assignment whose target and
    // value designate overlapping storage of the same array. See the
    // comment above gpu_designator_base: without it the element loops
    // built below read elements the same statement has already written.
    //   a(:) = a(n:1:-1)
    // becomes
    //   __gpu_alias(1:n) = a(n:1:-1)
    //   a(:)            = __gpu_alias(1:n)
    // Both halves are alias-free, and the existing ArraySection and
    // whole-array lowerings turn each of them into an element loop.
    void materialize_aliased_assignments(ASR::DoConcurrentLoop_t &x) {
        bool changed = false;
        materialize_aliased_in_body(x.m_body, x.n_body, changed);
    }

    void materialize_aliased_in_body(ASR::stmt_t** &body, size_t &n_body,
            bool &changed) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 2);
        bool local_changed = false;

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                materialize_aliased_in_body(dl->m_body, dl->n_body, changed);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    materialize_aliased_in_body(blk->m_body, blk->n_body,
                        changed);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        ASR::down_cast<ASR::AssociateBlockCall_t>(
                            stmt)->m_m);
                materialize_aliased_in_body(ab->m_body, ab->n_body,
                    changed);
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            ASR::expr_t *target = self_aliasing_target(asgn);
            // A loop holding an assignment that needs a temporary this
            // pass cannot size was already declined for offload, so the
            // second test only guards the statements spliced in since.
            if (!target || !alias_temp_is_fixed_size(target)) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(target));

            if (ASR::is_a<ASR::ArraySection_t>(*target)) {
                ASR::ArraySection_t *as =
                    ASR::down_cast<ASR::ArraySection_t>(target);
                Vec<ASR::expr_t*> extents;
                extents.reserve(al, as->n_args);
                for (size_t i = 0; i < as->n_args; i++) {
                    if (as->m_args[i].m_left && as->m_args[i].m_right
                            && as->m_args[i].m_step) {
                        extents.push_back(al,
                            section_extent(loc, as->m_args[i]));
                    }
                }
                if (extents.n == 0) {
                    new_body.push_back(al, stmt);
                    continue;
                }
                ASR::expr_t *tmp = declare_temp_array(loc, var_scope,
                    elem_type, extents.p, extents.n, "__gpu_alias");
                // A full 1:extent:1 section over the temporary, built
                // twice so the two statements do not share nodes.
                auto tmp_section = [&]() -> ASR::expr_t* {
                    Vec<ASR::array_index_t> args;
                    args.reserve(al, extents.n);
                    for (size_t i = 0; i < extents.n; i++) {
                        ASR::array_index_t idx;
                        idx.loc = loc;
                        idx.m_left = int32_const(loc, 1);
                        idx.m_right = extents[i];
                        idx.m_step = int32_const(loc, 1);
                        args.push_back(al, idx);
                    }
                    return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                        tmp, args.p, args.n,
                        ASRUtils::expr_type(tmp), nullptr));
                };
                new_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, tmp_section(),
                        asgn->m_value, nullptr, false, false)));
                asgn->m_value = tmp_section();
            } else {
                Vec<ASR::expr_t*> extents;
                ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(
                        ASRUtils::expr_type(target)));
                ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
                extents.reserve(al, at->n_dims);
                for (size_t i = 0; i < at->n_dims; i++) {
                    extents.push_back(al, at->m_dims[i].m_length);
                }
                ASR::expr_t *tmp = declare_temp_array(loc, var_scope,
                    elem_type, extents.p, extents.n, "__gpu_alias");
                new_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, tmp, asgn->m_value,
                        nullptr, false, false)));
                asgn->m_value = tmp;
            }
            new_body.push_back(al, stmt);
            local_changed = true;
        }

        if (local_changed) {
            body = new_body.p;
            n_body = new_body.n;
            changed = true;
        }
    }

    void inline_array_section_assignment(ASR::DoConcurrentLoop_t &x) {
        bool changed = false;
        inline_array_section_in_body(x.m_body, x.n_body, changed);
    }

    void inline_array_section_in_body(ASR::stmt_t** &body, size_t &n_body,
            bool &changed) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 2);

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            // Recurse into DoLoop bodies
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_array_section_in_body(dl->m_body, dl->n_body,
                    changed);
                new_body.push_back(al, stmt);
                continue;
            }
            // Recurse into BlockCall bodies
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_array_section_in_body(block->m_body,
                        block->n_body, changed);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            // Recurse into AssociateBlockCall bodies
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                inline_array_section_in_body(ab->m_body,
                    ab->n_body, changed);
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::ArraySection_t>(*asgn->m_target)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(
                asgn->m_target);

            // Collect all range dimensions (have m_left, m_right, m_step
            // set, meaning it's a slice like 1:n, not a scalar index)
            std::vector<int> range_dims;
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right
                        && as->m_args[i].m_step) {
                    range_dims.push_back((int)i);
                }
            }
            if (range_dims.empty()) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            // Create loop variable(s) in the containing function/program
            // scope, not in any enclosing AssociateBlock scope.
            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            // Create a loop variable for each range dimension
            std::vector<ASR::expr_t*> loop_vars(range_dims.size());
            for (size_t ri = 0; ri < range_dims.size(); ri++) {
                std::string loop_var_name = var_scope->get_unique_name(
                    "__gpu_sec_i");
                ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, loop_var_name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(loop_var_name, loop_var_sym);
                loop_vars[ri] = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, loop_var_sym));
            }

            // Build ArrayItem: replace each range dim with its loop var,
            // keep scalar-index dims as-is
            Vec<ASR::array_index_t> new_args;
            new_args.reserve(al, as->n_args);
            for (size_t i = 0; i < as->n_args; i++) {
                ASR::array_index_t idx;
                idx.loc = as->m_args[i].loc;
                // Check if this dimension is a range dimension
                bool is_range = false;
                for (size_t ri = 0; ri < range_dims.size(); ri++) {
                    if ((int)i == range_dims[ri]) {
                        idx.m_left = nullptr;
                        idx.m_right = section_index(loc, as->m_args[i],
                            loop_vars[ri]);
                        idx.m_step = nullptr;
                        is_range = true;
                        break;
                    }
                }
                if (!is_range) {
                    idx.m_left = as->m_args[i].m_left;
                    idx.m_right = as->m_args[i].m_right;
                    idx.m_step = as->m_args[i].m_step;
                }
                new_args.push_back(al, idx);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(as->m_v));
            ASR::expr_t *array_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, as->m_v,
                    new_args.p, new_args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));

            // Elementize: recursively replace ArraySection with
            // ArrayItem and unwrap ArrayBroadcast in the RHS
            std::function<ASR::expr_t*(ASR::expr_t*)> elementize_rhs =
                [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *rhs_as =
                        ASR::down_cast<ASR::ArraySection_t>(e);
                    Vec<ASR::array_index_t> rhs_new_args;
                    rhs_new_args.reserve(al, rhs_as->n_args);
                    size_t rv_idx = 0;
                    for (size_t i = 0; i < rhs_as->n_args; i++) {
                        ASR::array_index_t idx;
                        idx.loc = rhs_as->m_args[i].loc;
                        if (rhs_as->m_args[i].m_left &&
                                rhs_as->m_args[i].m_right &&
                                rhs_as->m_args[i].m_step) {
                            if (rv_idx < loop_vars.size()) {
                                idx.m_left = nullptr;
                                idx.m_right = section_index(loc,
                                    rhs_as->m_args[i], loop_vars[rv_idx]);
                                idx.m_step = nullptr;
                                rv_idx++;
                            } else {
                                idx = rhs_as->m_args[i];
                            }
                        } else {
                            idx.m_left = rhs_as->m_args[i].m_left;
                            idx.m_right = rhs_as->m_args[i].m_right;
                            idx.m_step = rhs_as->m_args[i].m_step;
                        }
                        rhs_new_args.push_back(al, idx);
                    }
                    ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                        ASRUtils::expr_type(rhs_as->m_v));
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        rhs_as->m_v, rhs_new_args.p, rhs_new_args.n,
                        rhs_elem, ASR::arraystorageType::ColMajor,
                        nullptr));
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    return ASR::down_cast<ASR::ArrayBroadcast_t>(
                        e)->m_array;
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb =
                        ASR::down_cast<ASR::RealBinOp_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealBinOp_t(al,
                        loc, elementize_rhs(rb->m_left), rb->m_op,
                        elementize_rhs(rb->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib =
                        ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
                        loc, elementize_rhs(ib->m_left), ib->m_op,
                        elementize_rhs(ib->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                    ASR::RealUnaryMinus_t *ru =
                        ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al,
                        loc, elementize_rhs(ru->m_arg), et, nullptr));
                } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                    ASR::IntegerUnaryMinus_t *iu =
                        ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al,
                        loc, elementize_rhs(iu->m_arg), et, nullptr));
                } else if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
                    ASR::LogicalNot_t *ln =
                        ASR::down_cast<ASR::LogicalNot_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_LogicalNot_t(al,
                        loc, elementize_rhs(ln->m_arg), et, nullptr));
                } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                    ASR::RealCompare_t *rc =
                        ASR::down_cast<ASR::RealCompare_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealCompare_t(al,
                        loc, elementize_rhs(rc->m_left), rc->m_op,
                        elementize_rhs(rc->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                    ASR::IntegerCompare_t *ic =
                        ASR::down_cast<ASR::IntegerCompare_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al,
                        loc, elementize_rhs(ic->m_left), ic->m_op,
                        elementize_rhs(ic->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                    ASR::LogicalCompare_t *lc =
                        ASR::down_cast<ASR::LogicalCompare_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al,
                        loc, elementize_rhs(lc->m_left), lc->m_op,
                        elementize_rhs(lc->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(
                        *e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<
                            ASR::IntrinsicElementalFunction_t>(e);
                    Vec<ASR::expr_t*> new_fargs;
                    new_fargs.reserve(al, f->n_args);
                    for (size_t i = 0; i < f->n_args; i++) {
                        new_fargs.push_back(al,
                            f->m_args[i]
                                ? elementize_rhs(f->m_args[i])
                                : nullptr);
                    }
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntrinsicElementalFunction_t(al,
                            loc, f->m_intrinsic_id, new_fargs.p,
                            new_fargs.n, f->m_overload_id, et,
                            f->m_value));
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(e);
                    // Check if the function natively returns an array
                    // (not an elemental function with array args).
                    // In that case, keep the original return type and
                    // wrap with ArrayItem below.
                    ASR::ttype_t *fc_type = ASRUtils::type_get_past_allocatable(
                        fc->m_type);
                    ASR::Function_t *fn = ASRUtils::get_function(fc->m_name);
                    ASR::ttype_t *fn_ret = fn
                        ? ASRUtils::get_FunctionType(fn)->m_return_var_type
                        : nullptr;
                    bool fn_returns_array = fn_ret &&
                        ASR::is_a<ASR::Array_t>(
                            *ASRUtils::type_get_past_allocatable(fn_ret));
                    ASR::FunctionType_t *fn_type = fn
                        ? ASRUtils::get_FunctionType(fn) : nullptr;
                    Vec<ASR::call_arg_t> new_fargs;
                    new_fargs.reserve(al, fc->n_args);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        ASR::call_arg_t arg;
                        arg.loc = fc->m_args[i].loc;
                        if (!fc->m_args[i].m_value) {
                            arg.m_value = nullptr;
                        } else if (fn_returns_array && fn_type
                                && i < fn_type->n_arg_types
                                && ASR::is_a<ASR::Array_t>(
                                    *ASRUtils::type_get_past_allocatable(
                                        fn_type->m_arg_types[i]))) {
                            // Keep array arguments as-is for functions
                            // that return arrays; elementizing would
                            // turn ArraySection into scalar ArrayItem,
                            // breaking the function's array contract.
                            arg.m_value = fc->m_args[i].m_value;
                        } else {
                            arg.m_value = elementize_rhs(
                                fc->m_args[i].m_value);
                        }
                        new_fargs.push_back(al, arg);
                    }
                    if (fn_returns_array && ASR::is_a<ASR::Array_t>(*fc_type)) {
                        ASR::expr_t *new_fc = ASRUtils::EXPR(
                            ASR::make_FunctionCall_t(al, loc,
                                fc->m_name, fc->m_original_name,
                                new_fargs.p, new_fargs.n, fc->m_type,
                                fc->m_value, fc->m_dt));
                        Vec<ASR::array_index_t> rhs_args;
                        rhs_args.reserve(al, range_dims.size());
                        for (size_t ri = 0; ri < range_dims.size(); ri++) {
                            ASR::array_index_t idx;
                            idx.loc = loc;
                            idx.m_left = nullptr;
                            idx.m_right = loop_vars[ri];
                            idx.m_step = nullptr;
                            rhs_args.push_back(al, idx);
                        }
                        ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                            fc->m_type);
                        return ASRUtils::EXPR(
                            ASR::make_ArrayItem_t(al, loc, new_fc,
                                rhs_args.p, rhs_args.n, rhs_elem,
                                ASR::arraystorageType::ColMajor, nullptr));
                    }
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_FunctionCall_t(al, loc,
                            fc->m_name, fc->m_original_name,
                            new_fargs.p, new_fargs.n, et,
                            fc->m_value, fc->m_dt));
                } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    return elementize_rhs(
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            e)->m_arg);
                } else if (ASR::is_a<ASR::Cast_t>(*e)) {
                    ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
                    ASR::ttype_t *ct = c->m_type;
                    if (ASR::is_a<ASR::Array_t>(*ct)) {
                        ct = ASRUtils::extract_type(ct);
                    }
                    return ASRUtils::EXPR(ASR::make_Cast_t(al, loc,
                        elementize_rhs(c->m_arg), c->m_kind, ct,
                        c->m_value, nullptr));
                }
                // Fallback: if still array-typed, wrap with ArrayItem
                ASR::ttype_t *e_type = ASRUtils::expr_type(e);
                ASR::ttype_t *e_type_inner =
                    ASRUtils::type_get_past_allocatable(e_type);
                if (ASR::is_a<ASR::Array_t>(*e_type_inner)) {
                    Vec<ASR::array_index_t> rhs_args;
                    rhs_args.reserve(al, range_dims.size());
                    for (size_t ri = 0; ri < range_dims.size(); ri++) {
                        ASR::array_index_t idx;
                        idx.loc = loc;
                        idx.m_left = nullptr;
                        idx.m_right = loop_vars[ri];
                        idx.m_step = nullptr;
                        rhs_args.push_back(al, idx);
                    }
                    ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                        e_type);
                    return ASRUtils::EXPR(
                        ASR::make_ArrayItem_t(al, loc, e,
                            rhs_args.p, rhs_args.n, rhs_elem,
                            ASR::arraystorageType::ColMajor, nullptr));
                }
                return e;
            };
            ASR::expr_t *scalar_value = elementize_rhs(asgn->m_value);

            // Build innermost loop body: array_item = scalar_value
            Vec<ASR::stmt_t*> inner_body;
            inner_body.reserve(al, 1);
            inner_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, array_item, scalar_value,
                    nullptr, false, false)));

            // Build nested DoLoops from innermost to outermost
            ASR::stmt_t *loop_stmt = nullptr;
            for (int ri = (int)range_dims.size() - 1; ri >= 0; ri--) {
                int dim = range_dims[ri];
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = loop_vars[ri];
                head.m_start = int32_const(loc, 1);
                head.m_end = section_extent(loc, as->m_args[dim]);
                head.m_increment = nullptr;

                Vec<ASR::stmt_t*> body;
                body.reserve(al, 1);
                if (loop_stmt) {
                    body.push_back(al, loop_stmt);
                } else {
                    body.push_back(al, inner_body[0]);
                }
                loop_stmt = ASRUtils::STMT(
                    ASR::make_DoLoop_t(al, loc, nullptr,
                        head, body.p, body.n, nullptr, 0));
            }
            new_body.push_back(al, loop_stmt);

            changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }


    // ---- gather/scatter for a strided section actual argument ----
    //
    // A strided array section passed as an actual argument --
    // `s3(a(j:j+4:2))` -- reaches a device function as a bare base pointer
    // plus an element count. A device pointer cannot express a stride, so
    // the callee reads the contiguous run a(j), a(j+1), a(j+2) instead of
    // every second element: finite numbers, wrong answers, no diagnostic.
    //
    // Gather the section into a contiguous temporary before the call, pass
    // the temporary, and scatter it back afterwards when the dummy may be
    // written. The temporary is an ordinary loop-body local, so each thread
    // gets its own copy through the machinery that already sizes and slices
    // kernel locals.
    //
    // A unit-stride section is left alone: base pointer plus element count
    // describes it exactly, and that path must stay as cheap as it is.
    // Linear form of an integer expression: a constant plus integer
    // multiples of scalar variables. It exists to fold `(j + 4) - j` to 4,
    // which makes the extent of a section like `a(j:j+4:2)` a compile-time
    // constant even though neither of its bounds is one.
    struct LinearForm {
        int64_t constant = 0;
        std::map<ASR::symbol_t*, int64_t> terms;
    };

    bool linear_form(ASR::expr_t *e, LinearForm &f, int64_t scale) {
        if (!e) return false;
        ASR::expr_t *v = ASRUtils::expr_value(e);
        if (v) e = v;
        if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
            f.constant += scale
                * ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
            return true;
        }
        if (ASR::is_a<ASR::Cast_t>(*e)) {
            return linear_form(ASR::down_cast<ASR::Cast_t>(e)->m_arg, f,
                scale);
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
            return linear_form(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, f,
                -scale);
        }
        if (ASR::is_a<ASR::Var_t>(*e)) {
            f.terms[ASR::down_cast<ASR::Var_t>(e)->m_v] += scale;
            return true;
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            int64_t c;
            switch (b->m_op) {
                case ASR::binopType::Add:
                    return linear_form(b->m_left, f, scale)
                        && linear_form(b->m_right, f, scale);
                case ASR::binopType::Sub:
                    return linear_form(b->m_left, f, scale)
                        && linear_form(b->m_right, f, -scale);
                case ASR::binopType::Mul:
                    if (eval_int_literal(b->m_left, c)) {
                        return linear_form(b->m_right, f, scale * c);
                    }
                    if (eval_int_literal(b->m_right, c)) {
                        return linear_form(b->m_left, f, scale * c);
                    }
                    return false;
                default: return false;
            }
        }
        return false;
    }

    // Number of elements of the section dimension `lo:hi:step` when it is
    // a compile-time constant, which it is whenever `hi - lo` and `step`
    // are -- the bounds themselves need not be.
    bool const_section_extent(const ASR::array_index_t &d, int64_t &n) {
        int64_t step;
        if (!d.m_left || !d.m_right || !d.m_step) return false;
        if (!eval_int_literal(d.m_step, step) || step == 0) return false;
        LinearForm f;
        if (!linear_form(d.m_right, f, 1)) return false;
        if (!linear_form(d.m_left, f, -1)) return false;
        for (auto &t : f.terms) {
            if (t.second != 0) return false;
        }
        n = f.constant / step + 1;
        if (n < 0) n = 0;
        return true;
    }

    static bool section_is_strided(const ASR::ArraySection_t *as) {
        for (size_t i = 0; i < as->n_args; i++) {
            if (!as->m_args[i].m_left || !as->m_args[i].m_right
                    || !as->m_args[i].m_step) {
                continue;
            }
            if (!is_int_literal(as->m_args[i].m_step, 1)) return true;
        }
        return false;
    }

    // A dummy the callee may write has to be copied back. An unknown
    // intent is treated as writable: a wrong answer is worse than a copy.
    static bool dummy_is_written(ASR::Function_t *fn, size_t arg_index) {
        if (!fn || arg_index >= fn->n_args) return true;
        if (!ASR::is_a<ASR::Var_t>(*fn->m_args[arg_index])) return true;
        ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(
            fn->m_args[arg_index])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*s)) return true;
        return ASR::down_cast<ASR::Variable_t>(s)->m_intent
            != ASR::intentType::In;
    }

    // Build `do c = 1, extent ... end do` nests copying between the
    // section `as` of its base array and the contiguous temporary `tmp`.
    // With `to_temp` the section is read into the temporary (gather);
    // otherwise the temporary is written back into the section (scatter).
    ASR::stmt_t* build_section_copy_loops(const Location &loc,
            SymbolTable *var_scope, ASR::ArraySection_t *as,
            const std::vector<int> &range_dims, ASR::expr_t *tmp,
            bool to_temp) {
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        std::vector<ASR::expr_t*> counters(range_dims.size());
        for (size_t ri = 0; ri < range_dims.size(); ri++) {
            std::string name = var_scope->get_unique_name("__gpu_gather_i");
            ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, var_scope,
                    s2c(al, name), nullptr, 0, ASR::intentType::Local,
                    nullptr, nullptr, ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type), nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            var_scope->add_symbol(name, sym);
            counters[ri] = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(as->m_v));

        // The section element for this iteration.
        Vec<ASR::array_index_t> src_args;
        src_args.reserve(al, as->n_args);
        size_t ri = 0;
        for (size_t d = 0; d < as->n_args; d++) {
            ASR::array_index_t idx;
            idx.loc = as->m_args[d].loc;
            bool is_range = ri < range_dims.size()
                && (int)d == range_dims[ri];
            if (is_range) {
                idx.m_left = nullptr;
                idx.m_right = section_index(loc, as->m_args[d],
                    counters[ri]);
                idx.m_step = nullptr;
                ri++;
            } else {
                idx.m_left = as->m_args[d].m_left;
                idx.m_right = as->m_args[d].m_right;
                idx.m_step = as->m_args[d].m_step;
            }
            src_args.push_back(al, idx);
        }
        ASR::expr_t *src_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(al,
            loc, as->m_v, src_args.p, src_args.n, elem_type,
            ASR::arraystorageType::ColMajor, nullptr));

        // The temporary's element for the same iteration: the temporary is
        // 1-based and contiguous, so the counters index it directly.
        Vec<ASR::array_index_t> tmp_args;
        tmp_args.reserve(al, range_dims.size());
        for (size_t k = 0; k < range_dims.size(); k++) {
            ASR::array_index_t idx;
            idx.loc = loc;
            idx.m_left = nullptr;
            idx.m_right = counters[k];
            idx.m_step = nullptr;
            tmp_args.push_back(al, idx);
        }
        ASR::expr_t *tmp_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(al,
            loc, tmp, tmp_args.p, tmp_args.n, elem_type,
            ASR::arraystorageType::ColMajor, nullptr));

        ASR::stmt_t *inner = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            to_temp ? tmp_elem : src_elem,
            to_temp ? src_elem : tmp_elem, nullptr, false, false));
        for (int k = (int)range_dims.size() - 1; k >= 0; k--) {
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = counters[k];
            head.m_start = int32_const(loc, 1);
            head.m_end = section_extent(loc, as->m_args[range_dims[k]]);
            head.m_increment = nullptr;
            Vec<ASR::stmt_t*> body;
            body.reserve(al, 1);
            body.push_back(al, inner);
            inner = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                head, body.p, body.n, nullptr, 0));
        }
        return inner;
    }

    // The strided section under any physical casts of an actual argument,
    // or nullptr when the argument is not one.
    static ASR::ArraySection_t* strided_section_actual(ASR::expr_t *e) {
        while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        }
        if (!e || !ASR::is_a<ASR::ArraySection_t>(*e)) return nullptr;
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
        return section_is_strided(as) ? as : nullptr;
    }

    // Can this strided section be gathered into a contiguous temporary?
    // The base has to be a designator the copy loops can index, and every
    // extent has to fold to a constant, because the temporary is a
    // kernel-local array.
    bool strided_section_is_gatherable(ASR::ArraySection_t *as) {
        if (!ASR::is_a<ASR::Var_t>(*as->m_v)
                && !ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
            return false;
        }
        bool any_range = false;
        for (size_t d = 0; d < as->n_args; d++) {
            if (!as->m_args[d].m_left || !as->m_args[d].m_right
                    || !as->m_args[d].m_step) {
                continue;
            }
            any_range = true;
            int64_t n;
            if (!const_section_extent(as->m_args[d], n)) return false;
        }
        return any_range;
    }

    // Replace a strided section actual argument in `slot` with a gathered
    // temporary, appending the gather to `before` and, when the dummy may
    // be written, the scatter to `after`. Returns true when it did.
    bool gather_strided_section_arg(const Location &loc,
            SymbolTable *block_scope, ASR::expr_t **slot, bool writable,
            std::vector<ASR::stmt_t*> &before,
            std::vector<ASR::stmt_t*> &after) {
        ASR::ArrayPhysicalCast_t *cast = nullptr;
        ASR::expr_t *inner = *slot;
        while (inner && ASR::is_a<ASR::ArrayPhysicalCast_t>(*inner)) {
            cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(inner);
            inner = cast->m_arg;
        }
        if (!inner || !ASR::is_a<ASR::ArraySection_t>(*inner)) return false;
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(inner);
        if (!section_is_strided(as)) return false;
        if (!strided_section_is_gatherable(as)) return false;
        std::vector<int> range_dims;
        for (size_t d = 0; d < as->n_args; d++) {
            if (as->m_args[d].m_left && as->m_args[d].m_right
                    && as->m_args[d].m_step) {
                range_dims.push_back((int)d);
            }
        }
        if (range_dims.empty()) return false;

        // Every extent has to fold to a constant: the gathered buffer is a
        // kernel-local array, and a device function cannot declare one
        // whose size is only known per thread.
        Vec<ASR::expr_t*> extents;
        extents.reserve(al, range_dims.size());
        for (int d : range_dims) {
            int64_t n = 0;
            const_section_extent(as->m_args[d], n);
            extents.push_back(al, int32_const(loc, (int)n));
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(as->m_v));
        ASR::expr_t *tmp = declare_temp_array(loc, block_scope, elem_type,
            extents.p, extents.n, "__gpu_gather");

        before.push_back(build_section_copy_loops(loc, block_scope, as,
            range_dims, tmp, true));
        if (writable) {
            after.push_back(build_section_copy_loops(loc, block_scope, as,
                range_dims, tmp, false));
        }
        // The temporary is contiguous, so no physical-type cast is left to
        // make: the dummy takes the array as it stands.
        *slot = tmp;
        (void)cast;
        return true;
    }

    // Rewrite every strided section actual argument of every call in
    // `stmt`, collecting the gather and scatter statements that have to
    // bracket it.
    bool gather_strided_sections_in_stmt(ASR::stmt_t *stmt,
            SymbolTable *block_scope, std::vector<ASR::stmt_t*> &before,
            std::vector<ASR::stmt_t*> &after) {
        bool changed = false;
        const Location &loc = stmt->base.loc;
        auto do_call = [&](ASR::symbol_t *name, ASR::call_arg_t *args,
                size_t n_args) {
            ASR::symbol_t *resolved =
                ASRUtils::symbol_get_past_external(name);
            ASR::Function_t *fn =
                (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                    ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
            for (size_t i = 0; i < n_args; i++) {
                if (!args[i].m_value) continue;
                if (gather_strided_section_arg(loc, block_scope,
                        &args[i].m_value, dummy_is_written(fn, i),
                        before, after)) {
                    changed = true;
                }
            }
        };
        if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
            ASR::SubroutineCall_t *sc =
                ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            do_call(sc->m_name, sc->m_args, sc->n_args);
        }
        GpuCallSiteCollector csc;
        csc.visit_stmt(*stmt);
        for (const ASR::FunctionCall_t *c : csc.calls) {
            ASR::FunctionCall_t *fc = const_cast<ASR::FunctionCall_t*>(c);
            do_call(fc->m_name, fc->m_args, fc->n_args);
        }
        return changed;
    }

    // True when some call in `body` takes a strided section this pass
    // cannot gather. Passing it on would drop the stride silently, so the
    // loop is declined for offload instead, while the body is untouched.
    bool body_has_ungatherable_strided_section(ASR::stmt_t **body,
            size_t n_body) {
        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                if (body_has_ungatherable_strided_section(dl->m_body,
                        dl->n_body)) return true;
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    if (body_has_ungatherable_strided_section(blk->m_body,
                            blk->n_body)) return true;
                }
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(b);
                    if (body_has_ungatherable_strided_section(ab->m_body,
                            ab->n_body)) return true;
                }
                continue;
            }
            std::vector<ASR::call_arg_t*> arg_lists;
            std::vector<size_t> arg_counts;
            if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
                ASR::SubroutineCall_t *sc =
                    ASR::down_cast<ASR::SubroutineCall_t>(stmt);
                arg_lists.push_back(sc->m_args);
                arg_counts.push_back(sc->n_args);
            }
            GpuCallSiteCollector csc;
            csc.visit_stmt(*stmt);
            for (const ASR::FunctionCall_t *c : csc.calls) {
                arg_lists.push_back(c->m_args);
                arg_counts.push_back(c->n_args);
            }
            for (size_t li = 0; li < arg_lists.size(); li++) {
                for (size_t i = 0; i < arg_counts[li]; i++) {
                    if (!arg_lists[li][i].m_value) continue;
                    ASR::ArraySection_t *as = strided_section_actual(
                        arg_lists[li][i].m_value);
                    if (as && !strided_section_is_gatherable(as)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    void gather_strided_section_arguments(ASR::DoConcurrentLoop_t &x) {
        gather_strided_sections_in_body(x.m_body, x.n_body, current_scope);
    }

    // `scope` owns the statements in `body`: the new BLOCK has to be
    // registered there, not in the procedure, or it will not resolve from
    // the BlockCall that replaces the statement.
    void gather_strided_sections_in_body(ASR::stmt_t** &body,
            size_t &n_body, SymbolTable *scope) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body);
        bool changed = false;
        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                gather_strided_sections_in_body(dl->m_body, dl->n_body,
                    scope);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    gather_strided_sections_in_body(blk->m_body,
                        blk->n_body, blk->m_symtab);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(b);
                    gather_strided_sections_in_body(ab->m_body,
                        ab->n_body, ab->m_symtab);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            // The gathered buffers and their loop counters live in a
            // BLOCK scope nested inside the loop. A variable owned by such
            // a scope travels into the kernel with the block instead of
            // becoming a kernel parameter, which is what makes the buffer
            // per-thread: one shared buffer written by every thread would
            // be a race.
            SymbolTable *block_scope = al.make_new<SymbolTable>(scope);
            std::vector<ASR::stmt_t*> before, after;
            if (!gather_strided_sections_in_stmt(stmt, block_scope, before,
                    after)) {
                new_body.push_back(al, stmt);
                continue;
            }
            Vec<ASR::stmt_t*> block_body;
            block_body.reserve(al, before.size() + after.size() + 1);
            for (ASR::stmt_t *s : before) block_body.push_back(al, s);
            block_body.push_back(al, stmt);
            for (ASR::stmt_t *s : after) block_body.push_back(al, s);
            std::string block_name = scope->get_unique_name(
                "__gpu_gather_scope");
            ASR::asr_t *block = ASR::make_Block_t(al, stmt->base.loc,
                block_scope, s2c(al, block_name), block_body.p,
                block_body.n);
            block_scope->asr_owner = block;
            ASR::symbol_t *block_sym =
                ASR::down_cast<ASR::symbol_t>(block);
            scope->add_symbol(block_name, block_sym);
            new_body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
                al, stmt->base.loc, -1, block_sym)));
            changed = true;
        }
        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    // Inline whole-array assignments whose RHS contains ArraySection
    // wrapped in elemental operations (e.g., b = abs(a(:,l))).
    // Replaces:
    //   b = abs(a(:,l))
    // With:
    //   do __gpu_elem_i = lbound(a,1), ubound(a,1)
    //     b(__gpu_elem_i) = abs(a(__gpu_elem_i, l))
    //   end do
    void inline_elemental_array_var_assignment(ASR::DoConcurrentLoop_t &x) {
        bool changed = false;
        inline_elemental_array_var_in_body(x.m_body, x.n_body, changed);
    }

    void inline_elemental_array_var_in_body(ASR::stmt_t** &body,
            size_t &n_body, bool &changed) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 2);

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            // Recurse into DoLoop bodies
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_elemental_array_var_in_body(dl->m_body, dl->n_body,
                    changed);
                new_body.push_back(al, stmt);
                continue;
            }
            // Recurse into BlockCall bodies
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_elemental_array_var_in_body(block->m_body,
                        block->n_body, changed);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            // Recurse into AssociateBlockCall bodies
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                inline_elemental_array_var_in_body(ab->m_body,
                    ab->n_body, changed);
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

            // An array constructor is not an elementwise expression: its
            // i-th element does not come from the i-th element of each
            // operand. Rewriting `r = [a, b]` as `r(i) = [a, b]` would
            // make the backend emit a whole constructor per element. The
            // Metal backend already expands a whole-array constructor
            // assignment into element writes, so leave it alone.
            {
                ASR::expr_t *rhs = asgn->m_value;
                while (rhs && ASR::is_a<ASR::ArrayPhysicalCast_t>(*rhs)) {
                    rhs = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                        rhs)->m_arg;
                }
                if (rhs && ASR::is_a<ASR::ArrayConstructor_t>(*rhs)) {
                    new_body.push_back(al, stmt);
                    continue;
                }
            }

            // Only handle Var targets with array type
            if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::ttype_t *target_type = ASRUtils::type_get_past_allocatable(
                ASRUtils::expr_type(asgn->m_target));
            if (!ASR::is_a<ASR::Array_t>(*target_type)) {
                new_body.push_back(al, stmt);
                continue;
            }

            // Walk the RHS to find the first ArraySection
            ASR::ArraySection_t *first_as = nullptr;
            std::function<void(ASR::expr_t*)> find_array_section =
                [&](ASR::expr_t *e) {
                if (first_as) return;
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    first_as = ASR::down_cast<ASR::ArraySection_t>(e);
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                    for (size_t i = 0; i < f->n_args; i++) {
                        if (f->m_args[i]) find_array_section(f->m_args[i]);
                    }
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(e);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        if (fc->m_args[i].m_value)
                            find_array_section(fc->m_args[i].m_value);
                    }
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
                    find_array_section(rb->m_left);
                    find_array_section(rb->m_right);
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    find_array_section(ib->m_left);
                    find_array_section(ib->m_right);
                } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                    find_array_section(
                        ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg);
                } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                    find_array_section(
                        ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
                } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                    ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
                    find_array_section(rc->m_left);
                    find_array_section(rc->m_right);
                } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                    ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
                    find_array_section(ic->m_left);
                    find_array_section(ic->m_right);
                } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                    ASR::LogicalCompare_t *lc = ASR::down_cast<ASR::LogicalCompare_t>(e);
                    find_array_section(lc->m_left);
                    find_array_section(lc->m_right);
                } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
                    ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                    find_array_section(lb->m_left);
                    find_array_section(lb->m_right);
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    ASR::ArrayBroadcast_t *ab = ASR::down_cast<ASR::ArrayBroadcast_t>(e);
                    find_array_section(ab->m_array);
                } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    ASR::ArrayPhysicalCast_t *apc =
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(e);
                    find_array_section(apc->m_arg);
                }
            };
            find_array_section(asgn->m_value);

            if (!first_as) {
                if (ASR::is_a<ASR::ArrayBroadcast_t>(*asgn->m_value)) {
                    // Handle whole-array broadcast assignment:
                    //   x = 1.0  (Var_array = ArrayBroadcast(scalar))
                    // Convert to: do i = 1, size(x); x(i) = 1.0; end do
                    ASR::ArrayBroadcast_t *ab =
                        ASR::down_cast<ASR::ArrayBroadcast_t>(asgn->m_value);
                    ASR::expr_t *scalar_value = ab->m_array;

                    Location loc = stmt->base.loc;
                    ASR::ttype_t *int_type = ASRUtils::TYPE(
                        ASR::make_Integer_t(al, loc, 4));
                    ASR::ttype_t *elem_type =
                        ASRUtils::extract_type(target_type);

                    ASR::Array_t *arr =
                        ASR::down_cast<ASR::Array_t>(target_type);
                    ASR::dimension_t *dims = arr->m_dims;
                    size_t n_dims = arr->n_dims;

                    SymbolTable *var_scope = current_scope;
                    while (var_scope && var_scope->asr_owner &&
                           var_scope->asr_owner->type ==
                               ASR::asrType::symbol &&
                           ASR::is_a<ASR::AssociateBlock_t>(
                               *ASR::down_cast<ASR::symbol_t>(
                                   var_scope->asr_owner))) {
                        var_scope = var_scope->parent;
                    }

                    auto make_bc_loop_var =
                        [&](const std::string &prefix) -> ASR::expr_t* {
                        std::string name =
                            var_scope->get_unique_name(prefix);
                        ASR::symbol_t *sym =
                            ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                var_scope, s2c(al, name), nullptr, 0,
                                ASR::intentType::Local, nullptr, nullptr,
                                ASR::storage_typeType::Default,
                                ASRUtils::duplicate_type(al, int_type),
                                nullptr, ASR::abiType::Source,
                                ASR::accessType::Public,
                                ASR::presenceType::Required, false));
                        var_scope->add_symbol(name, sym);
                        return ASRUtils::EXPR(
                            ASR::make_Var_t(al, loc, sym));
                    };

                    std::vector<ASR::expr_t*> loop_vars;
                    for (size_t d = 0; d < n_dims; d++) {
                        loop_vars.push_back(
                            make_bc_loop_var("__gpu_bc_i"));
                    }

                    Vec<ASR::array_index_t> lhs_args;
                    lhs_args.reserve(al, n_dims);
                    for (size_t d = 0; d < n_dims; d++) {
                        ASR::array_index_t idx;
                        idx.loc = loc;
                        idx.m_left = nullptr;
                        idx.m_right = loop_vars[d];
                        idx.m_step = nullptr;
                        lhs_args.push_back(al, idx);
                    }
                    ASR::expr_t *lhs_item = ASRUtils::EXPR(
                        ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                            lhs_args.p, lhs_args.n, elem_type,
                            ASR::arraystorageType::ColMajor, nullptr));

                    Vec<ASR::stmt_t*> innermost_body;
                    innermost_body.reserve(al, 1);
                    innermost_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, lhs_item,
                            scalar_value, nullptr, false, false)));

                    ASR::stmt_t *loop_nest = nullptr;
                    for (int d = (int)n_dims - 1; d >= 0; d--) {
                        ASR::do_loop_head_t head;
                        head.loc = loc;
                        head.m_v = loop_vars[d];
                        set_loop_head_bounds(al, loc, head, dims, (size_t)d,
                            asgn->m_target);
                        head.m_increment = nullptr;
                        if (loop_nest == nullptr) {
                            loop_nest = ASRUtils::STMT(
                                ASR::make_DoLoop_t(al, loc, nullptr,
                                    head, innermost_body.p,
                                    innermost_body.n, nullptr, 0));
                        } else {
                            Vec<ASR::stmt_t*> outer_body;
                            outer_body.reserve(al, 1);
                            outer_body.push_back(al, loop_nest);
                            loop_nest = ASRUtils::STMT(
                                ASR::make_DoLoop_t(al, loc, nullptr,
                                    head, outer_body.p, outer_body.n,
                                    nullptr, 0));
                        }
                    }
                    new_body.push_back(al, loop_nest);
                    changed = true;
                    continue;
                }

                // Handle whole-array elemental assignment without
                // ArraySection (e.g., a = obj%eval(z) where eval is
                // elemental and z is a whole-array Var).
                // Convert to:
                //   do i = 1, size(a); a(i) = obj%eval(z(i)); end do
                ASR::ttype_t *rhs_type =
                    ASRUtils::type_get_past_allocatable(
                        ASRUtils::expr_type(asgn->m_value));
                if (!ASR::is_a<ASR::Array_t>(*rhs_type)) {
                    new_body.push_back(al, stmt);
                    continue;
                }

                // Skip decomposition for non-elemental FunctionCalls
                // that return arrays (e.g., a = f() where f returns
                // a whole array). Only elemental operations can be
                // safely decomposed into element-wise loops.
                if (ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(
                            asgn->m_value);
                    if (!ASRUtils::is_elemental(fc->m_name)) {
                        new_body.push_back(al, stmt);
                        continue;
                    }
                }

                ASR::Array_t *target_arr =
                    ASR::down_cast<ASR::Array_t>(target_type);

                Location loc = stmt->base.loc;
                ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4));
                ASR::ttype_t *elem_type =
                    ASRUtils::extract_type(target_type);
                ASR::dimension_t *dims = target_arr->m_dims;
                size_t n_dims = target_arr->n_dims;

                SymbolTable *var_scope = current_scope;
                while (var_scope && var_scope->asr_owner &&
                       var_scope->asr_owner->type ==
                           ASR::asrType::symbol &&
                       ASR::is_a<ASR::AssociateBlock_t>(
                           *ASR::down_cast<ASR::symbol_t>(
                               var_scope->asr_owner))) {
                    var_scope = var_scope->parent;
                }

                auto make_elem_loop_var =
                    [&](const std::string &prefix) -> ASR::expr_t* {
                    std::string name =
                        var_scope->get_unique_name(prefix);
                    ASR::symbol_t *sym =
                        ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc,
                            var_scope, s2c(al, name), nullptr, 0,
                            ASR::intentType::Local, nullptr, nullptr,
                            ASR::storage_typeType::Default,
                            ASRUtils::duplicate_type(al, int_type),
                            nullptr, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                    var_scope->add_symbol(name, sym);
                    return ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, sym));
                };

                std::vector<ASR::expr_t*> loop_vars;
                for (size_t d = 0; d < n_dims; d++) {
                    loop_vars.push_back(
                        make_elem_loop_var("__gpu_elem_i"));
                }

                // Elementize: replace array-typed Vars with ArrayItem
                std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
                    [&](ASR::expr_t *e) -> ASR::expr_t* {
                    if (ASR::is_a<ASR::Var_t>(*e)) {
                        ASR::ttype_t *vtype =
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::expr_type(e));
                        if (ASR::is_a<ASR::Array_t>(*vtype)) {
                            ASR::ttype_t *velem =
                                ASRUtils::extract_type(vtype);
                            ASR::Array_t *va =
                                ASR::down_cast<ASR::Array_t>(vtype);
                            Vec<ASR::array_index_t> idx_args;
                            idx_args.reserve(al, va->n_dims);
                            for (size_t d = 0; d < va->n_dims; d++) {
                                ASR::array_index_t idx;
                                idx.loc = loc;
                                idx.m_left = nullptr;
                                idx.m_right = loop_vars[
                                    d < loop_vars.size() ? d : 0];
                                idx.m_step = nullptr;
                                idx_args.push_back(al, idx);
                            }
                            return ASRUtils::EXPR(
                                ASR::make_ArrayItem_t(al, loc, e,
                                    idx_args.p, idx_args.n, velem,
                                    ASR::arraystorageType::ColMajor,
                                    nullptr));
                        }
                        return e;
                    } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                        ASR::FunctionCall_t *fc =
                            ASR::down_cast<ASR::FunctionCall_t>(e);
                        Vec<ASR::call_arg_t> new_args;
                        new_args.reserve(al, fc->n_args);
                        for (size_t i = 0; i < fc->n_args; i++) {
                            ASR::call_arg_t arg;
                            arg.loc = fc->m_args[i].loc;
                            arg.m_value = fc->m_args[i].m_value
                                ? elementize(fc->m_args[i].m_value)
                                : nullptr;
                            new_args.push_back(al, arg);
                        }
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_FunctionCall_t(al, loc,
                                fc->m_name, fc->m_original_name,
                                new_args.p, new_args.n, ret_type,
                                fc->m_value, fc->m_dt));
                    } else if (ASR::is_a<
                            ASR::IntrinsicElementalFunction_t>(*e)) {
                        ASR::IntrinsicElementalFunction_t *f =
                            ASR::down_cast<
                                ASR::IntrinsicElementalFunction_t>(e);
                        Vec<ASR::expr_t*> new_args;
                        new_args.reserve(al, f->n_args);
                        for (size_t i = 0; i < f->n_args; i++) {
                            new_args.push_back(al,
                                f->m_args[i]
                                    ? elementize(f->m_args[i])
                                    : nullptr);
                        }
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_IntrinsicElementalFunction_t(
                                al, loc, f->m_intrinsic_id,
                                new_args.p, new_args.n,
                                f->m_overload_id, ret_type,
                                f->m_value));
                    } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                        ASR::RealBinOp_t *rb =
                            ASR::down_cast<ASR::RealBinOp_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_RealBinOp_t(al, loc,
                                elementize(rb->m_left), rb->m_op,
                                elementize(rb->m_right), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                        ASR::IntegerBinOp_t *ib =
                            ASR::down_cast<ASR::IntegerBinOp_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_IntegerBinOp_t(al, loc,
                                elementize(ib->m_left), ib->m_op,
                                elementize(ib->m_right), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                        ASR::RealUnaryMinus_t *u =
                            ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_RealUnaryMinus_t(al, loc,
                                elementize(u->m_arg), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                        ASR::IntegerUnaryMinus_t *u =
                            ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_IntegerUnaryMinus_t(al, loc,
                                elementize(u->m_arg), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                        return ASR::down_cast<ASR::ArrayBroadcast_t>(
                            e)->m_array;
                    } else if (ASR::is_a<
                            ASR::ArrayPhysicalCast_t>(*e)) {
                        return elementize(
                            ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                                e)->m_arg);
                    } else if (ASR::is_a<
                            ASR::StructInstanceMember_t>(*e)) {
                        ASR::ttype_t *mtype =
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::expr_type(e));
                        if (ASR::is_a<ASR::Array_t>(*mtype)) {
                            ASR::ttype_t *melem =
                                ASRUtils::extract_type(mtype);
                            ASR::Array_t *ma =
                                ASR::down_cast<ASR::Array_t>(mtype);
                            Vec<ASR::array_index_t> idx_args;
                            idx_args.reserve(al, ma->n_dims);
                            for (size_t d = 0; d < ma->n_dims; d++) {
                                ASR::array_index_t idx;
                                idx.loc = loc;
                                idx.m_left = nullptr;
                                idx.m_right = loop_vars[
                                    d < loop_vars.size() ? d : 0];
                                idx.m_step = nullptr;
                                idx_args.push_back(al, idx);
                            }
                            return ASRUtils::EXPR(
                                ASR::make_ArrayItem_t(al, loc, e,
                                    idx_args.p, idx_args.n, melem,
                                    ASR::arraystorageType::ColMajor,
                                    nullptr));
                        }
                        return e;
                    }
                    return e;
                };

                Vec<ASR::array_index_t> lhs_args;
                lhs_args.reserve(al, n_dims);
                for (size_t d = 0; d < n_dims; d++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = nullptr;
                    idx.m_right = loop_vars[d];
                    idx.m_step = nullptr;
                    lhs_args.push_back(al, idx);
                }
                ASR::expr_t *lhs_item = ASRUtils::EXPR(
                    ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                        lhs_args.p, lhs_args.n, elem_type,
                        ASR::arraystorageType::ColMajor, nullptr));

                ASR::expr_t *rhs_item = elementize(asgn->m_value);

                Vec<ASR::stmt_t*> innermost_body;
                innermost_body.reserve(al, 1);
                innermost_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, lhs_item,
                        rhs_item, nullptr, false, false)));

                ASR::stmt_t *loop_nest = nullptr;
                for (int d = (int)n_dims - 1; d >= 0; d--) {
                    ASR::do_loop_head_t head;
                    head.loc = loc;
                    head.m_v = loop_vars[d];
                    set_loop_head_bounds(al, loc, head, dims, (size_t)d,
                        asgn->m_target);
                    head.m_increment = nullptr;
                    if (loop_nest == nullptr) {
                        loop_nest = ASRUtils::STMT(
                            ASR::make_DoLoop_t(al, loc, nullptr,
                                head, innermost_body.p,
                                innermost_body.n, nullptr, 0));
                    } else {
                        Vec<ASR::stmt_t*> outer_body;
                        outer_body.reserve(al, 1);
                        outer_body.push_back(al, loop_nest);
                        loop_nest = ASRUtils::STMT(
                            ASR::make_DoLoop_t(al, loc, nullptr,
                                head, outer_body.p, outer_body.n,
                                nullptr, 0));
                    }
                }
                new_body.push_back(al, loop_nest);
                changed = true;
                continue;
            }

            // Find the range dimension
            int range_dim = -1;
            for (size_t i = 0; i < first_as->n_args; i++) {
                if (first_as->m_args[i].m_left && first_as->m_args[i].m_right
                        && first_as->m_args[i].m_step) {
                    if (range_dim != -1) {
                        range_dim = -1;
                        break;
                    }
                    range_dim = (int)i;
                }
            }
            if (range_dim == -1) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            ASR::expr_t *loop_start = int32_const(loc, 1);
            ASR::expr_t *loop_end = section_extent(loc,
                first_as->m_args[range_dim]);

            // Create loop variable in the containing function/program scope
            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }
            std::string loop_var_name = var_scope->get_unique_name(
                "__gpu_elem_i");
            ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, var_scope,
                    s2c(al, loop_var_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            var_scope->add_symbol(loop_var_name, loop_var_sym);
            ASR::expr_t *loop_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, loop_var_sym));

            // Elementize: replace ArraySection with ArrayItem, recurse
            // into elemental wrappers
            std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
                [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *as =
                        ASR::down_cast<ASR::ArraySection_t>(e);
                    Vec<ASR::array_index_t> new_args;
                    new_args.reserve(al, as->n_args);
                    for (size_t i = 0; i < as->n_args; i++) {
                        ASR::array_index_t idx;
                        idx.loc = as->m_args[i].loc;
                        if (as->m_args[i].m_left && as->m_args[i].m_right
                                && as->m_args[i].m_step) {
                            idx.m_left = nullptr;
                            idx.m_right = section_index(loc,
                                as->m_args[i], loop_var);
                            idx.m_step = nullptr;
                        } else {
                            idx.m_left = as->m_args[i].m_left;
                            idx.m_right = as->m_args[i].m_right;
                            idx.m_step = as->m_args[i].m_step;
                        }
                        new_args.push_back(al, idx);
                    }
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(as->m_v));
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        as->m_v, new_args.p, new_args.n,
                        elem_type, ASR::arraystorageType::ColMajor, nullptr));
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                    Vec<ASR::expr_t*> new_args;
                    new_args.reserve(al, f->n_args);
                    for (size_t i = 0; i < f->n_args; i++) {
                        new_args.push_back(al,
                            f->m_args[i] ? elementize(f->m_args[i])
                                         : nullptr);
                    }
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntrinsicElementalFunction_t(al, loc,
                            f->m_intrinsic_id, new_args.p, new_args.n,
                            f->m_overload_id, elem_type, f->m_value));
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb =
                        ASR::down_cast<ASR::RealBinOp_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                        elementize(rb->m_left), rb->m_op,
                        elementize(rb->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib =
                        ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        elementize(ib->m_left), ib->m_op,
                        elementize(ib->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                    ASR::RealUnaryMinus_t *u =
                        ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                        elementize(u->m_arg), elem_type, nullptr));
                } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                    ASR::IntegerUnaryMinus_t *u =
                        ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                        elementize(u->m_arg), elem_type, nullptr));
                } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                    ASR::RealCompare_t *rc =
                        ASR::down_cast<ASR::RealCompare_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
                        elementize(rc->m_left), rc->m_op,
                        elementize(rc->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                    ASR::IntegerCompare_t *ic =
                        ASR::down_cast<ASR::IntegerCompare_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                        elementize(ic->m_left), ic->m_op,
                        elementize(ic->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                    ASR::LogicalCompare_t *lc =
                        ASR::down_cast<ASR::LogicalCompare_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al, loc,
                        elementize(lc->m_left), lc->m_op,
                        elementize(lc->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    return ASR::down_cast<ASR::ArrayBroadcast_t>(e)->m_array;
                } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    return elementize(
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
                }
                return e;
            };

            // Build LHS ArrayItem: b(loop_var)
            ASR::ttype_t *elem_type = ASRUtils::extract_type(target_type);
            Vec<ASR::array_index_t> lhs_args;
            lhs_args.reserve(al, 1);
            ASR::array_index_t lhs_idx;
            lhs_idx.loc = loc;
            lhs_idx.m_left = nullptr;
            lhs_idx.m_right = loop_var;
            lhs_idx.m_step = nullptr;
            lhs_args.push_back(al, lhs_idx);
            ASR::expr_t *lhs_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                    lhs_args.p, lhs_args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));

            // Build RHS: elementize the value expression
            ASR::expr_t *rhs_item = elementize(asgn->m_value);

            // Build loop body: lhs_item = rhs_item
            Vec<ASR::stmt_t*> loop_body;
            loop_body.reserve(al, 1);
            loop_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, lhs_item, rhs_item,
                    nullptr, false, false)));

            // Build DoLoop
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = loop_var;
            head.m_start = loop_start;
            head.m_end = loop_end;
            head.m_increment = nullptr;
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr,
                    head, loop_body.p, loop_body.n, nullptr, 0)));

            changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    // Collects every symbol the kernel would have to reference for `x`:
    // the symbols appearing in the loop head and body, plus the symbols
    // that only appear inside the array-dimension expressions of those
    // symbols' types (e.g. `tmp(size(b))` pulls in `b`).
    // The names the kernel arguments will carry, in the spelling the
    // workspace extent resolver expects: every symbol the loop involves,
    // plus the synthetic per-dimension extent scalar the kernel
    // extraction adds for each dimension of an array argument.
    void collect_kernel_arg_names(const ASR::DoConcurrentLoop_t &x,
            const std::set<SymbolTable*> &enclosing_block_scopes,
            std::vector<std::string> &arg_names) {
        std::map<std::string,
            std::pair<ASR::ttype_t*, ASR::expr_t*>> syms;
        collect_involved_syms(x, enclosing_block_scopes, syms);
        for (auto &sym : syms) {
            arg_names.push_back(sym.first);
            if (sym.second.first == nullptr) continue;
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
                sym.second.first);
            if (!ASR::is_a<ASR::Array_t>(*type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                arg_names.push_back(gpu_dim_arg_name(sym.first, d));
            }
        }
    }

    void collect_involved_syms(const ASR::DoConcurrentLoop_t &x,
            const std::set<SymbolTable*> &enclosing_block_scopes,
            std::map<std::string,
                std::pair<ASR::ttype_t*, ASR::expr_t*>> &involved_syms) {
        GpuSymbolCollector collector(al, involved_syms,
            enclosing_block_scopes);
        collector.visit_DoConcurrentLoop(x);
        bool added = true;
        while (added) {
            added = false;
            std::map<std::string,
                std::pair<ASR::ttype_t*, ASR::expr_t*>> extra_syms;
            GpuSymbolCollector type_collector(al, extra_syms,
                enclosing_block_scopes);
            for (auto &[sym_name, sym_info] : involved_syms) {
                ASR::symbol_t *sym = current_scope->resolve_symbol(sym_name);
                if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_start)
                        type_collector.visit_expr(*arr->m_dims[d].m_start);
                    if (arr->m_dims[d].m_length)
                        type_collector.visit_expr(*arr->m_dims[d].m_length);
                }
            }
            for (auto &[name, info] : extra_syms) {
                if (involved_syms.find(name) == involved_syms.end()) {
                    involved_syms[name] = info;
                    added = true;
                }
            }
        }
    }

    // An AssociateBlock nested inside a do concurrent body is normally
    // inlined away by substituting each associate name with its selector
    // expression. An array-valued selector, however, is materialised into
    // a temporary variable owned by the AssociateBlock's own symbol table,
    // and that temporary is still referenced by the inlined statements.
    // It has to stay private to each loop iteration, so the scope cannot
    // simply be dropped. Rebuild it as an equivalent Block over the same
    // symbol table, which the kernel extraction already knows how to carry
    // into the generated kernel, and return the BlockCall replacing the
    // AssociateBlockCall. Returns nullptr when no symbol of the
    // AssociateBlock survives, in which case the caller inlines the
    // statements directly as before.
    ASR::stmt_t* wrap_assoc_scope_in_block(ASR::AssociateBlock_t *ab,
            Vec<ASR::stmt_t*> &resolved_stmts, SymbolTable *parent_scope) {
        std::set<ASR::symbol_t*> referenced_syms;
        VarSymbolCollector collector(referenced_syms);
        for (size_t i = 0; i < resolved_stmts.n; i++) {
            collector.visit_stmt(*resolved_stmts.p[i]);
        }
        bool scope_needed = false;
        for (auto &item : ab->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second) &&
                    referenced_syms.find(item.second)
                        != referenced_syms.end()) {
                scope_needed = true;
                break;
            }
        }
        if (!scope_needed) return nullptr;
        std::string block_name = parent_scope->get_unique_name(
            std::string(ab->m_name) + "_scope");
        ab->m_symtab->parent = parent_scope;
        ASR::asr_t *block = ASR::make_Block_t(al, ab->base.base.loc,
            ab->m_symtab, s2c(al, block_name), resolved_stmts.p,
            resolved_stmts.n);
        ab->m_symtab->asr_owner = block;
        ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
        parent_scope->add_symbol(block_name, block_sym);
        return ASRUtils::STMT(ASR::make_BlockCall_t(al, ab->base.base.loc,
            -1, block_sym));
    }

    // Move the symbols of an inlined AssociateBlock that are still
    // reachable from the resolved statements into the scope that now owns
    // those statements. ExternalSymbol entries (e.g. type-bound procedure
    // references) remain referenced by call nodes, and a Block scope may
    // have been created for a nested AssociateBlock.
    void migrate_inlined_assoc_symbols(ASR::AssociateBlock_t *ab,
            SymbolTable *parent_scope) {
        std::vector<std::pair<std::string, ASR::symbol_t*>> to_move;
        for (auto &item : ab->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second) ||
                    ASR::is_a<ASR::Block_t>(*item.second)) {
                to_move.push_back({item.first, item.second});
            }
        }
        for (auto &item : to_move) {
            std::string name = item.first;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                if (parent_scope->get_symbol(name)) continue;
                ASR::down_cast<ASR::ExternalSymbol_t>(item.second)
                    ->m_parent_symtab = parent_scope;
            } else {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    item.second);
                if (parent_scope->get_symbol(name)) {
                    name = parent_scope->get_unique_name(name);
                    block->m_name = s2c(al, name);
                }
                block->m_symtab->parent = parent_scope;
            }
            parent_scope->add_symbol(name, item.second);
        }
    }

    // Copy every loop-invariant, read-only element of an array of
    // derived type that the loop reaches through a component --
    // `x%c_(k)` -- into a temporary of the enclosing scope, and let the
    // loop body name the temporary instead.  The copy runs once, on the
    // host, before the launch; the temporary is then an ordinary
    // derived-type kernel argument whose own components have extents
    // the device can be told about, which is what the chain itself does
    // not.
    //
    // An element the loop writes into is handled the same way, with a
    // copy back over the original after the launch: because the gather
    // brought the whole element in first, the bytes the kernel left
    // alone still hold what was read, so the copy back is exact even
    // when only part of the element was written.  It is only allowed
    // when every write to the object lands inside that one element --
    // a write anywhere else, `allocate` of the array the element comes
    // from included, would be undone by the copy back.
    //
    // `undo` records every slot that was overwritten and `temp_names`
    // every symbol that was added, so `GpuGatherGuard` can put the loop
    // back exactly as it was if the offload is declined further down.
    bool hoist_struct_element_gathers(const ASR::DoConcurrentLoop_t &x,
            Vec<ASR::stmt_t*> &gather_stmts,
            Vec<ASR::stmt_t*> &scatter_stmts,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo,
            std::vector<std::string> &temp_names) {
        GpuStructElementGatherCollector collector;
        for (size_t i = 0; i < x.n_body; i++) {
            collector.visit_stmt(*x.m_body[i]);
        }
        if (collector.found.empty()) return true;

        GpuWrittenRootCollector written;
        for (size_t i = 0; i < x.n_body; i++) {
            written.visit_stmt(*x.m_body[i]);
        }
        std::set<ASR::symbol_t*> loop_indices;
        for (size_t d = 0; d < x.n_head; d++) {
            if (!x.m_head[d].m_v) continue;
            if (!ASR::is_a<ASR::Var_t>(*x.m_head[d].m_v)) continue;
            loop_indices.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_head[d].m_v)->m_v));
        }

        std::vector<GpuStructElementGather> gathers;
        for (ASR::ArrayItem_t *item : collector.found) {
            ASR::expr_t *chain = ASRUtils::EXPR((ASR::asr_t*)item);
            GpuDesignatorBase base = gpu_designator_base(chain);
            if (!base.is_known()) return false;
            // A write to the object the chain hangs off makes a
            // read-only copy stale. The copy can still stand in for the
            // element when every such write lands inside that element,
            // because it is then put back over the original after the
            // launch. A write anywhere else in the object would be lost
            // or, worse, undone by that copy back.
            bool scatter = false;
            if (written.roots.count(base.root)) {
                // A callee that writes the object through one of its
                // dummies is spliced into the kernel, and the pass does
                // not build a kernel for that shape yet; the copy back
                // would be over storage the launch never reached.
                if (written.call_roots.count(base.root)) return false;
                for (ASR::expr_t *target : written.targets) {
                    if (gpu_designator_base(target).root != base.root) {
                        continue;
                    }
                    if (!gpu_designator_within(target, chain)) return false;
                }
                scatter = true;
            }
            if (!host_nameable(base.root)) return false;
            // The subscripts have to mean the same thing for every
            // iteration, and mean it in the scope the copy is made in.
            bool invariant = true;
            for (size_t k = 0; k < item->n_args && invariant; k++) {
                GpuExprSymbolCollector sc;
                sc.visit_expr(*item->m_args[k].m_right);
                for (ASR::symbol_t *sym : sc.syms) {
                    if (loop_indices.count(sym) || written.roots.count(sym)
                            || !host_nameable(sym)) {
                        invariant = false;
                        break;
                    }
                }
            }
            if (!invariant) return false;
            bool seen = false;
            for (const GpuStructElementGather &g : gathers) {
                if (gpu_same_designator(g.chain, chain)) {
                    seen = true;
                    break;
                }
            }
            if (seen) continue;
            ASR::symbol_t *struct_sym =
                ASRUtils::get_struct_sym_from_struct_expr(chain);
            if (struct_sym == nullptr) return false;
            GpuStructElementGather g;
            g.chain = chain;
            g.temp = nullptr;
            g.scatter = scatter;
            gathers.push_back(g);
        }
        if (gathers.empty()) return true;

        // Two elements of the same object can designate the same
        // storage at run time -- `x%c_(i)` and `x%c_(k)` with `i == k`
        // -- and then one copy back would silently undo the other. Take
        // only a single element per written object.
        for (const GpuStructElementGather &g : gathers) {
            if (!g.scatter) continue;
            ASR::symbol_t *root = gpu_designator_base(g.chain).root;
            for (const GpuStructElementGather &other : gathers) {
                if (&other == &g) continue;
                if (gpu_designator_base(other.chain).root == root) {
                    return false;
                }
            }
        }

        gather_stmts.reserve(al, gathers.size());
        scatter_stmts.reserve(al, gathers.size());
        for (GpuStructElementGather &g : gathers) {
            const Location &gloc = g.chain->base.loc;
            GpuDesignatorBase base = gpu_designator_base(g.chain);
            std::string stem = base.members.empty()
                ? std::string("elem")
                : std::string(ASRUtils::symbol_name(base.members.front()));
            std::string name = current_scope->get_unique_name(
                "__gpu_gather_" + stem);
            ASR::symbol_t *temp = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, gloc, current_scope,
                    s2c(al, name), nullptr, 0, ASR::intentType::Local,
                    nullptr, nullptr, ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al,
                        ASRUtils::expr_type(g.chain)),
                    ASRUtils::get_struct_sym_from_struct_expr(g.chain),
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            current_scope->add_symbol(name, temp);
            temp_names.push_back(name);
            g.temp = temp;
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            ASR::expr_t *rhs = dup.duplicate_expr(g.chain);
            if (!rhs || !dup.success) rhs = g.chain;
            gather_stmts.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, gloc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, gloc, temp)),
                    rhs, nullptr, false, false)));
            if (!g.scatter) continue;
            ASRUtils::ExprStmtDuplicator back(al);
            back.success = true;
            ASR::expr_t *lhs = back.duplicate_expr(g.chain);
            if (!lhs || !back.success) lhs = g.chain;
            scatter_stmts.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, gloc, lhs,
                    ASRUtils::EXPR(ASR::make_Var_t(al, gloc, temp)),
                    nullptr, false, false)));
        }

        GpuStructElementGatherVisitor sub(al, gathers, undo);
        for (size_t i = 0; i < x.n_body; i++) {
            sub.visit_stmt(*x.m_body[i]);
        }
        return true;
    }

    // True when `sym` is the very symbol the enclosing scope resolves its
    // name to, so an expression written in terms of it can be repeated
    // outside the loop.
    bool host_nameable(ASR::symbol_t *sym) {
        if (sym == nullptr) return false;
        ASR::symbol_t *found = current_scope->resolve_symbol(
            ASRUtils::symbol_name(sym));
        return found != nullptr
            && ASRUtils::symbol_get_past_external(found) == sym;
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (!pass_options.gpu_offload_metal && !pass_options.gpu_offload_cuda) return;

        GpuOffloadReport::clear_detail();
        std::string report_proc;
        if (GpuOffloadReport::enabled) report_proc = report_enclosing_proc();

        // Skip loops with reduce clause (let do_loops handle as regular loop)
        if (x.n_reduction > 0) {
            GpuOffloadReport::emit(x.base.base.loc, report_proc,
                "reduce-clause");
            return;
        }

        Location loc = x.base.base.loc;
        size_t n_dims = x.n_head;
        // Any rank is supported: the nest is dispatched as a flat 1-D grid of
        // product(extents) threads and every index is recovered from the flat
        // thread id by successive divmod over the per-dimension extents. The
        // 3-D shape of the underlying dispatch grid therefore does not limit
        // the number of loop indices.
        if (n_dims == 0) {
            GpuOffloadReport::emit(loc, report_proc, "no-loop-index");
            return;
        }

        for (size_t d = 0; d < n_dims; d++) {
            if (!x.m_head[d].m_v || !x.m_head[d].m_start || !x.m_head[d].m_end) {
                GpuOffloadReport::emit(loc, report_proc,
                    "loop-head-incomplete");
                return;
            }
        }

        // Resolve associate variables to their original targets if this
        // DoConcurrentLoop is inside one or more nested AssociateBlocks.
        // The kernel function lives at the translation-unit level and
        // cannot reference symbols from any AssociateBlock's scope, so
        // we walk up through all enclosing AssociateBlock ancestors and
        // collect all their associate mappings.
        // The map is declared outside the block so it is available later
        // when resolving inner AssociateBlockCalls in the loop body.
        std::map<ASR::symbol_t*, ASR::expr_t*> enclosing_assoc_map;
        {
            std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map = enclosing_assoc_map;
            SymbolTable *scope = current_scope;
            while (scope && scope->asr_owner &&
                   scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (is_a<ASR::Block_t>(*owner_sym)) {
                    scope = scope->parent;
                    continue;
                }
                if (!is_a<ASR::AssociateBlock_t>(*owner_sym)) break;
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast2<ASR::AssociateBlock_t>(scope->asr_owner);
                for (size_t i = 0; i < ab->n_body; i++) {
                    if (is_a<ASR::Associate_t>(*ab->m_body[i])) {
                        ASR::Associate_t *assoc = down_cast<ASR::Associate_t>(
                            ab->m_body[i]);
                        if (is_a<ASR::Var_t>(*assoc->m_target)) {
                            ASR::symbol_t *assoc_sym =
                                down_cast<ASR::Var_t>(assoc->m_target)->m_v;
                            assoc_map[assoc_sym] = assoc->m_value;
                        }
                    } else if (is_a<ASR::Assignment_t>(*ab->m_body[i])) {
                        // associate(n => constant_expr) generates an
                        // Assignment instead of Associate. Capture the
                        // initial value for variables owned by this
                        // AssociateBlock so they can be resolved.
                        // Only add if the symbol isn't already mapped
                        // (e.g., from a prior Associate node); otherwise
                        // we would overwrite the real alias with a
                        // regular assignment like `v = 0.`, whose RHS
                        // may reference `v` itself and cause infinite
                        // recursion during resolution.
                        // Only scalar selectors are inlined. An
                        // array-valued selector is materialized by the
                        // compiler into a temporary that lives in this
                        // AssociateBlock's symtab, and its defining
                        // expression may itself reference further
                        // AssociateBlock-local symbols (the array
                        // constant buffer, the resolved specific
                        // procedure of a generic constructor, ...).
                        // Inlining it would drag those unreachable
                        // symbols into the kernel. The associate name is
                        // backed by real storage here, so it is passed
                        // into the kernel as an ordinary buffer instead.
                        ASR::Assignment_t *asgn = down_cast<ASR::Assignment_t>(
                            ab->m_body[i]);
                        if (is_a<ASR::Var_t>(*asgn->m_target)) {
                            ASR::symbol_t *sym =
                                down_cast<ASR::Var_t>(asgn->m_target)->m_v;
                            if (is_a<ASR::Variable_t>(*sym) &&
                                down_cast<ASR::Variable_t>(sym)->m_parent_symtab
                                    == ab->m_symtab &&
                                !ASRUtils::is_array(
                                    down_cast<ASR::Variable_t>(sym)->m_type) &&
                                assoc_map.find(sym) == assoc_map.end()) {
                                assoc_map[sym] = asgn->m_value;
                            }
                        }
                    }
                }
                scope = scope->parent;
            }
            if (!assoc_map.empty()) {
                AssociateVarResolver resolver(al, assoc_map);
                ASR::DoConcurrentLoop_t &xx =
                    const_cast<ASR::DoConcurrentLoop_t&>(x);
                for (size_t d = 0; d < n_dims; d++) {
                    if (xx.m_head[d].m_start) {
                        resolver.current_expr = &(xx.m_head[d].m_start);
                        resolver.replace_expr(xx.m_head[d].m_start);
                    }
                    if (xx.m_head[d].m_end) {
                        resolver.current_expr = &(xx.m_head[d].m_end);
                        resolver.replace_expr(xx.m_head[d].m_end);
                    }
                    if (xx.m_head[d].m_increment) {
                        resolver.current_expr = &(xx.m_head[d].m_increment);
                        resolver.replace_expr(xx.m_head[d].m_increment);
                    }
                }
                AssociateVarResolverVisitor resolver_visitor(al, assoc_map);
                for (size_t i = 0; i < x.n_body; i++) {
                    resolver_visitor.visit_stmt(*x.m_body[i]);
                }
                // The statement visitor above does not descend into
                // BlockCall targets (Blocks have their own scope), so
                // resolve associate aliases in both block body statements
                // and block-local type expressions (e.g., `real a(n)` where
                // `n` is an associate alias from an enclosing associate).
                // This must be recursive to handle nested blocks.
                std::function<void(ASR::stmt_t**, size_t)>
                    resolve_assoc_in_blocks = [&](ASR::stmt_t **stmts,
                                                  size_t n_stmts) {
                    for (size_t i = 0; i < n_stmts; i++) {
                        if (!ASR::is_a<ASR::BlockCall_t>(*stmts[i])) continue;
                        ASR::BlockCall_t *bc =
                            ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                        if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                        ASR::Block_t *block =
                            ASR::down_cast<ASR::Block_t>(bc->m_m);
                        // Resolve in block body statements
                        for (size_t j = 0; j < block->n_body; j++) {
                            resolver_visitor.visit_stmt(*block->m_body[j]);
                        }
                        // Recurse into nested blocks
                        resolve_assoc_in_blocks(block->m_body,
                                                block->n_body);
                        // Resolve in block-local array dimension expressions
                        AssociateVarResolver type_resolver(al, assoc_map);
                        for (auto &item : block->m_symtab->get_scope()) {
                            if (!ASR::is_a<ASR::Variable_t>(*item.second))
                                continue;
                            ASR::Variable_t *var =
                                ASR::down_cast<ASR::Variable_t>(item.second);
                            if (!ASR::is_a<ASR::Array_t>(*var->m_type))
                                continue;
                            ASR::Array_t *arr =
                                ASR::down_cast<ASR::Array_t>(var->m_type);
                            for (size_t d = 0; d < arr->n_dims; d++) {
                                if (arr->m_dims[d].m_start) {
                                    type_resolver.current_expr =
                                        &(arr->m_dims[d].m_start);
                                    type_resolver.replace_expr(
                                        arr->m_dims[d].m_start);
                                }
                                if (arr->m_dims[d].m_length) {
                                    type_resolver.current_expr =
                                        &(arr->m_dims[d].m_length);
                                    type_resolver.replace_expr(
                                        arr->m_dims[d].m_length);
                                }
                            }
                        }
                    }
                };
                resolve_assoc_in_blocks(x.m_body, x.n_body);
                // Resolve associate aliases in enclosing Block scopes'
                // variable type expressions. When a do concurrent is
                // inside a Block that is inside an AssociateBlock, the
                // block-local arrays may use associate variables in
                // their dimension expressions (e.g., `real r(size(n))`
                // where `n` is an associate alias). These must be
                // resolved before kernel extraction moves the block
                // into the kernel scope where the AssociateBlock's
                // symtab is no longer reachable.
                {
                    SymbolTable *bs = current_scope;
                    while (bs && bs->asr_owner &&
                           bs->asr_owner->type == ASR::asrType::symbol) {
                        ASR::symbol_t *owner = down_cast<ASR::symbol_t>(
                            bs->asr_owner);
                        if (is_a<ASR::Block_t>(*owner)) {
                            AssociateVarResolver type_resolver(al,
                                assoc_map);
                            for (auto &item : bs->get_scope()) {
                                if (!ASR::is_a<ASR::Variable_t>(
                                        *item.second))
                                    continue;
                                ASR::Variable_t *var =
                                    ASR::down_cast<ASR::Variable_t>(
                                        item.second);
                                if (!ASR::is_a<ASR::Array_t>(
                                        *var->m_type))
                                    continue;
                                ASR::Array_t *arr =
                                    ASR::down_cast<ASR::Array_t>(
                                        var->m_type);
                                for (size_t d = 0; d < arr->n_dims;
                                     d++) {
                                    if (arr->m_dims[d].m_start) {
                                        type_resolver.current_expr =
                                            &(arr->m_dims[d].m_start);
                                        type_resolver.replace_expr(
                                            arr->m_dims[d].m_start);
                                    }
                                    if (arr->m_dims[d].m_length) {
                                        type_resolver.current_expr =
                                            &(arr->m_dims[d].m_length);
                                        type_resolver.replace_expr(
                                            arr->m_dims[d].m_length);
                                    }
                                }
                            }
                            bs = bs->parent;
                        } else if (is_a<ASR::AssociateBlock_t>(*owner)) {
                            bs = bs->parent;
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        // Detect if the do concurrent is inside a Block scope. If so,
        // block-local variables need to be collected as kernel parameters
        // rather than skipped. Walk up through AssociateBlock and Block
        // parents to find ALL enclosing Block scopes (e.g., do concurrent
        // inside a nested Block that accesses variables from outer Blocks).
        std::set<SymbolTable*> enclosing_block_scopes;
        {
            SymbolTable *scope = current_scope;
            while (scope && scope->asr_owner &&
                   scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (is_a<ASR::Block_t>(*owner_sym) ||
                    is_a<ASR::AssociateBlock_t>(*owner_sym)) {
                    enclosing_block_scopes.insert(scope);
                    scope = scope->parent;
                } else {
                    break;
                }
            }
        }

        // An element of an array of derived type reached through a
        // component -- `x%c_(k)` -- is copied to a temporary of this
        // scope before the launch, and the loop body reads the temporary.
        // This runs ahead of the checks below on purpose: they must judge
        // the shape the kernel would really be built from. The guard puts
        // the loop back untouched if any of them declines.
        Vec<ASR::stmt_t*> gather_stmts;
        gather_stmts.reserve(al, 1);
        Vec<ASR::stmt_t*> scatter_stmts;
        scatter_stmts.reserve(al, 1);
        std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> gather_undo;
        std::vector<std::string> gather_temp_names;
        GpuGatherGuard gather_guard(current_scope, gather_undo,
            gather_temp_names);
        if (pass_options.gpu_offload_metal
                && !hoist_struct_element_gathers(x, gather_stmts,
                    scatter_stmts, gather_undo, gather_temp_names)) {
            // The element could not be hoisted -- a subscript that moves
            // with the loop, or a write to the object that the copy back
            // after the launch could not reproduce exactly. Passing
            // the chain on unchanged reaches the device as a component of
            // the wrong element, which is a wrong number and no
            // diagnostic, so decline the loop instead.
            GpuOffloadReport::emit(loc, report_proc,
                "struct-element-cannot-be-gathered");
            return;
        }

        // Decide whether this loop can be offloaded at all *before* any of
        // the inline_* helpers below rewrite the loop body. Those helpers
        // are destructive: they lower array-section and intrinsic-array
        // assignments into explicit element loops, a half-lowered shape
        // that only the kernel extractor understands. If we declined the
        // offload after rewriting, the loop would stay on the host in a
        // form the later array_op pass no longer normalizes, and codegen
        // would fail. So: no mutation until the decision is made.
        if (pass_options.gpu_offload_metal) {
            std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
                candidate_syms;
            collect_involved_syms(x, enclosing_block_scopes, candidate_syms);
            // Every symbol reaching the kernel — buffer parameters,
            // by-value members of the __ScalarArgs struct and kernel-local
            // temporaries alike — is collected here, so a single sweep
            // covers all of them.
            for (auto &sym : candidate_syms) {
                if (!is_metal_representable_type(sym.second.first,
                        sym.second.second)) {
                    GpuOffloadReport::set_detail("sym=" + sym.first);
                    GpuOffloadReport::emit(loc, report_proc,
                        "type-not-metal-representable");
                    return;
                }
            }
            GpuLocalArrayChecker local_array_checker;
            for (size_t i = 0; i < x.n_body; i++) {
                local_array_checker.visit_stmt(*x.m_body[i]);
            }
            if (local_array_checker.has_unsized_local_array) {
                GpuOffloadReport::set_detail(
                    "sym=" + local_array_checker.unsized_name);
                GpuOffloadReport::emit(loc, report_proc,
                    "unsized-local-array");
                return;
            }
            // An array assignment whose two sides overlap the same array
            // needs a temporary (see materialize_aliased_assignments).
            // If that temporary cannot be fixed-size, decline here,
            // while the body is still untouched.
            if (body_needs_unsupported_alias_temp(x.m_body, x.n_body,
                    true)) {
                GpuOffloadReport::emit(loc, report_proc,
                    "runtime-sized-alias-temp");
                return;
            }
            // A strided section actual argument is gathered into a
            // contiguous kernel-local temporary below. When that temporary
            // cannot be sized at compile time the gather is impossible,
            // and passing the section on would silently drop its stride.
            if (body_has_ungatherable_strided_section(x.m_body, x.n_body)) {
                GpuOffloadReport::emit(loc, report_proc,
                    "strided-section-cannot-be-gathered");
                return;
            }
            // A device function may need a run-time sized local -- an
            // array-constructor temporary sized from an assumed-shape
            // dummy, say -- which Metal cannot declare. Work out here
            // which callees have to be spliced into the kernel body to
            // move those locals to kernel scope, where the VLA workspace
            // machinery applies. This is analysis only; the splice
            // itself happens below, after the offload decision.
            functions_to_inline.clear();
            {
                std::map<ASR::Function_t*, bool> needs_inline_memo;
                std::set<ASR::Function_t*> on_stack;
                if (!plan_device_function_inlining(x.m_body, x.n_body,
                        needs_inline_memo, on_stack)) {
                    // Some callee that must be inlined cannot be
                    // (recursive, early `return`, nested scopes, or
                    // called from a position with nowhere to put the
                    // result). Emitting a shader that cannot compile
                    // would be worse than not offloading at all.
                    functions_to_inline.clear();
                    GpuOffloadReport::emit(loc, report_proc,
                        "device-function-cannot-be-inlined");
                    return;
                }
            }
        }

        // Splice the planned device functions into the loop body. This
        // must come first among the rewrites below: the intrinsic and
        // array-section inliners then see the spliced-in statements too.
        // The splice is recorded so that it can be undone: the workspace
        // pre-flight right below needs the spliced shape, but must still
        // be able to leave the loop untouched when it declines.
        GpuLoopBodySnapshot splice_snapshot;
        std::vector<ScopeArrayDims> scope_dims_undo;
        auto restore_loop = [&]() {
            for (auto it = scope_dims_undo.rbegin();
                    it != scope_dims_undo.rend(); ++it) {
                it->var->m_type = it->type;
            }
            scope_dims_undo.clear();
            splice_snapshot.restore();
        };
        {
            ASR::DoConcurrentLoop_t &xx =
                const_cast<ASR::DoConcurrentLoop_t&>(x);
            splice_snapshot.record(xx, current_scope);
            inline_device_function_calls(xx.m_body, xx.n_body);
            functions_to_inline.clear();
            // Run-time sized alias temporaries become BLOCK locals here,
            // ahead of the workspace pre-flight below, so that the
            // pre-flight sizes them too and can still decline the loop.
            materialize_runtime_alias_blocks(xx);
            size_scope_array_temporaries(xx.m_body, xx.n_body,
                scope_dims_undo);
        }

        // Each run-time sized local of a kernel BLOCK becomes a per-thread
        // workspace buffer, which the host has to size before it launches
        // the kernel. An extent the host cannot work out from the kernel
        // arguments is a code generation error -- raised long after the
        // pass has committed to offloading, and so a hard build failure.
        // Run the backend's own resolution here instead, while the loop
        // can still be left on the host. This is the last point at which
        // it can be: the workspaces only exist once the callees are
        // spliced in, and the rewrites below are not reversible.
        if (pass_options.gpu_offload_metal) {
            std::vector<std::string> kernel_arg_names;
            collect_kernel_arg_names(x, enclosing_block_scopes,
                kernel_arg_names);
            std::string unresolved_name;
            if (!gpu_block_workspace_extents_resolvable(al, x.m_body,
                    x.n_body, kernel_arg_names, unresolved_name)) {
                restore_loop();
                GpuOffloadReport::set_detail("sym=" + unresolved_name);
                GpuOffloadReport::emit(loc, report_proc,
                    "workspace-extent-unresolvable");
                return;
            }
            GpuNestedSectionFinder nested_section;
            for (size_t i = 0; i < x.n_body; i++) {
                nested_section.visit_stmt(*x.m_body[i]);
            }
            if (nested_section.found) {
                restore_loop();
                GpuOffloadReport::emit(loc, report_proc,
                    "nested-section-cannot-be-addressed");
                return;
            }
        }

        // Inline IntrinsicArrayFunction All before kernel extraction
        all_reduction_targets.clear();
        inline_intrinsic_all(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Hoist matmuls out of the expression positions the matmul
        // lowering below cannot see them in.
        hoist_nested_matmuls(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline IntrinsicArrayFunction MatMul before kernel extraction
        inline_intrinsic_matmul(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline IntrinsicArrayFunction DotProduct before kernel
        // extraction
        inline_intrinsic_dot_product(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline IntrinsicArrayFunction Sum before kernel extraction
        inline_intrinsic_sum(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Also inline Sum in helper functions called from the
        // DoConcurrent body. This ensures that sum(f(x)) patterns
        // inside helper functions are expanded into loops before
        // kernel extraction, avoiding allocatable temporaries that
        // cannot be represented as VLAs in Metal shaders.
        {
            GpuFunctionCollector sum_fc;
            for (size_t i = 0; i < x.n_body; i++) {
                sum_fc.visit_stmt(*x.m_body[i]);
            }
            bool sum_added = true;
            while (sum_added) {
                sum_added = false;
                GpuFunctionCollector sum_tc;
                for (auto &[fn_name, fn_sym] : sum_fc.functions) {
                    ASR::symbol_t *resolved =
                        ASRUtils::symbol_get_past_external(fn_sym);
                    if (ASR::is_a<ASR::Function_t>(*resolved)) {
                        ASR::Function_t *fn =
                            ASR::down_cast<ASR::Function_t>(resolved);
                        for (size_t i = 0; i < fn->n_body; i++) {
                            sum_tc.visit_stmt(*fn->m_body[i]);
                        }
                    }
                }
                for (auto &[name, sym] : sum_tc.functions) {
                    if (sum_fc.functions.find(name) ==
                            sum_fc.functions.end()) {
                        sum_fc.functions[name] = sym;
                        sum_added = true;
                    }
                }
            }
            for (auto &[fn_name, fn_sym] : sum_fc.functions) {
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(fn_sym);
                if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    ASR::Function_t *fn =
                        ASR::down_cast<ASR::Function_t>(resolved);
                    inline_dot_product_in_stmts(fn->m_body, fn->n_body,
                        fn->m_symtab);
                    inline_sum_in_stmts(fn->m_body, fn->n_body,
                        fn->m_symtab);
                }
            }
        }

        // Inline IntrinsicArrayFunction Transpose before kernel extraction
        inline_intrinsic_transpose(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Materialise temporaries for assignments whose target and value
        // overlap the same array, before the element loops below are
        // built from them.
        materialize_aliased_assignments(
            const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline ArraySection assignments before kernel extraction
        inline_array_section_assignment(
            const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline whole-array elemental assignments (e.g., b = abs(a(:,l)))
        inline_elemental_array_var_assignment(
            const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Recursive helper to inline an AssociateBlock's body.
        // Collects Associate mappings into assoc_map and non-Associate
        // statements into resolved_stmts. Handles nested
        // AssociateBlockCalls by recursing into inner blocks.
        std::function<void(ASR::AssociateBlock_t*,
                           std::map<ASR::symbol_t*, ASR::expr_t*>&,
                           Vec<ASR::stmt_t*>&)>
            inline_assoc_body = [&](ASR::AssociateBlock_t *ab,
                                    std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map,
                                    Vec<ASR::stmt_t*> &resolved_stmts) {
            for (size_t ai = 0; ai < ab->n_body; ai++) {
                if (ASR::is_a<ASR::Associate_t>(*ab->m_body[ai])) {
                    ASR::Associate_t *assoc =
                        ASR::down_cast<ASR::Associate_t>(
                            ab->m_body[ai]);
                    if (ASR::is_a<ASR::Var_t>(*assoc->m_target)) {
                        ASR::symbol_t *sym =
                            ASR::down_cast<ASR::Var_t>(
                                assoc->m_target)->m_v;
                        ASRUtils::ExprStmtDuplicator dup(al);
                        dup.success = true;
                        ASR::expr_t *value =
                            dup.duplicate_expr(assoc->m_value);
                        if (!assoc_map.empty()) {
                            AssociateVarResolver resolver(al, assoc_map);
                            resolver.current_expr = &value;
                            resolver.replace_expr(value);
                        }
                        assoc_map[sym] = value;
                    }
                } else if (ASR::is_a<ASR::AssociateBlockCall_t>(
                               *ab->m_body[ai])) {
                    ASR::AssociateBlockCall_t *inner_abc =
                        ASR::down_cast<ASR::AssociateBlockCall_t>(
                            ab->m_body[ai]);
                    if (ASR::is_a<ASR::AssociateBlock_t>(
                            *inner_abc->m_m)) {
                        ASR::AssociateBlock_t *inner_ab =
                            ASR::down_cast<ASR::AssociateBlock_t>(
                                inner_abc->m_m);
                        Vec<ASR::stmt_t*> inner_stmts;
                        inner_stmts.reserve(al, inner_ab->n_body);
                        inline_assoc_body(inner_ab, assoc_map,
                            inner_stmts);
                        // Resolve the inner statements now: once they are
                        // wrapped in a Block below the caller's resolver
                        // no longer reaches them.
                        if (!assoc_map.empty()) {
                            AssociateVarResolverVisitor inner_resolver(
                                al, assoc_map);
                            for (size_t ii = 0; ii < inner_stmts.n; ii++) {
                                inner_resolver.visit_stmt(
                                    *inner_stmts.p[ii]);
                            }
                        }
                        ASR::stmt_t *inner_call =
                            wrap_assoc_scope_in_block(inner_ab,
                                inner_stmts, ab->m_symtab);
                        if (inner_call) {
                            resolved_stmts.push_back(al, inner_call);
                        } else {
                            for (size_t ii = 0; ii < inner_stmts.n; ii++) {
                                resolved_stmts.push_back(al,
                                    inner_stmts.p[ii]);
                            }
                            migrate_inlined_assoc_symbols(inner_ab,
                                ab->m_symtab);
                        }
                        std::string inner_name = inner_ab->m_name;
                        ab->m_symtab->erase_symbol(inner_name);
                    } else {
                        resolved_stmts.push_back(al,
                            ab->m_body[ai]);
                    }
                } else if (ASR::is_a<ASR::Assignment_t>(
                               *ab->m_body[ai])) {
                    ASR::Assignment_t *asgn =
                        ASR::down_cast<ASR::Assignment_t>(
                            ab->m_body[ai]);
                    if (ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                        ASR::symbol_t *sym =
                            ASR::down_cast<ASR::Var_t>(
                                asgn->m_target)->m_v;
                        if (ASR::is_a<ASR::Variable_t>(*sym) &&
                            ASR::down_cast<ASR::Variable_t>(sym)
                                ->m_parent_symtab == ab->m_symtab &&
                            assoc_map.find(sym) == assoc_map.end() &&
                            is_single_assignment_binding(sym,
                                ab->m_body, ab->n_body)) {
                            assoc_map[sym] = asgn->m_value;
                        } else {
                            resolved_stmts.push_back(al,
                                ab->m_body[ai]);
                        }
                    } else {
                        resolved_stmts.push_back(al, ab->m_body[ai]);
                    }
                } else {
                    resolved_stmts.push_back(al, ab->m_body[ai]);
                }
            }
        };

        // Resolve AssociateBlocks inside the do concurrent body (e.g.,
        // block { associate(nh => n) ... } within the loop). GPU kernels
        // cannot use Pointer-based associate aliases, so we inline the
        // associate targets and replace the AssociateBlockCall with the
        // resolved statements.
        for (size_t bi = 0; bi < x.n_body; bi++) {
            if (!ASR::is_a<ASR::BlockCall_t>(*x.m_body[bi])) continue;
            ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
                x.m_body[bi]);
            if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
            Vec<ASR::stmt_t*> new_block_body;
            new_block_body.reserve(al, block->n_body);
            bool changed = false;
            for (size_t si = 0; si < block->n_body; si++) {
                if (!ASR::is_a<ASR::AssociateBlockCall_t>(
                        *block->m_body[si])) {
                    new_block_body.push_back(al, block->m_body[si]);
                    continue;
                }
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        block->m_body[si]);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                    new_block_body.push_back(al, block->m_body[si]);
                    continue;
                }
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map(
                    enclosing_assoc_map);
                Vec<ASR::stmt_t*> resolved_stmts;
                resolved_stmts.reserve(al, ab->n_body);
                inline_assoc_body(ab, assoc_map, resolved_stmts);
                if (!assoc_map.empty()) {
                    AssociateVarResolverVisitor resolver(al, assoc_map);
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        resolver.visit_stmt(*resolved_stmts.p[ri]);
                    }
                }
                // If the AssociateBlock still owns variables referenced by
                // the resolved statements (an array-valued selector
                // temporary), keep the scope alive as a Block instead of
                // dropping it.
                ASR::stmt_t *block_call = wrap_assoc_scope_in_block(
                    ab, resolved_stmts, block->m_symtab);
                if (block_call) {
                    new_block_body.push_back(al, block_call);
                } else {
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        new_block_body.push_back(al, resolved_stmts.p[ri]);
                    }
                    // Migrate ExternalSymbol entries (e.g., type-bound
                    // procedure references like `1_t_f`) from the
                    // AssociateBlock's symtab to the enclosing scope
                    // before erasing it. These symbols are still
                    // referenced by FunctionCall/SubroutineCall nodes
                    // in the resolved statements and must remain
                    // reachable for import_struct_def.
                    migrate_inlined_assoc_symbols(ab, current_scope);
                }
                std::string ab_name = ab->m_name;
                block->m_symtab->erase_symbol(ab_name);
                changed = true;
            }
            if (changed) {
                block->m_body = new_block_body.p;
                block->n_body = new_block_body.n;
            }
        }

        // Resolve bare AssociateBlockCall statements directly in the
        // do concurrent body (not wrapped in a BlockCall). GPU kernels
        // cannot use Pointer-based associate aliases, so we inline the
        // associate targets and replace each AssociateBlockCall with
        // the resolved statements.
        {
            Vec<ASR::stmt_t*> new_dc_body;
            new_dc_body.reserve(al, x.n_body);
            bool dc_changed = false;
            for (size_t bi = 0; bi < x.n_body; bi++) {
                if (!ASR::is_a<ASR::AssociateBlockCall_t>(*x.m_body[bi])) {
                    new_dc_body.push_back(al, x.m_body[bi]);
                    continue;
                }
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        x.m_body[bi]);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                    new_dc_body.push_back(al, x.m_body[bi]);
                    continue;
                }
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                // Start with mappings from enclosing AssociateBlocks so
                // that references to outer associate variables (e.g., `m`
                // from an outer `associate(m => n)`) are resolved even
                // when they appear inside an inner associate block.
                std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map(
                    enclosing_assoc_map);
                Vec<ASR::stmt_t*> resolved_stmts;
                resolved_stmts.reserve(al, ab->n_body);
                inline_assoc_body(ab, assoc_map, resolved_stmts);
                if (!assoc_map.empty()) {
                    AssociateVarResolverVisitor resolver(al, assoc_map);
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        resolver.visit_stmt(*resolved_stmts.p[ri]);
                    }
                }
                // Keep the scope alive as a Block when it still owns
                // variables referenced by the resolved statements.
                ASR::stmt_t *block_call = wrap_assoc_scope_in_block(
                    ab, resolved_stmts, current_scope);
                if (block_call) {
                    new_dc_body.push_back(al, block_call);
                } else {
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        new_dc_body.push_back(al, resolved_stmts.p[ri]);
                    }
                    // Migrate ExternalSymbol entries from the
                    // AssociateBlock's symtab to the enclosing scope
                    // before erasing it (same as above for BlockCall).
                    migrate_inlined_assoc_symbols(ab, current_scope);
                }
                std::string ab_name = ab->m_name;
                current_scope->erase_symbol(ab_name);
                dc_changed = true;
            }
            if (dc_changed) {
                ASR::DoConcurrentLoop_t &xx =
                    const_cast<ASR::DoConcurrentLoop_t&>(x);
                xx.m_body = new_dc_body.p;
                xx.n_body = new_dc_body.n;
            }
        }

        // A strided section actual argument cannot be handed to a device
        // function as a base pointer; gather it into a contiguous
        // temporary first. This runs after the ASSOCIATE scopes above have
        // been inlined, so a section whose bounds come from an associate
        // name is gathered with the selector substituted in.
        gather_strided_section_arguments(
            const_cast<ASR::DoConcurrentLoop_t&>(x));

        // 1. Collect all symbols from body AND head expressions
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
        collect_involved_syms(x, enclosing_block_scopes, involved_syms);

        // Collect loop variable names
        std::vector<std::string> loop_var_names;
        for (size_t d = 0; d < n_dims; d++) {
            ASR::Var_t *lv = down_cast<ASR::Var_t>(x.m_head[d].m_v);
            loop_var_names.push_back(ASRUtils::symbol_name(lv->m_v));
        }

        // Find local scalar temporaries (assigned but not arrays, not loop vars)
        std::set<std::string> local_vars, assigned_vars;
        GpuLocalVarCollector lv_collector(local_vars, assigned_vars, enclosing_block_scopes);
        for (size_t i = 0; i < x.n_body; i++) {
            lv_collector.visit_stmt(*x.m_body[i]);
        }

        // Separate into kernel params vs local vars
        // Params: arrays + scalars that are read but NOT assigned in loop body
        // (unless they're also read from arrays, in which case they're params)
        // Local: scalars that are assigned in the loop body and not arrays
        std::set<std::string> loop_var_set(loop_var_names.begin(), loop_var_names.end());

        // Remove loop variables from involved_syms (kernel computes them)
        for (auto &lvn : loop_var_names) {
            involved_syms.erase(lvn);
        }

        // Identify which symbols are local temporaries (assigned scalar, non-array)
        // vs kernel parameters (arrays or read-only scalars).
        // Assigned scalars are kernel-local unless they are reduction
        // targets from inlined all(), or are referenced after the do
        // concurrent loop (liveout) — those need to be communicated back
        // to the host via 1-element array device buffers.

        // Collect variables referenced in statements after this do concurrent
        // in the parent body, to identify liveout scalars.
        std::set<std::string> post_loop_vars;
        {
            ASR::stmt_t **parent_body = nullptr;
            size_t parent_n_body = 0;
            SymbolTable *scope = current_scope;
            while (scope && scope->asr_owner) {
                if (scope->asr_owner->type == ASR::asrType::symbol) {
                    ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(
                        scope->asr_owner);
                    if (ASR::is_a<ASR::Program_t>(*owner_sym)) {
                        ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(
                            owner_sym);
                        parent_body = prog->m_body;
                        parent_n_body = prog->n_body;
                        break;
                    } else if (ASR::is_a<ASR::Function_t>(*owner_sym)) {
                        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                            owner_sym);
                        parent_body = fn->m_body;
                        parent_n_body = fn->n_body;
                        break;
                    } else if (ASR::is_a<ASR::Block_t>(*owner_sym)) {
                        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(
                            owner_sym);
                        parent_body = blk->m_body;
                        parent_n_body = blk->n_body;
                        break;
                    }
                }
                scope = scope->parent;
            }
            if (parent_body) {
                bool found_dc = false;
                for (size_t si = 0; si < parent_n_body; si++) {
                    if (!found_dc) {
                        if (parent_body[si]->base.loc.first == loc.first &&
                                parent_body[si]->base.loc.last == loc.last) {
                            found_dc = true;
                        }
                        continue;
                    }
                    PostLoopVarCollector plvc(post_loop_vars);
                    plvc.visit_stmt(*parent_body[si]);
                }
            }
        }

        std::set<std::string> local_scalar_names;
        for (auto &name : assigned_vars) {
            if (loop_var_set.count(name)) continue;
            if (all_reduction_targets.count(name)) continue;
            if (post_loop_vars.count(name)) continue;
            auto it = involved_syms.find(name);
            if (it != involved_syms.end()) {
                ASR::ttype_t *type = it->second.first;
                if (!ASRUtils::is_array(type)) {
                    local_scalar_names.insert(name);
                }
            }
        }

        // Remove local scalars from involved_syms (they become kernel locals)
        for (auto &name : local_scalar_names) {
            involved_syms.erase(name);
        }

        // Collect optional variables from involved_syms. When an optional
        // argument is used inside a do concurrent body guarded by present(),
        // the kernel launch and all buffer setup must be skipped when the
        // argument is not present, otherwise the host will segfault trying
        // to read a null descriptor.
        std::vector<ASR::symbol_t*> optional_syms;
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::symbol_t *sym = this->current_scope->resolve_symbol(sym_name);
            if (!sym) continue;
            ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(sym);
            if (!ASR::is_a<ASR::Variable_t>(*resolved)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(resolved);
            if (var->m_presence == ASR::presenceType::Optional) {
                optional_syms.push_back(sym);
            }
        }

        // Wrap liveout scalars (assigned user variables still in
        // involved_syms) in 1-element FixedSizeArrays so they can be
        // passed as writable device buffers and read back after the kernel.
        struct LiveoutScalarInfo {
            std::string orig_name;
            std::string buf_name;
            ASR::symbol_t *host_buf_sym;
            ASR::symbol_t *orig_scalar_sym;
            ASR::ttype_t *scalar_type;
        };
        std::vector<LiveoutScalarInfo> liveout_scalars;
        {
            ASR::ttype_t *int4_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            std::vector<std::string> liveout_names;
            for (auto &name : assigned_vars) {
                if (loop_var_set.count(name)) continue;
                if (local_scalar_names.count(name)) continue;
                auto it = involved_syms.find(name);
                if (it != involved_syms.end()) {
                    ASR::ttype_t *type = it->second.first;
                    if (!ASRUtils::is_array(type)) {
                        liveout_names.push_back(name);
                    }
                }
            }
            for (auto &name : liveout_names) {
                auto it = involved_syms.find(name);
                ASR::ttype_t *scalar_type = it->second.first;
                ASR::symbol_t *orig_sym = current_scope->resolve_symbol(name);

                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, 1, int4_type,
                        ASR::integerbozType::Decimal));
                dim.m_length = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, 1, int4_type,
                        ASR::integerbozType::Decimal));
                Vec<ASR::dimension_t> dims_vec;
                dims_vec.reserve(al, 1);
                dims_vec.push_back(al, dim);
                ASR::ttype_t *arr_type = ASRUtils::TYPE(
                    ASR::make_Array_t(al, loc,
                        ASRUtils::duplicate_type(al, scalar_type),
                        dims_vec.p, 1,
                        ASR::array_physical_typeType::FixedSizeArray));

                std::string buf_name = current_scope->get_unique_name(
                    "__gpu_buf_" + name);
                ASR::symbol_t *buf_sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, current_scope,
                        s2c(al, buf_name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, arr_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                current_scope->add_symbol(buf_name, buf_sym);

                it->second.first = arr_type;

                liveout_scalars.push_back(
                    {name, buf_name, buf_sym, orig_sym, scalar_type});
            }
        }

        // Decompose struct variables with allocatable array members.
        // Metal cannot represent allocatable descriptors inside structs,
        // so we extract each allocatable array member into a separate
        // kernel buffer parameter and replace StructInstanceMember
        // references in the body with the new flat-array Var.
        GpuAllocStructMemberCollector alloc_collector;
        for (size_t i = 0; i < x.n_body; i++) {
            alloc_collector.visit_stmt(*x.m_body[i]);
        }
        // Also scan array dimension expressions of involved symbols for
        // StructInstanceMember accesses. VLA arrays sized by struct
        // members (e.g., `integer :: n(x%m)`) constitute a non-allocatable
        // access that must prevent struct removal from involved_syms.
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::symbol_t *sym = current_scope->resolve_symbol(sym_name);
            if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_start)
                    alloc_collector.visit_expr(*arr->m_dims[d].m_start);
                if (arr->m_dims[d].m_length)
                    alloc_collector.visit_expr(*arr->m_dims[d].m_length);
            }
        }
        // Maps (struct_name, member_name) -> decomposed parameter name
        std::map<std::pair<std::string, std::string>, std::string>
            decomp_map;
        // Info for creating host-side call arguments later
        struct DecompInfo {
            std::string struct_name;
            std::string member_name;
            std::string param_name;
            ASR::symbol_t *orig_mem_sym;
            ASR::ttype_t *alloc_type;
        };
        std::vector<DecompInfo> decomp_infos;
        for (auto &[struct_name, members] :
                alloc_collector.alloc_members) {
            if (involved_syms.find(struct_name) == involved_syms.end())
                continue;
            for (auto &[mem_name, mem_info] : members) {
                std::string param_name = struct_name + "__" + mem_name;
                decomp_map[{struct_name, mem_name}] = param_name;
                decomp_infos.push_back({struct_name, mem_name,
                    param_name, mem_info.first, mem_info.second});
            }
            // If struct only accessed through allocatable members,
            // remove from involved_syms (it won't be passed as a
            // kernel parameter)
            if (alloc_collector.has_non_alloc_access.find(struct_name)
                    == alloc_collector.has_non_alloc_access.end()) {
                involved_syms.erase(struct_name);
            }
        }

        // 2. Create kernel scope and parameters
        SymbolTable *tu_symtab = tu.m_symtab;
        std::string kernel_name = tu_symtab->get_unique_name(
            "__lfortran_gpu_kernel_" + std::to_string(gpu_kernel_counter++));
        SymbolTable *kernel_scope = al.make_new<SymbolTable>(tu_symtab);

        Vec<ASR::expr_t*> kernel_args;
        kernel_args.reserve(al, involved_syms.size());
        Vec<ASR::call_arg_t> call_args;
        call_args.reserve(al, involved_syms.size());

        SymbolTable *orig_scope = this->current_scope;

        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *type = sym_info.first;

            // For struct-typed variables, import the Struct into kernel scope
            ASR::symbol_t *type_decl = nullptr;
            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            if (orig_sym) {
                type_decl = import_struct_type(orig_sym,
                    orig_scope, kernel_scope, loc);
            }

            // Parameter variables are compile-time constants without
            // runtime storage. Clone them into the kernel scope with
            // their values preserved instead of passing as GPU buffers.
            if (orig_sym) {
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(orig_sym);
                if (ASR::is_a<ASR::Variable_t>(*resolved)) {
                    ASR::Variable_t *orig_var =
                        ASR::down_cast<ASR::Variable_t>(resolved);
                    if (orig_var->m_storage ==
                            ASR::storage_typeType::Parameter) {
                        ASR::symbol_t *cloned =
                            ASR::down_cast<ASR::symbol_t>(
                                ASRUtils::make_Variable_t_util(al, loc,
                                    kernel_scope, s2c(al, sym_name),
                                    nullptr, 0,
                                    ASR::intentType::Local,
                                    orig_var->m_value,
                                    orig_var->m_value,
                                    ASR::storage_typeType::Parameter,
                                    ASRUtils::duplicate_type(al,
                                        orig_var->m_type),
                                    nullptr, orig_var->m_abi,
                                    orig_var->m_access,
                                    ASR::presenceType::Required, false));
                        kernel_scope->add_symbol(sym_name, cloned);
                        continue;
                    }
                }
            }

            // Strip Allocatable/Pointer wrapper: GPU kernel parameters
            // receive raw array data, not array descriptors
            ASR::ttype_t *dup_type = ASRUtils::duplicate_type(al,
                ASRUtils::type_get_past_allocatable_pointer(type));

            // Recompute dependencies from the type alone (symbolic_value
            // and value are nullptr for kernel parameters)
            SetChar deps_vec;
            deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(
                al, deps_vec, dup_type, nullptr, nullptr, sym_name);

            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, sym_name), deps_vec.p, deps_vec.size(),
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, dup_type,
                    type_decl, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(sym_name, param);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));

            ASR::call_arg_t carg;
            carg.loc = loc;
            // For liveout scalars, use the host-side 1-element array
            // buffer as the call arg so it's passed as a device buffer
            bool is_liveout = false;
            for (auto &ls : liveout_scalars) {
                if (ls.orig_name == sym_name) {
                    carg.m_value = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, ls.host_buf_sym));
                    is_liveout = true;
                    break;
                }
            }
            if (!is_liveout) {
                carg.m_value = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, orig_sym));
            }
            call_args.push_back(al, carg);
        }

        // Create kernel parameters for decomposed allocatable struct
        // members. Each allocatable array member becomes a separate
        // flat-array buffer parameter.
        for (auto &di : decomp_infos) {
            ASR::ttype_t *flat_type = ASRUtils::duplicate_type(al,
                ASRUtils::type_get_past_allocatable(di.alloc_type));

            ASR::symbol_t *flat_type_decl = nullptr;
            if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(flat_type))) {
                ASR::symbol_t *mem_resolved =
                    ASRUtils::symbol_get_past_external(di.orig_mem_sym);
                if (is_a<ASR::Variable_t>(*mem_resolved)) {
                    ASR::Variable_t *mv =
                        down_cast<ASR::Variable_t>(mem_resolved);
                    if (mv->m_type_declaration) {
                        ASR::symbol_t *inner_struct_sym =
                            ASRUtils::symbol_get_past_external(
                                mv->m_type_declaration);
                        if (is_a<ASR::Struct_t>(*inner_struct_sym)) {
                            flat_type_decl = import_struct_def(
                                down_cast<ASR::Struct_t>(inner_struct_sym),
                                orig_scope, kernel_scope, loc);
                        }
                    }
                }
            }

            SetChar deps_vec;
            deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(
                al, deps_vec, flat_type, nullptr, nullptr, di.param_name);

            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, di.param_name), deps_vec.p, deps_vec.size(),
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, flat_type,
                    flat_type_decl, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            kernel_scope->add_symbol(di.param_name, param);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));

            // Host-side: pass StructInstanceMember(Var(x), member)
            ASR::symbol_t *orig_struct_sym =
                orig_scope->resolve_symbol(di.struct_name);
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        orig_struct_sym)),
                    di.orig_mem_sym, di.alloc_type, nullptr));
            call_args.push_back(al, carg);
        }

        // Pass dimension sizes for decomposed allocatable struct
        // members so the kernel can compute ArraySize and strides.
        {
            ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            for (auto &di : decomp_infos) {
                ASR::ttype_t *inner =
                    ASRUtils::type_get_past_allocatable(di.alloc_type);
                if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);

                ASR::symbol_t *k_sym =
                    kernel_scope->get_symbol(di.param_name);
                LCOMPILERS_ASSERT(k_sym);
                ASR::Variable_t *k_var =
                    ASR::down_cast<ASR::Variable_t>(k_sym);
                ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable(k_var->m_type));

                ASR::symbol_t *orig_struct_sym =
                    orig_scope->resolve_symbol(di.struct_name);
                ASR::expr_t *host_member_expr = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_struct_sym)),
                        di.orig_mem_sym, di.alloc_type, nullptr));

                for (size_t d = 0; d < arr->n_dims; d++) {
                    std::string dim_name =
                        gpu_dim_arg_name(di.param_name, d);
                    ASR::symbol_t *dim_sym =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                kernel_scope, s2c(al, dim_name),
                                nullptr, 0,
                                ASR::intentType::InOut, nullptr,
                                nullptr,
                                ASR::storage_typeType::Default,
                                ASRUtils::duplicate_type(al,
                                    int_type_dim),
                                nullptr, ASR::abiType::Source,
                                ASR::accessType::Public,
                                ASR::presenceType::Required, false));
                    kernel_scope->add_symbol(dim_name, dim_sym);
                    kernel_args.push_back(al,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            dim_sym)));

                    ASR::expr_t *dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc,
                            (int64_t)(d + 1), int_type_dim,
                            ASR::integerbozType::Decimal));
                    ASR::expr_t *host_size = ASRUtils::EXPR(
                        ASR::make_ArraySize_t(al, loc,
                            host_member_expr, dim_expr,
                            int_type_dim, nullptr));
                    ASR::call_arg_t carg;
                    carg.loc = loc;
                    carg.m_value = host_size;
                    call_args.push_back(al, carg);

                    k_arr->m_dims[d].m_length = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, dim_sym));
                    if (!k_arr->m_dims[d].m_start) {
                        // Pass lower bound as kernel parameter
                        std::string lb_name = "__lb_" + di.param_name
                            + "_" + std::to_string(d);
                        ASR::symbol_t *lb_sym = ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                                s2c(al, lb_name), nullptr, 0,
                                ASR::intentType::InOut, nullptr, nullptr,
                                ASR::storage_typeType::Default,
                                ASRUtils::duplicate_type(al, int_type_dim),
                                nullptr, ASR::abiType::Source,
                                ASR::accessType::Public,
                                ASR::presenceType::Required, false));
                        kernel_scope->add_symbol(lb_name, lb_sym);
                        kernel_args.push_back(al,
                            ASRUtils::EXPR(ASR::make_Var_t(al, loc, lb_sym)));
                        // Host-side value: lbound(struct%member, dim=d+1)
                        ASR::expr_t *lb_dim_expr = ASRUtils::EXPR(
                            ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                                int_type_dim, ASR::integerbozType::Decimal));
                        ASR::expr_t *host_lb = ASRUtils::EXPR(
                            ASR::make_ArrayBound_t(al, loc,
                                host_member_expr, lb_dim_expr,
                                int_type_dim,
                                ASR::arrayboundType::LBound, nullptr));
                        ASR::call_arg_t lb_carg;
                        lb_carg.loc = loc;
                        lb_carg.m_value = host_lb;
                        call_args.push_back(al, lb_carg);
                        k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                            ASR::make_Var_t(al, loc, lb_sym));
                    }
                }
            }
        }
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *orig_type = sym_info.first;
            // Allocatable and pointer arrays are both deferred-shape:
            // their extents and lower bounds are only known at run time
            // and must be passed to the kernel as extra scalar
            // arguments so it can compute strides.
            if (!ASRUtils::is_allocatable_or_pointer(orig_type)) continue;
            ASR::ttype_t *inner =
                ASRUtils::type_get_past_allocatable_pointer(orig_type);
            if (!ASR::is_a<ASR::Array_t>(*inner)) continue;

            // Locate the kernel-scope Variable whose type we must update
            ASR::symbol_t *k_sym = kernel_scope->get_symbol(sym_name);
            LCOMPILERS_ASSERT(k_sym);
            ASR::Variable_t *k_var = ASR::down_cast<ASR::Variable_t>(k_sym);
            ASR::ttype_t *k_type = k_var->m_type;
            if (!ASR::is_a<ASR::Array_t>(
                    *ASRUtils::type_get_past_allocatable_pointer(k_type))) {
                continue;
            }
            ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable_pointer(k_type));

            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            for (size_t d = 0; d < k_arr->n_dims; d++) {
                std::string dim_name = gpu_dim_arg_name(sym_name, d);
                ASR::symbol_t *dim_sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                        s2c(al, dim_name), nullptr, 0,
                        ASR::intentType::InOut, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type_dim),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                kernel_scope->add_symbol(dim_name, dim_sym);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, dim_sym)));

                // Host-side value: size(arr, dim=d+1)
                ASR::expr_t *dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                        int_type_dim, ASR::integerbozType::Decimal));
                ASR::expr_t *host_size = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym)),
                        dim_expr, int_type_dim, nullptr));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_size;
                call_args.push_back(al, carg);

                // Set dimension length in kernel-scope array type
                k_arr->m_dims[d].m_length = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, dim_sym));
                if (!k_arr->m_dims[d].m_start) {
                    // Pass lower bound as kernel parameter
                    std::string lb_name = "__lb_" + sym_name + "_"
                        + std::to_string(d);
                    ASR::symbol_t *lb_sym = ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                            s2c(al, lb_name), nullptr, 0,
                            ASR::intentType::InOut, nullptr, nullptr,
                            ASR::storage_typeType::Default,
                            ASRUtils::duplicate_type(al, int_type_dim),
                            nullptr, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                    kernel_scope->add_symbol(lb_name, lb_sym);
                    kernel_args.push_back(al,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, lb_sym)));
                    // Host-side value: lbound(arr, dim=d+1)
                    ASR::expr_t *lb_dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                            int_type_dim, ASR::integerbozType::Decimal));
                    ASR::expr_t *host_lb = ASRUtils::EXPR(
                        ASR::make_ArrayBound_t(al, loc,
                            ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym)),
                            lb_dim_expr, int_type_dim,
                            ASR::arrayboundType::LBound, nullptr));
                    ASR::call_arg_t lb_carg;
                    lb_carg.loc = loc;
                    lb_carg.m_value = host_lb;
                    call_args.push_back(al, lb_carg);
                    k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, lb_sym));
                }
            }
        }

        // Add total-size kernel parameters for allocatable array members
        // of struct-typed kernel parameters. These sizes are needed by
        // Metal inline functions that call size() on struct members.
        // Skip array-of-structs variables — StructInstanceMember requires
        // a scalar struct base, not an array of structs.
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *type = sym_info.first;
            ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
            if (ASRUtils::is_array(inner_t)) continue;
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(inner_t)))
                continue;
            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
            ASR::Variable_t *orig_var =
                down_cast<ASR::Variable_t>(orig_sym);
            if (!orig_var->m_type_declaration) continue;
            ASR::symbol_t *struct_sym =
                ASRUtils::symbol_get_past_external(
                    orig_var->m_type_declaration);
            if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
            ASR::Struct_t *st = down_cast<ASR::Struct_t>(struct_sym);
            ASR::ttype_t *int_type_sz = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            for (auto &mem_entry :
                    ASRUtils::collect_allocatable_array_members(st)) {
                const std::string &mem_name = mem_entry.first;
                ASR::Variable_t *mv = mem_entry.second;
                ASR::symbol_t *mem_sym = (ASR::symbol_t*)mv;
                std::string size_name = "__size_" + sym_name + "_"
                    + mem_name;
                ASR::symbol_t *size_sym =
                    ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc,
                            kernel_scope, s2c(al, size_name),
                            nullptr, 0,
                            ASR::intentType::InOut, nullptr,
                            nullptr,
                            ASR::storage_typeType::Default,
                            ASRUtils::duplicate_type(al, int_type_sz),
                            nullptr, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                kernel_scope->add_symbol(size_name, size_sym);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        size_sym)));
                // Host-side: size(struct%member) (total size)
                // Look up the member symbol in the original struct's
                // scope for the ExternalSymbol reference used in the
                // program scope (needed for StructInstanceMember).
                ASR::symbol_t *orig_mem_ref = nullptr;
                for (auto &scope_item :
                        orig_scope->get_scope()) {
                    if (!is_a<ASR::ExternalSymbol_t>(
                            *scope_item.second)) continue;
                    ASR::ExternalSymbol_t *es =
                        down_cast<ASR::ExternalSymbol_t>(
                            scope_item.second);
                    ASR::symbol_t *resolved =
                        ASRUtils::symbol_get_past_external(
                            es->m_external);
                    if (resolved == mem_sym) {
                        orig_mem_ref = scope_item.second;
                        break;
                    }
                }
                if (!orig_mem_ref) orig_mem_ref = mem_sym;
                ASR::expr_t *host_member = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_sym)),
                        orig_mem_ref, mv->m_type, nullptr));
                ASR::expr_t *host_size = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc,
                        host_member, nullptr, int_type_sz,
                        nullptr));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_size;
                call_args.push_back(al, carg);

                // For a component of rank > 1 the total size is not the
                // extent of any single dimension, so also pass each
                // per-dimension extent; size(struct%member, dim) in the
                // kernel reads these.
                ASR::ttype_t *mem_inner =
                    ASRUtils::type_get_past_allocatable(mv->m_type);
                size_t rank = ASR::down_cast<ASR::Array_t>(
                    mem_inner)->n_dims;
                if (rank <= 1) continue;
                for (size_t d = 0; d < rank; d++) {
                    std::string dim_size_name = size_name + "_dim"
                        + std::to_string(d + 1);
                    ASR::symbol_t *dim_size_sym =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                kernel_scope, s2c(al, dim_size_name),
                                nullptr, 0,
                                ASR::intentType::InOut, nullptr,
                                nullptr,
                                ASR::storage_typeType::Default,
                                ASRUtils::duplicate_type(al,
                                    int_type_sz),
                                nullptr, ASR::abiType::Source,
                                ASR::accessType::Public,
                                ASR::presenceType::Required, false));
                    kernel_scope->add_symbol(dim_size_name,
                        dim_size_sym);
                    kernel_args.push_back(al,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            dim_size_sym)));
                    ASR::expr_t *dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc,
                            (int64_t)(d + 1), int_type_sz,
                            ASR::integerbozType::Decimal));
                    ASR::expr_t *dim_member = ASRUtils::EXPR(
                        ASR::make_StructInstanceMember_t(al, loc,
                            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                                orig_sym)),
                            orig_mem_ref, mv->m_type, nullptr));
                    ASR::call_arg_t dim_carg;
                    dim_carg.loc = loc;
                    dim_carg.m_value = ASRUtils::EXPR(
                        ASR::make_ArraySize_t(al, loc, dim_member,
                            dim_expr, int_type_sz, nullptr));
                    call_args.push_back(al, dim_carg);
                }
            }
        }

        // Add allocatable-member data kernel parameters for struct-typed
        // kernel parameters that were NOT fully decomposed. These provide
        // the actual array data as separate device buffers so that Metal
        // inline functions can index into allocatable members.
        // Skip array-of-structs variables — StructInstanceMember requires
        // a scalar struct base, not an array of structs.
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *type = sym_info.first;
            ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
            if (ASRUtils::is_array(inner_t)) continue;
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(inner_t)))
                continue;
            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
            ASR::Variable_t *orig_var =
                down_cast<ASR::Variable_t>(orig_sym);
            if (!orig_var->m_type_declaration) continue;
            ASR::symbol_t *struct_sym =
                ASRUtils::symbol_get_past_external(
                    orig_var->m_type_declaration);
            if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
            ASR::Struct_t *st = down_cast<ASR::Struct_t>(struct_sym);
            for (auto &mem_entry :
                    ASRUtils::collect_allocatable_array_members(st)) {
                const std::string &mem_name = mem_entry.first;
                ASR::Variable_t *mv = mem_entry.second;
                ASR::symbol_t *mem_sym = (ASR::symbol_t*)mv;
                ASR::ttype_t *mem_inner =
                    ASRUtils::type_get_past_allocatable(mv->m_type);
                std::string data_name = "__data_" + sym_name + "_"
                    + mem_name;
                ASR::ttype_t *data_type =
                    ASRUtils::duplicate_type(al, mem_inner);
                ASR::symbol_t *data_type_decl = nullptr;
                if (ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(data_type)) &&
                        mv->m_type_declaration) {
                    ASR::symbol_t *inner_struct_sym =
                        ASRUtils::symbol_get_past_external(
                            mv->m_type_declaration);
                    if (is_a<ASR::Struct_t>(*inner_struct_sym)) {
                        data_type_decl = import_struct_def(
                            down_cast<ASR::Struct_t>(inner_struct_sym),
                            orig_scope, kernel_scope, loc);
                    }
                }
                SetChar deps_vec;
                deps_vec.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(
                    al, deps_vec, data_type, nullptr, nullptr,
                    data_name);
                ASR::symbol_t *data_sym =
                    ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc,
                            kernel_scope, s2c(al, data_name),
                            deps_vec.p, deps_vec.size(),
                            ASR::intentType::InOut, nullptr,
                            nullptr,
                            ASR::storage_typeType::Default,
                            data_type,
                            data_type_decl, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                kernel_scope->add_symbol(data_name, data_sym);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        data_sym)));
                ASR::symbol_t *orig_mem_ref = nullptr;
                for (auto &scope_item :
                        orig_scope->get_scope()) {
                    if (!is_a<ASR::ExternalSymbol_t>(
                            *scope_item.second)) continue;
                    ASR::ExternalSymbol_t *es =
                        down_cast<ASR::ExternalSymbol_t>(
                            scope_item.second);
                    ASR::symbol_t *resolved =
                        ASRUtils::symbol_get_past_external(
                            es->m_external);
                    if (resolved == mem_sym) {
                        orig_mem_ref = scope_item.second;
                        break;
                    }
                }
                if (!orig_mem_ref) orig_mem_ref = mem_sym;
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_sym)),
                        orig_mem_ref, mv->m_type, nullptr));
                call_args.push_back(al, carg);
            }
        }

        // Create loop variables in kernel scope (local, not parameters)
        for (size_t d = 0; d < n_dims; d++) {
            ASR::Var_t *lv = down_cast<ASR::Var_t>(x.m_head[d].m_v);
            ASR::ttype_t *loop_var_type = ASRUtils::symbol_type(lv->m_v);
            std::string lvn = loop_var_names[d];
            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, lvn), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, loop_var_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(lvn, param);
        }

        // Create local scalar temporaries in kernel scope
        for (auto &name : local_scalar_names) {
            auto it_orig = orig_scope->resolve_symbol(name);
            if (!it_orig) continue;
            ASR::ttype_t *type = ASRUtils::symbol_type(it_orig);
            ASR::symbol_t *type_decl = import_struct_type(it_orig,
                orig_scope, kernel_scope, loc);
            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, type),
                    type_decl, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(name, param);
        }

        // Import functions/subroutines called in the do concurrent body
        // into the kernel scope so FunctionCall/SubroutineCall nodes
        // can reference them after symbol remapping.
        // Collect transitively: if f() calls g(), both must be imported.
        {
            GpuFunctionCollector func_collector;
            for (size_t i = 0; i < x.n_body; i++) {
                func_collector.visit_stmt(*x.m_body[i]);
            }
            {
                bool added = true;
                while (added) {
                    added = false;
                    GpuFunctionCollector transitive_collector;
                    for (auto &[fn_name, fn_sym] : func_collector.functions) {
                        ASR::symbol_t *fn_resolved =
                            ASRUtils::symbol_get_past_external(fn_sym);
                        ASR::Function_t *fn = nullptr;
                        if (ASR::is_a<ASR::Function_t>(*fn_resolved)) {
                            fn = ASR::down_cast<ASR::Function_t>(fn_resolved);
                        } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                                *fn_resolved)) {
                            ASR::StructMethodDeclaration_t *smd =
                                ASR::down_cast<ASR::StructMethodDeclaration_t>(
                                    fn_resolved);
                            ASR::symbol_t *proc =
                                ASRUtils::symbol_get_past_external(smd->m_proc);
                            if (ASR::is_a<ASR::Function_t>(*proc)) {
                                fn = ASR::down_cast<ASR::Function_t>(proc);
                            }
                        }
                        if (fn) {
                            ASR::Function_t *fn_impl =
                                resolve_function_implementation(fn);
                            for (size_t i = 0; i < fn_impl->n_body; i++) {
                                transitive_collector.visit_stmt(
                                    *fn_impl->m_body[i]);
                            }
                        }
                    }
                    for (auto &[name, sym] : transitive_collector.functions) {
                        if (func_collector.functions.find(name) ==
                                func_collector.functions.end()) {
                            func_collector.functions[name] = sym;
                            added = true;
                        }
                    }
                }
            }
            ASRUtils::SymbolDuplicator sym_dup(al);
            for (auto &[func_name, func_sym] : func_collector.functions) {
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(func_sym);
                if (kernel_scope->get_symbol(func_name)) {
                    // ExternalSymbol already created (e.g., by
                    // import_struct_def). Still need to import the
                    // function body for StructMethodDeclaration calls
                    // so the Metal backend can generate shader code.
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*func_sym) &&
                           ASR::is_a<ASR::Function_t>(*resolved)) {
                    // The function is accessed via use-association
                    // (ExternalSymbol). Duplicate the underlying function
                    // body into the kernel scope so its types reference
                    // the kernel's struct copies (not the module's).
                    ASR::Function_t *resolved_func =
                        ASR::down_cast<ASR::Function_t>(resolved);
                    ASR::FunctionType_t *resolved_ftype =
                        ASR::down_cast<ASR::FunctionType_t>(
                            resolved_func->m_function_signature);
                    if (resolved_ftype->m_deftype ==
                            ASR::deftypeType::Interface) {
                        // Submodule interface: find the Implementation
                        // in a submodule already in the TU, or load it
                        // from disk (needed for --separate-compilation).
                        std::string pname =
                            ASRUtils::symbol_name(resolved);
                        bool found = false;
                        for (auto &tu_item :
                                tu.m_symtab->get_scope()) {
                            if (!ASR::is_a<ASR::Module_t>(
                                    *tu_item.second)) continue;
                            ASR::Module_t *mod =
                                ASR::down_cast<ASR::Module_t>(
                                    tu_item.second);
                            ASR::symbol_t *impl_sym =
                                mod->m_symtab->get_symbol(pname);
                            if (!impl_sym ||
                                !ASR::is_a<ASR::Function_t>(
                                    *impl_sym)) continue;
                            ASR::Function_t *impl_func =
                                ASR::down_cast<ASR::Function_t>(
                                    impl_sym);
                            ASR::FunctionType_t *impl_ft =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    impl_func
                                        ->m_function_signature);
                            if (impl_ft->m_deftype !=
                                    ASR::deftypeType::Implementation)
                                continue;
                            resolved = impl_sym;
                            found = true;
                            break;
                        }
                        if (!found) {
                            // Load submodule from smod file on disk.
                            SymbolTable *parent_st =
                                ASRUtils::symbol_parent_symtab(
                                    resolved);
                            if (parent_st->asr_owner &&
                                    parent_st->asr_owner->type ==
                                        ASR::asrType::symbol &&
                                    ASR::is_a<ASR::Module_t>(
                                        *ASR::down_cast<ASR::symbol_t>(
                                            parent_st->asr_owner))) {
                                std::string parent_mod =
                                    ASR::down_cast<ASR::Module_t>(
                                        ASR::down_cast<ASR::symbol_t>(
                                            parent_st->asr_owner))
                                        ->m_name;
                                std::string smod_prefix =
                                    parent_mod + "@";
                                std::vector<std::filesystem::path>
                                    mod_dirs;
                                mod_dirs.push_back(
                                    pass_options.runtime_library_dir);
                                mod_dirs.push_back(
                                    pass_options.mod_files_dir);
                                mod_dirs.insert(mod_dirs.end(),
                                    pass_options.include_dirs.begin(),
                                    pass_options.include_dirs.end());
                                for (auto &dir : mod_dirs) {
                                    if (dir.empty())
                                        dir = ".";
                                    if (!std::filesystem::is_directory(
                                            dir)) continue;
                                    for (auto &file :
                                            std::filesystem::
                                                directory_iterator(
                                                    dir)) {
                                        std::string fname =
                                            file.path().filename()
                                                .string();
                                        if (!startswith(fname,
                                                smod_prefix) ||
                                            !endswith(fname, ".smod"))
                                            continue;
                                        std::string content;
                                        if (!read_file(
                                                file.path().string(),
                                                content) ||
                                            content.empty())
                                            continue;
                                        LocationManager lm_tmp;
                                        auto res = load_modfile(
                                            al, content, false,
                                            *tu.m_symtab, lm_tmp);
                                        if (!res.ok) continue;
                                        load_submodule_deps(
                                            *res.result);
                                        fix_external_symbols(
                                            *res.result,
                                            *tu.m_symtab);
                                        lower_loaded_implied_do_loops(
                                            *res.result);
                                        ASR::Module_t *submod =
                                            ASRUtils::extract_module(
                                                *res.result);
                                        ASR::symbol_t *impl_sym =
                                            submod->m_symtab
                                                ->get_symbol(pname);
                                        if (!impl_sym ||
                                            !ASR::is_a<ASR::Function_t>(
                                                *impl_sym)) continue;
                                        ASR::Function_t *impl_func =
                                            ASR::down_cast<
                                                ASR::Function_t>(
                                                    impl_sym);
                                        ASR::FunctionType_t *impl_ft =
                                            ASR::down_cast<
                                                ASR::FunctionType_t>(
                                                    impl_func
                                                    ->m_function_signature);
                                        if (impl_ft->m_deftype !=
                                                ASR::deftypeType::
                                                    Implementation)
                                            continue;
                                        resolved = impl_sym;
                                        found = true;
                                        break;
                                    }
                                    if (found) break;
                                }
                            }
                        }
                    }
                    if (ASR::is_a<ASR::Function_t>(*resolved)) {
                        ASR::Function_t *rf =
                            ASR::down_cast<ASR::Function_t>(resolved);
                        ASR::FunctionType_t *rft =
                            ASR::down_cast<ASR::FunctionType_t>(
                                rf->m_function_signature);
                        if (rft->m_deftype ==
                                ASR::deftypeType::Interface) {
                            // Still an interface after searching TU and
                            // .smod files — the submodule body is
                            // unavailable (e.g., parallel build race).
                            // Error out instead of generating an empty
                            // function in the Metal shader.
                            throw LCompilersException(
                                "GPU Metal offload: cannot find "
                                "submodule implementation for '" +
                                std::string(rf->m_name) + "'; "
                                "ensure the submodule is compiled "
                                "before the file that uses it");
                        }
                    }
                    std::string real_name =
                        ASRUtils::symbol_name(resolved);
                    // When two modules define functions with the same
                    // name (e.g., both have "my_construct"), the first
                    // gets added under real_name. For subsequent
                    // collisions, sanitize the ExternalSymbol name to
                    // a valid C identifier to disambiguate.
                    std::string dup_name = real_name;
                    if (kernel_scope->get_symbol(real_name)) {
                        dup_name = func_name;
                        for (char &c : dup_name) {
                            if (c == '~' || c == '@') c = '_';
                        }
                    }
                    if (!kernel_scope->get_symbol(dup_name)) {
                        ASR::symbol_t *dup =
                            sym_dup.duplicate_Function(
                                ASR::down_cast<ASR::Function_t>(
                                    resolved),
                                kernel_scope);
                        if (dup) {
                            ASR::down_cast<ASR::Function_t>(dup)
                                ->m_name = s2c(al, dup_name);
                            kernel_scope->add_symbol(dup_name, dup);
                            // The duplicated function still references
                            // the module's struct definitions. Remap
                            // ExternalSymbol targets and Variable
                            // m_type_declarations to point to the
                            // kernel's struct copies instead.
                            fixup_struct_refs_in_scope(
                                ASR::down_cast<ASR::Function_t>(dup)
                                    ->m_symtab,
                                kernel_scope,
                                s2c(al, kernel_name));
                        }
                    }
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*func_sym) &&
                           !ASR::is_a<ASR::StructMethodDeclaration_t>(
                               *resolved)) {
                    // Non-function, non-method ExternalSymbol (e.g.,
                    // GenericProcedure from m_original_name). Create a
                    // matching ExternalSymbol in the kernel scope.
                    ASR::ExternalSymbol_t *es =
                        ASR::down_cast<ASR::ExternalSymbol_t>(func_sym);
                    ASR::asr_t *new_es = ASR::make_ExternalSymbol_t(
                        al, loc, kernel_scope, s2c(al, func_name),
                        es->m_external, es->m_module_name,
                        nullptr, 0, es->m_original_name,
                        es->m_access);
                    kernel_scope->add_symbol(func_name,
                        ASR::down_cast<ASR::symbol_t>(new_es));
                } else if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    // Skip functions that are already accessible through
                    // the kernel scope's parent chain (e.g., TU-scope
                    // generated helpers from the
                    // function_call_in_declaration pass).
                    if (kernel_scope->parent &&
                            kernel_scope->parent->resolve_symbol(
                                ASRUtils::symbol_name(resolved))) {
                        if (!ASR::is_a<ASR::StructMethodDeclaration_t>(
                                *resolved)) {
                            continue;
                        }
                    }
                    ASR::symbol_t *dup = sym_dup.duplicate_Function(
                        ASR::down_cast<ASR::Function_t>(resolved),
                        kernel_scope);
                    if (dup) {
                        kernel_scope->add_symbol(func_name, dup);
                    }
                } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                               *resolved)) {
                    // Type-bound procedure call: the resolved symbol is
                    // a StructMethodDeclaration inside a Struct's symtab.
                    // Create an ExternalSymbol in the kernel scope that
                    // points to the corresponding method declaration in
                    // the kernel's copy of the struct (imported earlier
                    // by import_struct_def for the struct-typed variable).
                    SymbolTable *method_st =
                        ASRUtils::symbol_parent_symtab(resolved);
                    if (method_st->asr_owner &&
                            method_st->asr_owner->type ==
                                ASR::asrType::symbol) {
                        ASR::symbol_t *struct_owner =
                            down_cast<ASR::symbol_t>(method_st->asr_owner);
                        if (is_a<ASR::Struct_t>(*struct_owner)) {
                            std::string struct_name =
                                down_cast<ASR::Struct_t>(struct_owner)
                                    ->m_name;
                            std::string orig_name =
                                ASRUtils::symbol_name(resolved);
                            ASR::symbol_t *kernel_struct =
                                find_kernel_struct(kernel_scope,
                                    struct_name, orig_name);
                            if (kernel_struct &&
                                    is_a<ASR::Struct_t>(*kernel_struct)) {
                                struct_name = down_cast<ASR::Struct_t>(
                                    kernel_struct)->m_name;
                                ASR::Struct_t *ks =
                                    down_cast<ASR::Struct_t>(kernel_struct);
                                ASR::symbol_t *kernel_method =
                                    get_struct_member_recursive(ks,
                                        orig_name);
                                if (kernel_method) {
                                    struct_name = struct_member_owner_name(
                                        kernel_method, struct_name);
                                    ASR::asr_t *new_es =
                                        ASR::make_ExternalSymbol_t(al, loc,
                                            kernel_scope,
                                            s2c(al, func_name),
                                            kernel_method,
                                            s2c(al, struct_name),
                                            nullptr, 0,
                                            s2c(al, orig_name),
                                            ASR::accessType::Public);
                                    kernel_scope->add_symbol(func_name,
                                        down_cast<ASR::symbol_t>(new_es));
                                }
                            }
                        }
                    }
                }
                // For type-bound procedure calls, also import the
                // underlying Function body into the kernel scope so
                // the Metal backend can generate shader code.
                // For submodule procedures, the module-scope Function
                // is just an interface (no body); find and import the
                // submodule implementation instead.
                if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                        *resolved)) {
                    ASR::StructMethodDeclaration_t *smd =
                        ASR::down_cast<ASR::StructMethodDeclaration_t>(
                            resolved);
                    ASR::symbol_t *proc_sym =
                        ASRUtils::symbol_get_past_external(smd->m_proc);
                    if (ASR::is_a<ASR::Function_t>(*proc_sym)) {
                        ASR::Function_t *proc_func =
                            ASR::down_cast<ASR::Function_t>(proc_sym);
                        std::string pname =
                            ASRUtils::symbol_name(proc_sym);
                        ASR::symbol_t *existing =
                            kernel_scope->get_symbol(pname);
                        bool already_has_body = false;
                        if (existing &&
                                ASR::is_a<ASR::Function_t>(*existing)) {
                            ASR::FunctionType_t *eft =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    ASR::down_cast<ASR::Function_t>(
                                        existing)
                                        ->m_function_signature);
                            if (eft->m_deftype ==
                                    ASR::deftypeType::Implementation) {
                                already_has_body = true;
                            }
                        }
                        if (!already_has_body) {
                            if (existing) {
                                kernel_scope->erase_symbol(pname);
                            }
                            ASR::FunctionType_t *ftype =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    proc_func->m_function_signature);
                            if (ftype->m_deftype ==
                                    ASR::deftypeType::Interface) {
                                // Submodule interface: find the
                                // Implementation in a submodule
                                // already in the TU, or load it from
                                // disk (--separate-compilation).
                                bool found = false;
                                for (auto &tu_item :
                                        tu.m_symtab->get_scope()) {
                                    if (!ASR::is_a<ASR::Module_t>(
                                            *tu_item.second)) continue;
                                    ASR::Module_t *mod =
                                        ASR::down_cast<ASR::Module_t>(
                                            tu_item.second);
                                    ASR::symbol_t *impl_sym =
                                        mod->m_symtab->get_symbol(pname);
                                    if (!impl_sym ||
                                        !ASR::is_a<ASR::Function_t>(
                                            *impl_sym)) continue;
                                    ASR::Function_t *impl_func =
                                        ASR::down_cast<ASR::Function_t>(
                                            impl_sym);
                                    ASR::FunctionType_t *impl_ft =
                                        ASR::down_cast<ASR::FunctionType_t>(
                                            impl_func
                                                ->m_function_signature);
                                    if (impl_ft->m_deftype !=
                                            ASR::deftypeType::Implementation)
                                        continue;
                                    ASR::symbol_t *dup =
                                        sym_dup.duplicate_Function(
                                            impl_func, kernel_scope);
                                    if (dup) {
                                        kernel_scope->add_symbol(
                                            pname, dup);
                                    }
                                    found = true;
                                    break;
                                }
                                if (!found) {
                                    // Load submodule from smod file.
                                    SymbolTable *parent_st =
                                        ASRUtils::symbol_parent_symtab(
                                            proc_sym);
                                    if (parent_st->asr_owner &&
                                            parent_st->asr_owner->type ==
                                                ASR::asrType::symbol &&
                                            ASR::is_a<ASR::Module_t>(
                                                *ASR::down_cast<
                                                    ASR::symbol_t>(
                                                    parent_st
                                                        ->asr_owner))) {
                                        std::string parent_mod =
                                            ASR::down_cast<ASR::Module_t>(
                                                ASR::down_cast<
                                                    ASR::symbol_t>(
                                                    parent_st
                                                        ->asr_owner))
                                                ->m_name;
                                        std::string smod_prefix =
                                            parent_mod + "@";
                                        std::vector<
                                            std::filesystem::path>
                                                mod_dirs;
                                        mod_dirs.push_back(
                                            pass_options
                                                .runtime_library_dir);
                                        mod_dirs.push_back(
                                            pass_options.mod_files_dir);
                                        mod_dirs.insert(mod_dirs.end(),
                                            pass_options.include_dirs
                                                .begin(),
                                            pass_options.include_dirs
                                                .end());
                                        for (auto &dir : mod_dirs) {
                                            if (dir.empty())
                                                dir = ".";
                                            if (!std::filesystem::
                                                    is_directory(dir))
                                                continue;
                                            for (auto &file :
                                                    std::filesystem::
                                                        directory_iterator(
                                                            dir)) {
                                                std::string fname =
                                                    file.path()
                                                        .filename()
                                                        .string();
                                                if (!startswith(fname,
                                                        smod_prefix) ||
                                                    !endswith(fname,
                                                        ".smod"))
                                                    continue;
                                                std::string content;
                                                if (!read_file(
                                                        file.path()
                                                            .string(),
                                                        content) ||
                                                    content.empty())
                                                    continue;
                                                LocationManager
                                                    lm_tmp;
                                                auto res =
                                                    load_modfile(
                                                        al, content,
                                                        false,
                                                        *tu.m_symtab,
                                                        lm_tmp);
                                                if (!res.ok) continue;
                                                load_submodule_deps(
                                                    *res.result);
                                                fix_external_symbols(
                                                    *res.result,
                                                    *tu.m_symtab);
                                                lower_loaded_implied_do_loops(
                                                    *res.result);
                                                ASR::Module_t *submod =
                                                    ASRUtils::
                                                        extract_module(
                                                            *res.result);
                                                ASR::symbol_t
                                                    *impl_sym =
                                                    submod->m_symtab
                                                        ->get_symbol(
                                                            pname);
                                                if (!impl_sym ||
                                                    !ASR::is_a<
                                                        ASR::Function_t
                                                            >(*impl_sym))
                                                    continue;
                                                ASR::Function_t
                                                    *impl_func =
                                                    ASR::down_cast<
                                                        ASR::Function_t>(
                                                            impl_sym);
                                                ASR::FunctionType_t
                                                    *impl_ft =
                                                    ASR::down_cast<
                                                        ASR::FunctionType_t>(
                                                        impl_func
                                                        ->m_function_signature);
                                                if (impl_ft->m_deftype
                                                        != ASR::
                                                        deftypeType::
                                                        Implementation)
                                                    continue;
                                                ASR::symbol_t *dup =
                                                    sym_dup
                                                        .duplicate_Function(
                                                        impl_func,
                                                        kernel_scope);
                                                if (dup) {
                                                    kernel_scope
                                                        ->add_symbol(
                                                            pname, dup);
                                                }
                                                found = true;
                                                break;
                                            }
                                            if (found) break;
                                        }
                                    }
                                }
                                if (!found) {
                                    throw LCompilersException(
                                        "GPU Metal offload: cannot find "
                                        "submodule implementation for '"
                                        + pname + "'; ensure the "
                                        "submodule is compiled before "
                                        "the file that uses it");
                                }
                            } else {
                                // Non-submodule: function has a body.
                                ASR::symbol_t *dup =
                                    sym_dup.duplicate_Function(
                                        proc_func, kernel_scope);
                                if (dup) {
                                    kernel_scope->add_symbol(pname, dup);
                                }
                            }
                        }
                        // Update the StructMethodDeclaration in the
                        // kernel's struct to point to the kernel-scope
                        // function copy instead of the original module
                        // interface (which may have no body).
                        ASR::symbol_t *kernel_func =
                            kernel_scope->get_symbol(pname);
                        if (kernel_func) {
                            SymbolTable *method_st =
                                ASRUtils::symbol_parent_symtab(resolved);
                            if (method_st->asr_owner &&
                                    method_st->asr_owner->type ==
                                        ASR::asrType::symbol) {
                                ASR::symbol_t *struct_owner =
                                    down_cast<ASR::symbol_t>(
                                        method_st->asr_owner);
                                if (is_a<ASR::Struct_t>(*struct_owner)) {
                                    std::string sname =
                                        down_cast<ASR::Struct_t>(
                                            struct_owner)->m_name;
                                    std::string mname =
                                        ASRUtils::symbol_name(
                                            resolved);
                                    ASR::symbol_t *ks =
                                        find_kernel_struct(kernel_scope,
                                            sname, mname);
                                    if (ks &&
                                            is_a<ASR::Struct_t>(*ks)) {
                                        ASR::symbol_t *km =
                                            down_cast<ASR::Struct_t>(ks)
                                                ->m_symtab
                                                ->get_symbol(mname);
                                        if (km && is_a<
                                            ASR::StructMethodDeclaration_t
                                                >(*km)) {
                                            down_cast<ASR::
                                                StructMethodDeclaration_t
                                                    >(km)->m_proc =
                                                        kernel_func;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Fix up struct references in ALL duplicated kernel functions.
        // After duplication, ExternalSymbol targets and Variable
        // m_type_declarations may still reference the original module's
        // struct definitions. Remap them to the kernel's copies.
        // This recurses into nested scopes (Block, AssociateBlock, etc.).
        {
            for (auto &item : kernel_scope->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *dfunc = ASR::down_cast<ASR::Function_t>(
                    item.second);
                fixup_struct_refs_in_scope(dfunc->m_symtab,
                    kernel_scope, s2c(al, kernel_name));
            }
        }

        // Fix dangling variable references in duplicated kernel functions.
        // When a contained function references variables from the original
        // enclosing scope (e.g., a program-scope Parameter used by a
        // contained function), the duplicated function body retains the
        // original Var references which are unreachable from the kernel.
        // For Parameter variables, clone them into the function's scope.
        // For other variables, add them as extra kernel parameters.
        {
            for (auto &item : kernel_scope->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(
                    item.second);
                DanglingVarCollector dvc(func->m_symtab);
                for (size_t bi = 0; bi < func->n_body; bi++) {
                    dvc.visit_stmt(*func->m_body[bi]);
                }
                if (dvc.dangling.empty()) continue;

                std::set<std::string> fixed_names;
                for (auto &[name, orig_sym] : dvc.dangling) {
                    ASR::symbol_t *resolved_sym =
                        ASRUtils::symbol_get_past_external(orig_sym);
                    if (!ASR::is_a<ASR::Variable_t>(*resolved_sym)) continue;
                    ASR::Variable_t *orig_var =
                        ASR::down_cast<ASR::Variable_t>(resolved_sym);
                    if (orig_var->m_storage ==
                            ASR::storage_typeType::Parameter) {
                        ASR::symbol_t *new_var =
                            ASR::down_cast<ASR::symbol_t>(
                                ASRUtils::make_Variable_t_util(al, loc,
                                    func->m_symtab, s2c(al, name),
                                    nullptr, 0,
                                    ASR::intentType::Local,
                                    orig_var->m_symbolic_value,
                                    orig_var->m_value,
                                    ASR::storage_typeType::Parameter,
                                    ASRUtils::duplicate_type(al,
                                        orig_var->m_type),
                                    nullptr, orig_var->m_abi,
                                    orig_var->m_access,
                                    ASR::presenceType::Required, false));
                        func->m_symtab->add_symbol(name, new_var);
                        fixed_names.insert(name);
                    } else {
                        if (!kernel_scope->get_symbol(name)) {
                            ASR::ttype_t *dup_type =
                                ASRUtils::duplicate_type(al,
                                    ASRUtils::type_get_past_allocatable(
                                        orig_var->m_type));
                            SetChar deps_vec;
                            deps_vec.reserve(al, 1);
                            ASRUtils::collect_variable_dependencies(
                                al, deps_vec, dup_type, nullptr,
                                nullptr, name);
                            ASR::symbol_t *param =
                                ASR::down_cast<ASR::symbol_t>(
                                    ASRUtils::make_Variable_t_util(al,
                                        loc, kernel_scope,
                                        s2c(al, name),
                                        deps_vec.p, deps_vec.size(),
                                        ASR::intentType::InOut,
                                        nullptr, nullptr,
                                        ASR::storage_typeType::Default,
                                        dup_type, nullptr,
                                        ASR::abiType::Source,
                                        ASR::accessType::Public,
                                        ASR::presenceType::Required,
                                        false));
                            kernel_scope->add_symbol(name, param);
                            kernel_args.push_back(al,
                                ASRUtils::EXPR(ASR::make_Var_t(
                                    al, loc, param)));
                            ASR::symbol_t *host_sym =
                                orig_scope->resolve_symbol(name);
                            ASR::call_arg_t carg;
                            carg.loc = loc;
                            carg.m_value = ASRUtils::EXPR(
                                ASR::make_Var_t(al, loc,
                                    host_sym ? host_sym : orig_sym));
                            call_args.push_back(al, carg);
                        }
                        fixed_names.insert(name);
                    }
                }
                if (!fixed_names.empty()) {
                    DanglingVarFixer fixer(func->m_symtab, fixed_names);
                    for (size_t bi = 0; bi < func->n_body; bi++) {
                        fixer.visit_stmt(*func->m_body[bi]);
                    }
                }
            }
        }

        // Remap FunctionCall/SubroutineCall references inside duplicated
        // kernel functions. When function f() calls g() and both are
        // duplicated into the kernel scope, f's body still references the
        // original g from the program scope. Fix those up.
        // Also descend into AssociateBlock and Block bodies within
        // duplicated functions — the statement visitor does not enter
        // these sub-scopes, so FunctionCall m_name references inside
        // them (e.g., type-bound procedure calls in associate blocks)
        // still point to the original scope after duplication.
        {
            for (auto &item : kernel_scope->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(
                    item.second);
                GpuReplaceSymbolsVisitor fn_replacer(*kernel_scope);
                fn_replacer.replacer.skip_scopes.insert(func->m_symtab);
                for (size_t bi = 0; bi < func->n_body; bi++) {
                    fn_replacer.visit_stmt(*func->m_body[bi]);
                }
                for (auto &fn_item : func->m_symtab->get_scope()) {
                    if (ASR::is_a<ASR::AssociateBlock_t>(
                            *fn_item.second)) {
                        ASR::AssociateBlock_t *ab =
                            ASR::down_cast<ASR::AssociateBlock_t>(
                                fn_item.second);
                        for (size_t bi = 0; bi < ab->n_body; bi++) {
                            fn_replacer.visit_stmt(*ab->m_body[bi]);
                        }
                    } else if (ASR::is_a<ASR::Block_t>(
                                   *fn_item.second)) {
                        ASR::Block_t *block =
                            ASR::down_cast<ASR::Block_t>(fn_item.second);
                        for (size_t bi = 0; bi < block->n_body; bi++) {
                            fn_replacer.visit_stmt(*block->m_body[bi]);
                        }
                    }
                }
            }
        }

        // Decompose StructInstanceMember references in kernel variable
        // type expressions (e.g., ArraySize(StructInstanceMember(Var(x),
        // nodes)) in VLA dimensions). When a struct variable is fully
        // decomposed into flat-array parameters, it is removed from the
        // kernel scope, but other variables' VLA dimensions may still
        // reference it through StructInstanceMember. Replace those with
        // the decomposed flat-array parameter Var before general symbol
        // remapping.
        if (!decomp_map.empty()) {
            GpuDecomposeStructReplacer type_decomp(al, kernel_scope,
                decomp_map);
            for (auto &item : kernel_scope->get_scope()) {
                if (!is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = down_cast<ASR::Variable_t>(
                    item.second);
                if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                    var->m_type);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_start) {
                        type_decomp.current_expr =
                            &(arr->m_dims[d].m_start);
                        type_decomp.replace_expr(
                            arr->m_dims[d].m_start);
                    }
                    if (arr->m_dims[d].m_length) {
                        type_decomp.current_expr =
                            &(arr->m_dims[d].m_length);
                        type_decomp.replace_expr(
                            arr->m_dims[d].m_length);
                    }
                }
            }
        }

        // Remap symbol references in kernel parameter types (e.g., array
        // dimension expressions like s(x%n) that still point to the
        // original scope after duplicate_type).
        {
            GpuReplaceSymbols type_replacer(*kernel_scope);
            for (auto &item : kernel_scope->get_scope()) {
                if (!is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = down_cast<ASR::Variable_t>(item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t i = 0; i < arr->n_dims; i++) {
                        if (arr->m_dims[i].m_start) {
                            type_replacer.current_expr = &(arr->m_dims[i].m_start);
                            type_replacer.replace_expr(arr->m_dims[i].m_start);
                        }
                        if (arr->m_dims[i].m_length) {
                            type_replacer.current_expr = &(arr->m_dims[i].m_length);
                            type_replacer.replace_expr(arr->m_dims[i].m_length);
                        }
                    }
                }
            }
        }

        // Save host-side head expressions BEFORE in-place replacement
        struct DimInfo {
            ASR::expr_t *host_start;
            ASR::expr_t *host_end;
        };
        std::vector<DimInfo> dim_info;
        for (size_t d = 0; d < n_dims; d++) {
            dim_info.push_back({x.m_head[d].m_start, x.m_head[d].m_end});
        }

        // Deep-copy the body statements so that in-place symbol remapping
        // does not corrupt types shared with the original function scope
        // (e.g., ArrayBroadcast type sharing the same Array dimension Var
        // nodes as the original variable's type).
        ASRUtils::ExprStmtDuplicator body_dup(al);
        body_dup.success = true;
        Vec<ASR::stmt_t*> body_copy;
        body_copy.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            ASR::stmt_t *copy = body_dup.duplicate_stmt(x.m_body[i]);
            LCOMPILERS_ASSERT(copy);
            body_copy.push_back(al, copy);
        }

        // Turn every `size(a(i)%m, d)` in the copied body into a scalar
        // kernel argument the host computes at launch time.  The extent
        // of an allocatable component reached through a subscript into an
        // array of derived types is otherwise available to neither side:
        // the kernel is handed only that component's flattened data and
        // its per-element total size.  A workspace sized by such an
        // extent would be declined -- or, worse, sized by a guess -- so
        // it is resolved here into a plain integer parameter.  This has
        // to happen before the decomposition below rewrites the component
        // access into a flat-array Var, and while the body still names
        // the host symbols the launch site passes as actuals.
        {
            std::vector<std::string> kernel_arg_names;
            for (size_t i = 0; i < kernel_args.n; i++) {
                kernel_arg_names.push_back(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(kernel_args.p[i])->m_v));
            }
            std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>>
                member_extent_args;
            GpuStructArrayMemberExtentVisitor mev(al, orig_scope,
                kernel_scope, kernel_arg_names, member_extent_args);
            for (size_t i = 0; i < body_copy.n; i++) {
                mev.visit_stmt(*body_copy.p[i]);
            }
            for (auto &pair : member_extent_args) {
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, pair.first)));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = pair.second;
                call_args.push_back(al, carg);
            }
        }

        // Replace StructInstanceMember references to decomposed
        // allocatable members with Var references to the new
        // flat-array kernel parameters, before general symbol remapping.
        if (!decomp_map.empty()) {
            GpuDecomposeStructVisitor decomp_visitor(al, kernel_scope,
                decomp_map);
            for (size_t i = 0; i < body_copy.n; i++) {
                decomp_visitor.visit_stmt(*body_copy.p[i]);
            }
        }

        // 3. Replace Var references in copied body to point to kernel scope
        GpuReplaceSymbolsVisitor sym_replacer(*kernel_scope);
        for (size_t i = 0; i < body_copy.n; i++) {
            sym_replacer.visit_stmt(*body_copy.p[i]);
        }

        // 4. Build kernel body
        Vec<ASR::stmt_t*> kernel_body;
        kernel_body.reserve(al, x.n_body + 2 * n_dims + 1);

        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        ASR::expr_t *thread_idx = ASRUtils::EXPR(
            ASR::make_GpuThreadIndex_t(al, loc, 0, int_type, nullptr));
        ASR::expr_t *block_idx = ASRUtils::EXPR(
            ASR::make_GpuBlockIndex_t(al, loc, 0, int_type, nullptr));
        ASR::expr_t *block_sz = ASRUtils::EXPR(
            ASR::make_GpuBlockSize_t(al, loc, 0, int_type, nullptr));

        // flat_idx = block_idx * block_size + thread_idx
        ASR::expr_t *flat_idx = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    block_idx, ASR::binopType::Mul, block_sz, int_type, nullptr)),
                ASR::binopType::Add, thread_idx, int_type, nullptr));

        // For multi-dimensional: linearize index
        // For do concurrent (i=1:m, j=1:n, k=1:p):
        //   flat = flat_idx
        //   i = flat % m + 1;  flat = flat / m
        //   j = flat % n + 1;  flat = flat / n
        //   k = flat + 1  (last dim)
        //   guard: flat_idx >= m*n*k → return

        // Create kernel-scope versions of start/end for each dimension.
        // Instead of duplicating host expressions (which may contain
        // ArrayBound/ArraySize on allocatable arrays that cannot be
        // correctly evaluated in the kernel scope), pass the loop
        // bounds as pre-computed scalar parameters from the host.
        std::vector<ASR::expr_t*> kernel_starts, kernel_ends;
        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *host_start = dim_info[d].host_start;
            ASR::expr_t *host_end = dim_info[d].host_end;
            bool start_is_const = ASR::is_a<ASR::IntegerConstant_t>(*host_start);
            bool end_is_const = ASR::is_a<ASR::IntegerConstant_t>(*host_end);

            if (start_is_const) {
                kernel_starts.push_back(dup_expr_to_scope(host_start, kernel_scope));
            } else {
                std::string name = "__loop_start_" + std::to_string(d);
                ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::InOut, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                kernel_scope->add_symbol(name, param);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_start;
                call_args.push_back(al, carg);
                kernel_starts.push_back(
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
            }

            if (end_is_const) {
                kernel_ends.push_back(dup_expr_to_scope(host_end, kernel_scope));
            } else {
                std::string name = "__loop_end_" + std::to_string(d);
                ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::InOut, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                kernel_scope->add_symbol(name, param);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_end;
                call_args.push_back(al, carg);
                kernel_ends.push_back(
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
            }
        }

        // Decompose StructInstanceMember references in kernel head
        // expressions. After associate resolution, head bounds may
        // contain e.g. ArraySize(StructInstanceMember(Var(arg), nodes))
        // where arg was decomposed and removed from involved_syms.
        // Replace these with Var(arg__nodes) to match the kernel params.
        if (!decomp_map.empty()) {
            GpuDecomposeStructReplacer head_decomp(al, kernel_scope,
                decomp_map);
            for (size_t d = 0; d < n_dims; d++) {
                if (kernel_starts[d]) {
                    head_decomp.current_expr = &kernel_starts[d];
                    head_decomp.replace_expr(kernel_starts[d]);
                }
                if (kernel_ends[d]) {
                    head_decomp.current_expr = &kernel_ends[d];
                    head_decomp.replace_expr(kernel_ends[d]);
                }
            }
        }

        // Compute total_elements for host-side grid size
        // Also compute per-dim range: range_d = end_d - start_d + 1
        // For kernel: dim_size_d = end_d - start_d + 1
        ASR::expr_t *one_const = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));

        // Compute total flat size for guard
        ASR::expr_t *total_size_kernel = nullptr;
        for (size_t d = 0; d < n_dims; d++) {
            // dim_range = kernel_end - kernel_start + 1
            ASR::expr_t *dim_range = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        kernel_ends[d], ASR::binopType::Sub,
                        kernel_starts[d], int_type, nullptr)),
                    ASR::binopType::Add, one_const, int_type, nullptr));
            if (total_size_kernel == nullptr) {
                total_size_kernel = dim_range;
            } else {
                total_size_kernel = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        total_size_kernel, ASR::binopType::Mul,
                        dim_range, int_type, nullptr));
            }
        }

        // Guard: if (flat_idx >= total_size) return
        ASR::expr_t *guard = ASRUtils::EXPR(
            ASR::make_IntegerCompare_t(al, loc, flat_idx,
                ASR::cmpopType::GtE, total_size_kernel,
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
        Vec<ASR::stmt_t*> guard_body;
        guard_body.reserve(al, 1);
        guard_body.push_back(al, ASRUtils::STMT(ASR::make_Return_t(al, loc)));
        Vec<ASR::stmt_t*> guard_else;
        guard_else.reserve(al, 0);
        kernel_body.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, guard,
                guard_body.p, guard_body.n,
                guard_else.p, guard_else.n)));

        // Compute per-dim loop variable from flat_idx
        // We need a "remaining" variable in kernel scope
        std::string remain_name = "__flat_idx";
        {
            ASR::symbol_t *remain_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, remain_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(remain_name, remain_sym);
        }
        ASR::expr_t *remain_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, kernel_scope->get_symbol(remain_name)));

        // __flat_idx = flat_idx (the raw thread index)
        kernel_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, remain_var, flat_idx, nullptr, false, false)));

        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *dim_range = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        kernel_ends[d], ASR::binopType::Sub,
                        kernel_starts[d], int_type, nullptr)),
                    ASR::binopType::Add, one_const, int_type, nullptr));

            ASR::symbol_t *kvar = kernel_scope->get_symbol(loop_var_names[d]);
            ASR::expr_t *kvar_expr = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, kvar));

            if (d < n_dims - 1) {
                // loop_var = __flat_idx % dim_range + start
                // Since ASR has no Mod binop, compute as: a - (a/b)*b
                ASR::expr_t *div_part = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Div,
                        dim_range, int_type, nullptr));
                ASR::expr_t *mul_part = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        div_part, ASR::binopType::Mul,
                        dim_range, int_type, nullptr));
                ASR::expr_t *mod_val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Sub,
                        mul_part, int_type, nullptr));
                ASR::expr_t *val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        mod_val, ASR::binopType::Add,
                        kernel_starts[d], int_type, nullptr));
                kernel_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, kvar_expr, val, nullptr, false, false)));

                // __flat_idx = __flat_idx / dim_range
                ASR::expr_t *div_val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Div,
                        dim_range, int_type, nullptr));
                kernel_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, remain_var, div_val, nullptr, false, false)));
            } else {
                // Last dim: loop_var = __flat_idx + start
                ASR::expr_t *val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Add,
                        kernel_starts[d], int_type, nullptr));
                kernel_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, kvar_expr, val, nullptr, false, false)));
            }
        }

        // Move Block symbols referenced by BlockCall into kernel scope.
        // This helper processes a block and recursively handles any nested
        // BlockCall statements, since GpuReplaceSymbolsVisitor does not
        // descend into BlockCall/AssociateBlockCall automatically.
        // `reparent` is true only for the top-level block; nested blocks
        // keep their existing parent (the enclosing block's symtab).
        std::function<void(ASR::Block_t*, bool)> process_block_for_kernel =
            [&](ASR::Block_t *block, bool reparent) {
            if (reparent) {
                block->m_symtab->parent = kernel_scope;
            }
            // Pre-compute VLA dimension expressions that contain
            // FunctionCall nodes on the host side and pass the
            // results as scalar kernel parameters, because GPU
            // kernels cannot call arbitrary host-side functions.
            // This must happen BEFORE body remapping: the variable
            // type and body expression types (e.g. ArrayBroadcast
            // m_type) may share the same Array_t pointer, so body
            // remapping would change Var references in the shared
            // dimension to point to kernel-scope symbols. The
            // host_expr duplicate must capture the original
            // (caller-scope) references for the host-side call args.
            // Track old→new expression replacements so that DoLoop
            // bounds created by the ArrayBroadcast lowering (which
            // copied the old dimension pointers) can be updated too.
            std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>>
                dim_replacements;
            {
                ASRUtils::ExprStmtDuplicator dim_dup(al);
                dim_dup.success = true;
                std::set<std::string> loop_index_names;
                for (size_t d = 0; d < x.n_head; d++) {
                    if (!x.m_head[d].m_v) continue;
                    if (!ASR::is_a<ASR::Var_t>(*x.m_head[d].m_v)) continue;
                    loop_index_names.insert(ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(x.m_head[d].m_v)->m_v));
                }
                for (auto &item : block->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second))
                        continue;
                    ASR::Variable_t *bvar =
                        ASR::down_cast<ASR::Variable_t>(item.second);
                    if (!ASR::is_a<ASR::Array_t>(*bvar->m_type))
                        continue;
                    ASR::Array_t *arr =
                        ASR::down_cast<ASR::Array_t>(bvar->m_type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        ASR::expr_t **dim_ptrs[2] = {
                            &arr->m_dims[d].m_start,
                            &arr->m_dims[d].m_length};
                        for (int e = 0; e < 2; e++) {
                            if (!*dim_ptrs[e]) continue;
                            if (!expr_has_function_call(
                                    *dim_ptrs[e]))
                                continue;
                            ASR::expr_t *old_dim_expr = *dim_ptrs[e];
                            // The host evaluates this expression before
                            // it launches, where the loop index has no
                            // value -- so a `size(f(i, ...))` must be
                            // taken from the shape in its own type
                            // rather than by calling `f` on the host.
                            ASR::expr_t *host_expr =
                                gpu_simplify_array_sizes(al,
                                    dim_dup.duplicate_expr(
                                        *dim_ptrs[e]));
                            gpu_check_host_expr_index_free(host_expr,
                                loop_index_names,
                                "the extent of '" +
                                    std::string(bvar->m_name) + "'");
                            std::string pname =
                                kernel_scope->get_unique_name(
                                    "__lfortran_gpu_dim_", false);
                            ASR::ttype_t *ptype =
                                ASRUtils::duplicate_type(al,
                                    ASRUtils::expr_type(
                                        *dim_ptrs[e]));
                            ASR::symbol_t *psym =
                                ASR::down_cast<ASR::symbol_t>(
                                    ASRUtils::make_Variable_t_util(
                                        al, loc, kernel_scope,
                                        s2c(al, pname),
                                        nullptr, 0,
                                        ASR::intentType::InOut,
                                        nullptr, nullptr,
                                        ASR::storage_typeType::Default,
                                        ptype, nullptr,
                                        ASR::abiType::Source,
                                        ASR::accessType::Public,
                                        ASR::presenceType::Required,
                                        false));
                            kernel_scope->add_symbol(pname, psym);
                            kernel_args.push_back(al,
                                ASRUtils::EXPR(ASR::make_Var_t(
                                    al, loc, psym)));
                            ASR::call_arg_t carg;
                            carg.loc = loc;
                            carg.m_value = host_expr;
                            call_args.push_back(al, carg);
                            ASR::expr_t *new_dim_expr = ASRUtils::EXPR(
                                ASR::make_Var_t(al, loc, psym));
                            *dim_ptrs[e] = new_dim_expr;
                            dim_replacements.push_back(
                                {old_dim_expr, new_dim_expr});
                        }
                    }
                }
            }
            // The ArrayBroadcast lowering (inline_elemental_array_var_
            // in_body) may have created DoLoop statements whose bounds
            // copied the old VLA dimension expression pointers before
            // the pre-computation above replaced them. Walk the block
            // body and patch any DoLoop bounds that still reference the
            // old expressions.
            if (!dim_replacements.empty()) {
                std::function<void(ASR::stmt_t**, size_t)>
                    patch_do_loop_bounds = [&](ASR::stmt_t **stmts,
                                               size_t n_stmts) {
                    for (size_t si = 0; si < n_stmts; si++) {
                        if (ASR::is_a<ASR::DoLoop_t>(*stmts[si])) {
                            ASR::DoLoop_t *dl =
                                ASR::down_cast<ASR::DoLoop_t>(
                                    stmts[si]);
                            for (auto &[old_e, new_e] :
                                    dim_replacements) {
                                if (dl->m_head.m_start == old_e)
                                    dl->m_head.m_start = new_e;
                                if (dl->m_head.m_end == old_e)
                                    dl->m_head.m_end = new_e;
                            }
                            patch_do_loop_bounds(dl->m_body,
                                dl->n_body);
                        }
                    }
                };
                patch_do_loop_bounds(block->m_body, block->n_body);
            }
            // Remap Var references inside the block body
            GpuReplaceSymbolsVisitor block_replacer(*kernel_scope);
            for (size_t j = 0; j < block->n_body; j++) {
                block_replacer.visit_stmt(*block->m_body[j]);
            }
            // Also remap Var references inside AssociateBlock bodies
            // within this Block, since the visitor does not descend
            // into AssociateBlockCall targets automatically.
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::AssociateBlock_t>(*item.second))
                    continue;
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        item.second);
                for (size_t j = 0; j < ab->n_body; j++) {
                    block_replacer.visit_stmt(*ab->m_body[j]);
                }
            }
            // Replace StructInstanceMember references to decomposed
            // allocatable members inside the block body.
            if (!decomp_map.empty()) {
                GpuDecomposeStructVisitor block_decomp(al, kernel_scope,
                    decomp_map);
                for (size_t j = 0; j < block->n_body; j++) {
                    block_decomp.visit_stmt(*block->m_body[j]);
                }
                // Also decompose StructInstanceMember references in
                // block-local variable type expressions (e.g., VLA
                // dimensions like size(self%x) after associate
                // resolution). Without this, a fully-decomposed struct
                // removed from involved_syms leaves dangling Var refs.
                GpuDecomposeStructReplacer block_type_decomp(al,
                    kernel_scope, decomp_map);
                for (auto &item : block->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second))
                        continue;
                    ASR::Variable_t *bvar =
                        ASR::down_cast<ASR::Variable_t>(item.second);
                    if (!ASR::is_a<ASR::Array_t>(*bvar->m_type))
                        continue;
                    ASR::Array_t *arr =
                        ASR::down_cast<ASR::Array_t>(bvar->m_type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start) {
                            block_type_decomp.current_expr =
                                &(arr->m_dims[d].m_start);
                            block_type_decomp.replace_expr(
                                arr->m_dims[d].m_start);
                        }
                        if (arr->m_dims[d].m_length) {
                            block_type_decomp.current_expr =
                                &(arr->m_dims[d].m_length);
                            block_type_decomp.replace_expr(
                                arr->m_dims[d].m_length);
                        }
                    }
                }
            }
            // Recursively process nested BlockCall statements
            for (size_t j = 0; j < block->n_body; j++) {
                if (ASR::is_a<ASR::BlockCall_t>(*block->m_body[j])) {
                    ASR::BlockCall_t *inner_bc =
                        ASR::down_cast<ASR::BlockCall_t>(block->m_body[j]);
                    if (ASR::is_a<ASR::Block_t>(*inner_bc->m_m)) {
                        process_block_for_kernel(
                            ASR::down_cast<ASR::Block_t>(inner_bc->m_m),
                            false);
                    }
                }
            }
            // Remap type expressions of block-local variables
            // (e.g., VLA dimensions like n(i) in real :: a(n(i)))
            GpuReplaceSymbols block_type_replacer(*kernel_scope);
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start) {
                            block_type_replacer.current_expr =
                                &(arr->m_dims[d].m_start);
                            block_type_replacer.replace_expr(
                                arr->m_dims[d].m_start);
                        }
                        if (arr->m_dims[d].m_length) {
                            block_type_replacer.current_expr =
                                &(arr->m_dims[d].m_length);
                            block_type_replacer.replace_expr(
                                arr->m_dims[d].m_length);
                        }
                    }
                }
            }
        };
        // Recursively find and move all BlockCall targets from any
        // nesting depth (e.g., BlockCall inside a DoLoop inside the
        // do concurrent body) into the kernel scope.
        std::function<void(ASR::stmt_t**, size_t)>
            move_blocks_to_kernel = [&](ASR::stmt_t **stmts,
                                        size_t n_stmts) {
            for (size_t i = 0; i < n_stmts; i++) {
                if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
                    ASR::BlockCall_t *bc =
                        ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                    if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                        ASR::Block_t *block =
                            ASR::down_cast<ASR::Block_t>(bc->m_m);
                        std::string block_name = block->m_name;
                        process_block_for_kernel(block, true);
                        if (orig_scope->get_symbol(block_name)) {
                            orig_scope->erase_symbol(block_name);
                        }
                        if (!kernel_scope->get_symbol(block_name)) {
                            kernel_scope->add_symbol(block_name, bc->m_m);
                        }
                    }
                } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
                    ASR::DoLoop_t *dl =
                        ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                    move_blocks_to_kernel(dl->m_body, dl->n_body);
                } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                    ASR::If_t *ifs =
                        ASR::down_cast<ASR::If_t>(stmts[i]);
                    move_blocks_to_kernel(ifs->m_body, ifs->n_body);
                    move_blocks_to_kernel(ifs->m_orelse, ifs->n_orelse);
                } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                    ASR::WhileLoop_t *wl =
                        ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                    move_blocks_to_kernel(wl->m_body, wl->n_body);
                }
            }
        };
        move_blocks_to_kernel(body_copy.p, body_copy.n);

        // Add copied loop body (already remapped)
        for (size_t i = 0; i < body_copy.n; i++) {
            kernel_body.push_back(al, body_copy.p[i]);
        }

        // 5. Build function signature
        // FunctionType arg_types must not contain scope-bound expressions,
        // so strip dimension expressions that reference variables.
        Vec<ASR::ttype_t*> arg_types;
        arg_types.reserve(al, kernel_args.n);
        for (size_t i = 0; i < kernel_args.n; i++) {
            ASR::Var_t *v = down_cast<ASR::Var_t>(kernel_args.p[i]);
            ASR::ttype_t *t = ASRUtils::symbol_type(v->m_v);
            if (ASR::is_a<ASR::Array_t>(*t)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
                ASR::dimension_t *new_dims = al.allocate<ASR::dimension_t>(arr->n_dims);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    new_dims[d].loc = arr->m_dims[d].loc;
                    new_dims[d].m_start = nullptr;
                    new_dims[d].m_length = nullptr;
                }
                t = ASRUtils::TYPE(ASR::make_Array_t(al, arr->base.base.loc,
                    arr->m_type, new_dims, arr->n_dims,
                    arr->m_physical_type));
            }
            arg_types.push_back(al, t);
        }
        ASR::ttype_t *fn_sig = ASRUtils::TYPE(
            ASR::make_FunctionType_t(al, loc,
                arg_types.p, arg_types.n, nullptr,
                ASR::abiType::Source, ASR::deftypeType::Implementation,
                nullptr, false, false, false, false, false, nullptr, 0, false));

        // 6. Create GpuKernelFunction
        ASR::asr_t *kernel_func = ASR::make_GpuKernelFunction_t(al, loc,
            kernel_scope, s2c(al, kernel_name), fn_sig,
            nullptr, 0,
            kernel_args.p, kernel_args.n,
            kernel_body.p, kernel_body.n,
            ASR::accessType::Public);
        tu_symtab->add_symbol(kernel_name,
            ASR::down_cast<ASR::symbol_t>(kernel_func));

        // Pre-allocate host-side allocatable arrays that are assigned
        // from a FunctionCall inside the do concurrent body. The GPU
        // kernel receives the buffer pointer at launch time, so the
        // array must already be allocated on the host before dispatch.
        Vec<ASR::stmt_t*> pre_launch_stmts;
        pre_launch_stmts.reserve(al, 4);
        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            // Unwrap BlockCall to inspect block body statements
            ASR::stmt_t **stmts_to_scan = &stmt;
            size_t n_stmts_to_scan = 1;
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *blk =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    stmts_to_scan = blk->m_body;
                    n_stmts_to_scan = blk->n_body;
                }
            }
            for (size_t sj = 0; sj < n_stmts_to_scan; sj++) {
                if (!ASR::is_a<ASR::Assignment_t>(*stmts_to_scan[sj]))
                    continue;
                ASR::Assignment_t *asgn =
                    ASR::down_cast<ASR::Assignment_t>(stmts_to_scan[sj]);
                if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
                if (!ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value))
                    continue;

                ASR::Var_t *target_var =
                    ASR::down_cast<ASR::Var_t>(asgn->m_target);
                ASR::symbol_t *orig_sym =
                    ASRUtils::symbol_get_past_external(target_var->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*orig_sym)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(orig_sym);
                if (!ASRUtils::is_allocatable(var->m_type)) continue;

                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(asgn->m_value);
                ASR::symbol_t *fn_sym =
                    ASRUtils::symbol_get_past_external(fc->m_name);
                if (!ASR::is_a<ASR::Function_t>(*fn_sym)) continue;

                ASR::Function_t *fn =
                    ASR::down_cast<ASR::Function_t>(fn_sym);
                std::string ret_name;
                if (fn->m_return_var &&
                        ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                    ret_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            fn->m_return_var)->m_v);
                }
                if (ret_name.empty()) continue;

                // Find the Allocate statement for the return variable
                // in the function body and use its dimensions.
                bool alloc_found = false;
                for (size_t bi = 0;
                        bi < fn->n_body && !alloc_found; bi++) {
                    if (!ASR::is_a<ASR::Allocate_t>(*fn->m_body[bi]))
                        continue;
                    ASR::Allocate_t *fn_alloc =
                        ASR::down_cast<ASR::Allocate_t>(fn->m_body[bi]);
                    for (size_t ai = 0; ai < fn_alloc->n_args; ai++) {
                        if (!fn_alloc->m_args[ai].m_a ||
                                !ASR::is_a<ASR::Var_t>(
                                    *fn_alloc->m_args[ai].m_a))
                            continue;
                        std::string aname = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                fn_alloc->m_args[ai].m_a)->m_v);
                        if (aname != ret_name) continue;

                        ASRUtils::ExprStmtDuplicator dup(al);
                        dup.success = true;
                        ASR::alloc_arg_t host_arg;
                        host_arg.loc = loc;
                        host_arg.m_a = asgn->m_target;
                        host_arg.n_dims =
                            fn_alloc->m_args[ai].n_dims;
                        host_arg.m_dims =
                            al.allocate<ASR::dimension_t>(
                                host_arg.n_dims);
                        for (size_t d = 0; d < host_arg.n_dims; d++) {
                            host_arg.m_dims[d].loc = loc;
                            host_arg.m_dims[d].m_start =
                                fn_alloc->m_args[ai].m_dims[d].m_start
                                ? dup.duplicate_expr(
                                    fn_alloc->m_args[ai]
                                        .m_dims[d].m_start)
                                : nullptr;
                            host_arg.m_dims[d].m_length =
                                fn_alloc->m_args[ai].m_dims[d].m_length
                                ? dup.duplicate_expr(
                                    fn_alloc->m_args[ai]
                                        .m_dims[d].m_length)
                                : nullptr;
                        }
                        host_arg.m_len_expr = nullptr;
                        host_arg.m_sym_subclass = nullptr;
                        host_arg.m_type = nullptr;
                        host_arg.m_codims = nullptr;
                        host_arg.n_codims = 0;

                        Vec<ASR::alloc_arg_t> alloc_vec;
                        alloc_vec.reserve(al, 1);
                        alloc_vec.push_back(al, host_arg);
                        pre_launch_stmts.push_back(al,
                            ASRUtils::STMT(ASR::make_Allocate_t(
                                al, loc, alloc_vec.p, alloc_vec.n,
                                nullptr, nullptr, nullptr)));
                        alloc_found = true;
                        break;
                    }
                }
            }
        }

        // 7. Replace DoConcurrentLoop with GpuKernelLaunch + GpuSync
        // Collect all launch-related statements into a temporary Vec.
        // If any involved variable is optional, wrap them in a
        // present() guard so the host never reads a null descriptor.
        gather_guard.commit();
        Vec<ASR::stmt_t*> launch_stmts;
        launch_stmts.reserve(al, gather_stmts.n + pre_launch_stmts.n
            + scatter_stmts.n + liveout_scalars.size() + 2
            + liveout_scalars.size());
        for (size_t gi = 0; gi < gather_stmts.n; gi++) {
            launch_stmts.push_back(al, gather_stmts.p[gi]);
        }
        for (size_t pi = 0; pi < pre_launch_stmts.n; pi++) {
            launch_stmts.push_back(al, pre_launch_stmts.p[pi]);
        }

        // Copy liveout scalars into their 1-element array buffers
        // before the kernel launch so the buffer has the initial value
        for (auto &ls : liveout_scalars) {
            ASR::expr_t *buf_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, ls.host_buf_sym));
            ASR::expr_t *scalar_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
            ASR::expr_t *idx_one = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                    ASR::integerbozType::Decimal));
            Vec<ASR::array_index_t> ai_args;
            ai_args.reserve(al, 1);
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = idx_one;
            ai.m_step = nullptr;
            ai_args.push_back(al, ai);
            ASR::expr_t *buf_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, buf_var,
                    ai_args.p, 1, ls.scalar_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            launch_stmts.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, buf_item, scalar_var,
                    nullptr, false, false)));
        }

        ASR::expr_t *block_size_const = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 256, int_type,
                ASR::integerbozType::Decimal));

        // Compute host-side total_elements = product of (end_d - start_d + 1)
        ASR::expr_t *host_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));
        ASR::expr_t *host_total = nullptr;
        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *dim_range = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        dim_info[d].host_end, ASR::binopType::Sub,
                        dim_info[d].host_start, int_type, nullptr)),
                    ASR::binopType::Add, host_one, int_type, nullptr));
            if (host_total == nullptr) {
                host_total = dim_range;
            } else {
                host_total = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        host_total, ASR::binopType::Mul,
                        dim_range, int_type, nullptr));
            }
        }

        // grid_size = (total + 255) / 256
        ASR::expr_t *grid_padded = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc, host_total, ASR::binopType::Add,
                ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 255, int_type,
                    ASR::integerbozType::Decimal)),
                int_type, nullptr));
        ASR::expr_t *grid_size = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc, grid_padded, ASR::binopType::Div,
                block_size_const, int_type, nullptr));

        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_GpuKernelLaunch_t(al, loc,
                ASR::down_cast<ASR::symbol_t>(kernel_func),
                grid_size, block_size_const,
                call_args.p, call_args.n)));

        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_GpuSync_t(al, loc)));

        // Put every gathered element the kernel wrote into back over the
        // original, before anything on the host can read it again.
        for (size_t si = 0; si < scatter_stmts.n; si++) {
            launch_stmts.push_back(al, scatter_stmts.p[si]);
        }

        // Copy liveout scalar results back from the 1-element array
        // buffers after the kernel has completed
        for (auto &ls : liveout_scalars) {
            ASR::expr_t *buf_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, ls.host_buf_sym));
            ASR::expr_t *scalar_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
            ASR::expr_t *idx_one = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                    ASR::integerbozType::Decimal));
            Vec<ASR::array_index_t> ai_args;
            ai_args.reserve(al, 1);
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = idx_one;
            ai.m_step = nullptr;
            ai_args.push_back(al, ai);
            ASR::expr_t *buf_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, buf_var,
                    ai_args.p, 1, ls.scalar_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            launch_stmts.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, scalar_var, buf_item,
                    nullptr, false, false)));
        }

        // If any involved variable is optional, wrap the whole kernel
        // launch block in if(present(v1) .and. present(v2) ...) so
        // the host never tries to read a null descriptor or compute
        // ArraySize on an absent argument.
        if (!optional_syms.empty()) {
            ASR::ttype_t *log_type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4));
            ASR::expr_t *guard = nullptr;
            for (ASR::symbol_t *opt_sym : optional_syms) {
                Vec<ASR::expr_t*> present_args;
                present_args.reserve(al, 1);
                present_args.push_back(al, ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, opt_sym)));
                ASR::expr_t *present_call = ASRUtils::EXPR(
                    ASR::make_IntrinsicElementalFunction_t(al, loc,
                        static_cast<int64_t>(
                            ASRUtils::IntrinsicElementalFunctions::Present),
                        present_args.p, present_args.n, 0,
                        log_type, nullptr));
                if (guard == nullptr) {
                    guard = present_call;
                } else {
                    guard = ASRUtils::EXPR(
                        ASR::make_LogicalBinOp_t(al, loc, guard,
                            ASR::logicalbinopType::And, present_call,
                            log_type, nullptr));
                }
            }
            Vec<ASR::stmt_t*> empty_else;
            empty_else.reserve(al, 0);
            pass_result.reserve(al, 1);
            pass_result.push_back(al, ASRUtils::STMT(
                ASR::make_If_t(al, loc, nullptr, guard,
                    launch_stmts.p, launch_stmts.n,
                    empty_else.p, empty_else.n)));
        } else {
            pass_result.reserve(al, launch_stmts.n);
            for (size_t i = 0; i < launch_stmts.n; i++) {
                pass_result.push_back(al, launch_stmts.p[i]);
            }
        }
    }
};

void pass_replace_gpu_offload(Allocator &al, ASR::TranslationUnit_t &unit,
                              const LCompilers::PassOptions& pass_options) {
    if (!pass_options.gpu_offload_metal && !pass_options.gpu_offload_cuda) return;
    GpuOffloadVisitor v(al, pass_options, unit);
    v.asr_changed = true;
    while (v.asr_changed) {
        v.asr_changed = false;
        v.collect_host_only_loops();
        v.visit_TranslationUnit(unit);
    }
    // Kernel extraction moves Block symbols out of their enclosing
    // function, which can leave stale entries in that function's
    // dependency list. Recompute all dependencies to fix this.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
