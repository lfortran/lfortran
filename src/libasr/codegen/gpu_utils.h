#ifndef LFORTRAN_GPU_UTILS_H
#define LFORTRAN_GPU_UTILS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

#include <string>
#include <vector>

namespace LCompilers {

// Describes one dimension of a VLA workspace buffer.
struct GpuVlaDim {
    bool is_constant;
    int64_t constant_value;
    size_t call_arg_index;
    ASR::expr_t *dim_expr; // original ASR dimension expression
    // When true, size is read from a struct member's allocatable
    // array size, resolved at dispatch time from the struct array's
    // per-element sizes. struct_member_key is "arr_name.member_name".
    bool is_struct_member_size = false;
    std::string struct_member_key;
    // When non-empty, the size is the scalar component chain
    // arg%member_path[0]%member_path[1]%... of the kernel argument at
    // `call_arg_index`. A struct is handed to the kernel as a buffer, so
    // the host reads the component out of it to size the workspace.
    std::vector<std::string> member_path;
    // When true, the size is the whole of `dim_expr`, which is arithmetic
    // over the kernel's parameters and nothing else. The launch rebuilds
    // it over the actual arguments rather than collapsing it onto one of
    // them -- `op%m_ + 1` is not `op%m_`.
    bool is_host_expr = false;
};

// Describes a VLA workspace buffer required by a GPU kernel.
struct GpuVlaWorkspace {
    std::string var_name;
    int buffer_index;
    int elem_size;
    std::vector<GpuVlaDim> dims;
};

// How the host runtime's argument slot holds one kernel parameter.
enum class GpuKernelParamKind {
    Buffer,           // slot points at the buffer pointer
    StructReference,  // slot points at the pointer to the struct
    ScalarStruct,     // slot points at the packed scalar struct itself
};

// One parameter of a generated device kernel, in the order the host runtime
// binds it. A dialect that has to unpack the runtime's argument array again --
// the CPU emulation of CUDA does -- reads the list back.
struct GpuKernelParam {
    std::string type;   // element type, or the struct name for a reference
    std::string name;
    GpuKernelParamKind kind;
};

// An allocatable array component of a kernel argument that is an array of a
// derived type reaches the device as three buffers: the elements' data laid
// out end to end, the per-element offsets into it, and the per-element sizes.
// The sizes buffer holds the *extents* of the component, one entry per
// dimension per element, in dimension order: element `k` of a rank `R`
// component occupies entries `k*R .. k*R + R - 1`. A rank-one component
// therefore keeps the single entry per element it has always had, that
// element's number of elements, and the buffer accounting below is unchanged;
// for a higher rank the element count is the product of the R entries.
// Carrying the extents rather than only the total is what lets the shader
// linearize `a(i)%m(p,q)`, which needs the extent of every dimension but the
// last. The host fills the buffer in device_launch_expand and the device
// reads it in asr_to_gpu_c.h; this is the one place the two agree.
inline size_t gpu_struct_member_rank(const ASR::Variable_t *var) {
    ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*inner)) return 0;
    return ASR::down_cast<ASR::Array_t>(inner)->n_dims;
}

// Classify kernel arguments into buffer (array/struct) and scalar categories.
// Returns the count of buffer args and scalar args respectively.
// For struct array args with allocatable array members, counts 3 extra
// buffers per member (data, offsets, sizes) as emitted by Metal codegen.
inline std::pair<int, int> classify_gpu_kernel_args(
        const ASR::Function_t &kernel) {
    int n_buffer = 0, n_scalar = 0;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        ASR::ttype_t *type = var->m_type;
        if (ASRUtils::is_array(type) ||
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(type))) {
            n_buffer++;
            if (ASRUtils::is_array(type) && var->m_type_declaration) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    var->m_type_declaration);
                if (ASR::is_a<ASR::Struct_t>(*s)) {
                    // Members inherited from the types this one extends are
                    // decomposed like its own, so they count here too.
                    n_buffer += 3 *
                        (int)ASRUtils::collect_allocatable_array_members(
                            ASR::down_cast<ASR::Struct_t>(s)).size();
                }
            }
        } else {
            n_scalar++;
        }
    }
    return {n_buffer, n_scalar};
}

// Helper to recursively find the first Allocate statement for a given
// variable name within a statement list.
inline ASR::Allocate_t* find_allocate_for_var(
        ASR::stmt_t **stmts, size_t n, const std::string &var_name) {
    for (size_t i = 0; i < n; i++) {
        if (ASR::is_a<ASR::Allocate_t>(*stmts[i])) {
            ASR::Allocate_t *alloc =
                ASR::down_cast<ASR::Allocate_t>(stmts[i]);
            for (size_t ai = 0; ai < alloc->n_args; ai++) {
                if (!alloc->m_args[ai].m_a) continue;
                if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a))
                    continue;
                std::string aname = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        alloc->m_args[ai].m_a)->m_v);
                if (aname == var_name) return alloc;
            }
        }
        if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
            ASR::WhileLoop_t *wl =
                ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
            auto *r = find_allocate_for_var(wl->m_body, wl->n_body,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
            ASR::DoLoop_t *dl =
                ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
            auto *r = find_allocate_for_var(dl->m_body, dl->n_body,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
            ASR::If_t *if_s =
                ASR::down_cast<ASR::If_t>(stmts[i]);
            auto *r = find_allocate_for_var(if_s->m_body, if_s->n_body,
                var_name);
            if (r) return r;
            r = find_allocate_for_var(if_s->m_orelse, if_s->n_orelse,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmts[i])->m_m);
            if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            auto *r = find_allocate_for_var(blk->m_body, blk->n_body,
                var_name);
            if (r) return r;
        }
    }
    return nullptr;
}

// Try to evaluate an ASR integer expression as a compile-time constant.
inline bool try_eval_int_constant(ASR::expr_t *e, int64_t &val) {
    if (!e) return false;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        val = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (v && v != e) return try_eval_int_constant(v, val);
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        return try_eval_int_constant(
            ASR::down_cast<ASR::Cast_t>(e)->m_arg, val);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        int64_t l, r;
        auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        if (!try_eval_int_constant(op->m_left, l)) return false;
        if (!try_eval_int_constant(op->m_right, r)) return false;
        switch (op->m_op) {
            case ASR::binopType::Add: val = l + r; return true;
            case ASR::binopType::Sub: val = l - r; return true;
            case ASR::binopType::Mul: val = l * r; return true;
            case ASR::binopType::Div:
                if (r != 0) { val = l / r; return true; }
                return false;
            default: return false;
        }
    }
    return false;
}

// Forward declaration for mutual recursion with
// try_resolve_array_size_via_associate.
inline bool try_resolve_alloc_dim_constant(
        ASR::expr_t *dim,
        ASR::stmt_t **body, size_t n_body,
        int64_t &result);

// Try to resolve ArraySize(ptr_var, dim) to a constant by tracing an
// Associate statement back to an ArraySection with constant bounds.
// If no Associate is found, traces through Allocate statements to
// resolve transitively (e.g. ArraySize(temp_merge) → temp_merge is
// allocated with ArraySize(temp_compare) → temp_compare is associated
// with a constant-bounds ArraySection).
inline bool try_resolve_array_size_via_associate(
        ASR::ArraySize_t *as,
        ASR::stmt_t **body, size_t n_body,
        int64_t &result) {
    if (!as->m_v || !ASR::is_a<ASR::Var_t>(*as->m_v)) return false;
    std::string var_name = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(as->m_v)->m_v);
    int64_t target_dim = 1;
    if (as->m_dim) {
        if (!try_eval_int_constant(as->m_dim, target_dim)) return false;
    }
    for (size_t i = 0; i < n_body; i++) {
        if (!ASR::is_a<ASR::Associate_t>(*body[i])) continue;
        ASR::Associate_t *assoc =
            ASR::down_cast<ASR::Associate_t>(body[i]);
        if (!ASR::is_a<ASR::Var_t>(*assoc->m_target)) continue;
        std::string tname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(assoc->m_target)->m_v);
        if (tname != var_name) continue;
        if (!ASR::is_a<ASR::ArraySection_t>(*assoc->m_value)) return false;
        ASR::ArraySection_t *sec =
            ASR::down_cast<ASR::ArraySection_t>(assoc->m_value);
        int range_dim = 0;
        for (size_t d = 0; d < sec->n_args; d++) {
            ASR::array_index_t &idx = sec->m_args[d];
            if (idx.m_left == nullptr) continue;
            range_dim++;
            if (range_dim == target_dim) {
                int64_t start_val, end_val, stride_val = 1;
                if (!try_eval_int_constant(idx.m_left, start_val))
                    return false;
                if (!try_eval_int_constant(idx.m_right, end_val))
                    return false;
                if (idx.m_step &&
                        !try_eval_int_constant(idx.m_step, stride_val))
                    return false;
                if (stride_val == 0) return false;
                result = (end_val - start_val) / stride_val + 1;
                if (result < 0) result = 0;
                return true;
            }
        }
        return false;
    }
    // No Associate found — try tracing through an Allocate statement
    // for the same variable.  If its allocation dimension for
    // `target_dim` is itself resolvable to a constant, use that.
    ASR::Allocate_t *alloc = find_allocate_for_var(
        body, n_body, var_name);
    if (alloc) {
        for (size_t ai = 0; ai < alloc->n_args; ai++) {
            if (!alloc->m_args[ai].m_a) continue;
            if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
            std::string aname = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(
                    alloc->m_args[ai].m_a)->m_v);
            if (aname != var_name) continue;
            ASR::alloc_arg_t &targ = alloc->m_args[ai];
            if (target_dim < 1 ||
                    (size_t)target_dim > targ.n_dims)
                return false;
            ASR::expr_t *dim_expr =
                targ.m_dims[target_dim - 1].m_length;
            if (!dim_expr) return false;
            return try_resolve_alloc_dim_constant(
                dim_expr, body, n_body, result);
        }
    }
    return false;
}

// Try to resolve an Allocate dimension to a compile-time constant,
// including tracing ArraySize through Associate statements.
inline bool try_resolve_alloc_dim_constant(
        ASR::expr_t *dim,
        ASR::stmt_t **body, size_t n_body,
        int64_t &result) {
    if (try_eval_int_constant(dim, result)) return true;
    if (ASR::is_a<ASR::ArraySize_t>(*dim)) {
        return try_resolve_array_size_via_associate(
            ASR::down_cast<ASR::ArraySize_t>(dim), body, n_body, result);
    }
    return false;
}

// The kernel's scalar parameter carrying one extent of an array parameter.
// The offload pass creates the parameter under this name and the resolvers
// below look it up again by it, so both spell it here.
inline std::string gpu_dim_arg_name(const std::string &name, size_t d) {
    return "__dim_" + name + "_" + std::to_string(d);
}

// The array and the 0-based dimension whose extent `e` is, when `e` is one
// dimension of an array designator however it is spelled: `size(a, d)`,
// `size(a)` on a rank-1 array, or the `ubound(a,d) - lbound(a,d) + 1` that
// a whole dimension of an assumed-shape array is lowered to.
inline bool gpu_extent_of_array_dim(ASR::expr_t *e, std::string &name,
        size_t &dim) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    auto array_name = [](ASR::expr_t *a, std::string &out) {
        ASR::expr_t *b = ASRUtils::get_past_array_physical_cast(a);
        if (!b || !ASR::is_a<ASR::Var_t>(*b)) return false;
        out = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(b)->m_v);
        return true;
    };
    if (ASR::is_a<ASR::ArraySize_t>(*v)) {
        ASR::ArraySize_t *sz = ASR::down_cast<ASR::ArraySize_t>(v);
        if (!array_name(sz->m_v, name)) return false;
        if (sz->m_dim == nullptr) {
            // Only unambiguous on a rank-1 array, where the total size is
            // the one dimension's extent.
            ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(sz->m_v));
            if (!t || !ASR::is_a<ASR::Array_t>(*t)) return false;
            if (ASR::down_cast<ASR::Array_t>(t)->n_dims != 1) return false;
            dim = 0;
            return true;
        }
        int64_t d;
        if (!try_eval_int_constant(sz->m_dim, d) || d < 1) return false;
        dim = (size_t)d - 1;
        return true;
    }
    // (ubound(a,d) - lbound(a,d)) + 1
    if (!ASR::is_a<ASR::IntegerBinOp_t>(*v)) return false;
    ASR::IntegerBinOp_t *add = ASR::down_cast<ASR::IntegerBinOp_t>(v);
    int64_t one;
    if (add->m_op != ASR::binopType::Add) return false;
    if (!try_eval_int_constant(add->m_right, one) || one != 1) return false;
    ASR::expr_t *l = ASRUtils::get_past_array_physical_cast(add->m_left);
    if (!ASR::is_a<ASR::IntegerBinOp_t>(*l)) return false;
    ASR::IntegerBinOp_t *sub = ASR::down_cast<ASR::IntegerBinOp_t>(l);
    if (sub->m_op != ASR::binopType::Sub) return false;
    if (!ASR::is_a<ASR::ArrayBound_t>(*sub->m_left)) return false;
    if (!ASR::is_a<ASR::ArrayBound_t>(*sub->m_right)) return false;
    ASR::ArrayBound_t *ub = ASR::down_cast<ASR::ArrayBound_t>(sub->m_left);
    ASR::ArrayBound_t *lb = ASR::down_cast<ASR::ArrayBound_t>(sub->m_right);
    if (ub->m_bound != ASR::arrayboundType::UBound) return false;
    if (lb->m_bound != ASR::arrayboundType::LBound) return false;
    std::string lname;
    if (!array_name(ub->m_v, name) || !array_name(lb->m_v, lname)) {
        return false;
    }
    if (name != lname) return false;
    int64_t ud, ld;
    if (!try_eval_int_constant(ub->m_dim, ud)) return false;
    if (!try_eval_int_constant(lb->m_dim, ld)) return false;
    if (ud != ld || ud < 1) return false;
    dim = (size_t)ud - 1;
    return true;
}

// The kernel argument and component chain an extent reads, when the extent
// is a scalar component of a derived-type kernel argument -- `s%m_`, or
// `s%in_%k_`. The kernel is handed `s` as a buffer, so the extent is not a
// scalar parameter it could be read from; the host reads the component
// itself before it launches.
inline bool resolve_extent_to_arg_member(ASR::expr_t *e,
        const std::vector<std::string> &arg_names, size_t &arg_index,
        std::vector<std::string> &member_path) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*v)) return false;
    std::vector<std::string> reversed;
    while (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
        ASR::StructInstanceMember_t *sm =
            ASR::down_cast<ASR::StructInstanceMember_t>(v);
        reversed.push_back(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(sm->m_m)));
        v = ASRUtils::get_past_array_physical_cast(sm->m_v);
    }
    // Only a chain rooted at a whole kernel argument: a subscript in the
    // middle names an element the host cannot pick, because the index only
    // exists on the device.
    if (!ASR::is_a<ASR::Var_t>(*v)) return false;
    std::string root = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(v)->m_v);
    for (size_t a = 0; a < arg_names.size(); a++) {
        if (arg_names[a] != root) continue;
        member_path.assign(reversed.rbegin(), reversed.rend());
        arg_index = a;
        return true;
    }
    return false;
}

// The extent expression of one dimension of a local array of `symtab`,
// taken from the array's own type or from the `allocate` that gives it a
// shape. An extent written over another local -- `size(t) + 1` -- is
// resolved by asking this and carrying on through the answer.
inline ASR::alloc_arg_t* find_alloc_arg_for_var(ASR::Allocate_t *alloc,
        const std::string &var_name);

inline ASR::expr_t* gpu_local_array_extent(SymbolTable *symtab,
        ASR::stmt_t **body, size_t n_body, const std::string &name,
        size_t dim) {
    if (symtab == nullptr) return nullptr;
    ASR::symbol_t *sym = symtab->resolve_symbol(name);
    if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
    if (ASRUtils::is_allocatable(var->m_type)) {
        ASR::Allocate_t *alloc = find_allocate_for_var(body, n_body, name);
        if (alloc == nullptr) return nullptr;
        ASR::alloc_arg_t *arg = find_alloc_arg_for_var(alloc, name);
        if (arg == nullptr || dim >= arg->n_dims) return nullptr;
        return arg->m_dims[dim].m_length;
    }
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*t)) return nullptr;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    if (dim >= arr->n_dims) return nullptr;
    return arr->m_dims[dim].m_length;
}

// Whether the host can read the designator `e`: a kernel parameter, or a
// component or element reached from one, with every subscript something the
// host can work out too. `self%points_(1,1,1,1)%values_` is such a
// designator; `a(i)` for a loop index `i` is not, the index existing only
// once the kernel runs.
inline bool gpu_designator_is_host_readable(ASR::expr_t *e,
        const std::vector<std::string> &arg_names,
        SymbolTable *symtab = nullptr, ASR::stmt_t **body = nullptr,
        size_t n_body = 0);

// Whether the host could evaluate `e` itself, given that it can read every
// kernel parameter. Arithmetic over integer literals, scalar parameters,
// scalar components of parameters and the extents of array parameters is
// evaluable; anything else -- a call, a subscript that only exists on the
// device -- is not.
inline bool gpu_extent_is_host_evaluable(ASR::expr_t *e,
        const std::vector<std::string> &arg_names,
        SymbolTable *symtab = nullptr, ASR::stmt_t **body = nullptr,
        size_t n_body = 0, int depth = 0);

// The kernel argument supplying that extent. An array parameter's extents
// are handed to the kernel as scalar parameters of their own, so an extent
// written over the array itself is read on the host from the same
// parameter the device reads it from.
inline bool resolve_extent_to_dim_arg(ASR::expr_t *e,
        const std::vector<std::string> &arg_names, size_t &arg_index) {
    std::string name;
    size_t dim = 0;
    if (!gpu_extent_of_array_dim(e, name, dim)) return false;
    std::string want = gpu_dim_arg_name(name, dim);
    for (size_t a = 0; a < arg_names.size(); a++) {
        if (arg_names[a] == want) {
            arg_index = a;
            return true;
        }
    }
    return false;
}

inline bool gpu_extent_is_host_evaluable(ASR::expr_t *e,
        const std::vector<std::string> &arg_names,
        SymbolTable *symtab, ASR::stmt_t **body, size_t n_body,
        int depth) {
    if (e == nullptr) return false;
    if (depth > 8) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    if (ASR::is_a<ASR::Cast_t>(*v)) {
        return gpu_extent_is_host_evaluable(
            ASR::down_cast<ASR::Cast_t>(v)->m_arg, arg_names, symtab,
            body, n_body, depth);
    }
    if (ASR::is_a<ASR::IntegerConstant_t>(*v)) return true;
    size_t idx = 0;
    std::vector<std::string> path;
    if (resolve_extent_to_dim_arg(v, arg_names, idx)) return true;
    if (resolve_extent_to_arg_member(v, arg_names, idx, path)) return true;
    if (ASR::is_a<ASR::IntegerBinOp_t>(*v)) {
        ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(v);
        switch (op->m_op) {
            case ASR::binopType::Add:
            case ASR::binopType::Sub:
            case ASR::binopType::Mul:
            case ASR::binopType::Div:
                break;
            default: return false;
        }
        return gpu_extent_is_host_evaluable(op->m_left, arg_names, symtab,
                body, n_body, depth)
            && gpu_extent_is_host_evaluable(op->m_right, arg_names, symtab,
                body, n_body, depth);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*v)) {
        return gpu_extent_is_host_evaluable(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(v)->m_arg, arg_names,
            symtab, body, n_body, depth);
    }
    // An element of an array parameter, at a subscript the host can work
    // out too. The loop index is not a parameter, so an element the
    // iteration picks is correctly not evaluable here.
    if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(
            item->m_v);
        if (!ASR::is_a<ASR::Var_t>(*base)) return false;
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(base)->m_v);
        bool is_arg = false;
        for (const std::string &a : arg_names) {
            if (a == name) { is_arg = true; break; }
        }
        if (!is_arg) return false;
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left || item->m_args[i].m_step) {
                return false;
            }
            if (!gpu_extent_is_host_evaluable(item->m_args[i].m_right,
                    arg_names, symtab, body, n_body, depth)) {
                return false;
            }
        }
        return true;
    }
    // `size(<designator>, d)` where the host can read the designator.
    if (ASR::is_a<ASR::ArraySize_t>(*v)) {
        ASR::ArraySize_t *sz = ASR::down_cast<ASR::ArraySize_t>(v);
        if (gpu_designator_is_host_readable(sz->m_v, arg_names, symtab,
                body, n_body)) {
            return sz->m_dim == nullptr
                || gpu_extent_is_host_evaluable(sz->m_dim, arg_names,
                    symtab, body, n_body, depth);
        }
    }
    if (ASR::is_a<ASR::ArrayBound_t>(*v)) {
        ASR::ArrayBound_t *bd = ASR::down_cast<ASR::ArrayBound_t>(v);
        if (gpu_designator_is_host_readable(bd->m_v, arg_names, symtab,
                body, n_body)) {
            return gpu_extent_is_host_evaluable(bd->m_dim, arg_names,
                symtab, body, n_body, depth);
        }
    }
    std::string local_name;
    size_t local_dim = 0;
    if (gpu_extent_of_array_dim(v, local_name, local_dim)) {
        bool is_arg = false;
        for (const std::string &a : arg_names) {
            if (a == local_name) { is_arg = true; break; }
        }
        if (!is_arg) {
            return gpu_extent_is_host_evaluable(
                gpu_local_array_extent(symtab, body, n_body, local_name,
                    local_dim),
                arg_names, symtab, body, n_body, depth + 1);
        }
    }
    if (ASR::is_a<ASR::Var_t>(*v)) {
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        for (const std::string &a : arg_names) {
            if (a == name) return true;
        }
    }
    return false;
}

inline bool gpu_designator_is_host_readable(ASR::expr_t *e,
        const std::vector<std::string> &arg_names,
        SymbolTable *symtab, ASR::stmt_t **body, size_t n_body) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    if (ASR::is_a<ASR::Var_t>(*v)) {
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        for (const std::string &a : arg_names) {
            if (a == name) return true;
        }
        return false;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
        return gpu_designator_is_host_readable(
            ASR::down_cast<ASR::StructInstanceMember_t>(v)->m_v, arg_names,
            symtab, body, n_body);
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left || item->m_args[i].m_step) return false;
            if (!gpu_extent_is_host_evaluable(item->m_args[i].m_right,
                    arg_names, symtab, body, n_body, 0)) {
                return false;
            }
        }
        return gpu_designator_is_host_readable(item->m_v, arg_names, symtab,
            body, n_body);
    }
    return false;
}

// Helper to extract a kernel argument reference from a complex Allocate
// dimension expression.
inline bool find_arg_var_in_expr(ASR::expr_t *expr,
        const std::vector<std::string> &arg_names,
        size_t &arg_index) {
    if (!expr) return false;
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        for (size_t a = 0; a < arg_names.size(); a++) {
            if (arg_names[a] == name) {
                arg_index = a;
                return true;
            }
        }
        return false;
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        ASR::IntegerBinOp_t *op =
            ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        if (find_arg_var_in_expr(op->m_left, arg_names, arg_index))
            return true;
        return find_arg_var_in_expr(op->m_right, arg_names, arg_index);
    }
    if (ASR::is_a<ASR::IfExp_t>(*expr)) {
        ASR::IfExp_t *ie = ASR::down_cast<ASR::IfExp_t>(expr);
        if (find_arg_var_in_expr(ie->m_body, arg_names, arg_index))
            return true;
        return find_arg_var_in_expr(ie->m_orelse, arg_names, arg_index);
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
        ASR::IntegerCompare_t *cmp =
            ASR::down_cast<ASR::IntegerCompare_t>(expr);
        if (find_arg_var_in_expr(cmp->m_left, arg_names, arg_index))
            return true;
        return find_arg_var_in_expr(cmp->m_right, arg_names, arg_index);
    }
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return find_arg_var_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg,
            arg_names, arg_index);
    }
    return false;
}

// Try to resolve an ArraySize expression through Associate statements
// to find a kernel argument that determines the dimension size.
// Handles the pattern: ArraySize(temp, dim) where temp is associated
// with ArraySection(array_arg, [start:end:step, ...]).
// When start == 1 and step == 1, the section size equals end, and we
// look for a kernel arg reference in end.
inline bool try_resolve_array_size_to_arg_var(
        ASR::expr_t *dim_expr,
        ASR::stmt_t **body, size_t n_body,
        const std::vector<std::string> &arg_names,
        size_t &arg_index) {
    if (!ASR::is_a<ASR::ArraySize_t>(*dim_expr)) return false;
    ASR::ArraySize_t *as = ASR::down_cast<ASR::ArraySize_t>(dim_expr);
    if (!as->m_v || !ASR::is_a<ASR::Var_t>(*as->m_v)) return false;

    std::string var_name = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(as->m_v)->m_v);
    int64_t target_dim = 1;
    if (as->m_dim) {
        if (!try_eval_int_constant(as->m_dim, target_dim)) return false;
    }

    for (size_t i = 0; i < n_body; i++) {
        if (!ASR::is_a<ASR::Associate_t>(*body[i])) continue;
        ASR::Associate_t *assoc =
            ASR::down_cast<ASR::Associate_t>(body[i]);
        if (!ASR::is_a<ASR::Var_t>(*assoc->m_target)) continue;
        std::string tname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(assoc->m_target)->m_v);
        if (tname != var_name) continue;
        if (!ASR::is_a<ASR::ArraySection_t>(*assoc->m_value))
            return false;
        ASR::ArraySection_t *sec =
            ASR::down_cast<ASR::ArraySection_t>(assoc->m_value);
        int range_dim = 0;
        for (size_t d = 0; d < sec->n_args; d++) {
            ASR::array_index_t &idx = sec->m_args[d];
            if (idx.m_left == nullptr) continue;
            range_dim++;
            if (range_dim == target_dim) {
                int64_t start_val = 0;
                bool start_is_one =
                    try_eval_int_constant(idx.m_left, start_val)
                    && start_val == 1;
                bool step_is_one = true;
                if (idx.m_step) {
                    int64_t sv;
                    step_is_one =
                        try_eval_int_constant(idx.m_step, sv) && sv == 1;
                }
                if (start_is_one && step_is_one && idx.m_right) {
                    return find_arg_var_in_expr(
                        idx.m_right, arg_names, arg_index);
                }
                // When the section spans a full dimension (e.g.
                // m(lbound(m,1):ubound(m,1):1, ...)), the size
                // equals the array's dimension size.  Look for the
                // __dim_<base>_<d> kernel arg directly.
                if (step_is_one && ASR::is_a<ASR::Var_t>(*sec->m_v)) {
                    std::string base_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(sec->m_v)->m_v);
                    std::string dim_arg = gpu_dim_arg_name(base_name, d);
                    for (size_t a = 0; a < arg_names.size(); a++) {
                        if (arg_names[a] == dim_arg) {
                            arg_index = a;
                            return true;
                        }
                    }
                }
                return false;
            }
        }
        return false;
    }

    // No Associate found — try tracing through an Allocate whose
    // dimension is itself an ArraySize that can be resolved.
    ASR::Allocate_t *alloc = find_allocate_for_var(
        body, n_body, var_name);
    if (alloc) {
        for (size_t ai = 0; ai < alloc->n_args; ai++) {
            if (!alloc->m_args[ai].m_a) continue;
            if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a))
                continue;
            std::string aname = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(
                    alloc->m_args[ai].m_a)->m_v);
            if (aname != var_name) continue;
            ASR::alloc_arg_t &targ = alloc->m_args[ai];
            if (target_dim < 1 || (size_t)target_dim > targ.n_dims)
                return false;
            ASR::expr_t *inner_dim =
                targ.m_dims[target_dim - 1].m_length;
            if (!inner_dim) return false;
            return try_resolve_array_size_to_arg_var(
                inner_dim, body, n_body, arg_names, arg_index);
        }
    }
    return false;
}

// Find the first struct array kernel arg that has an allocatable array
// member.  Returns "arr_name.member_name" or "" if none found.
inline std::string find_struct_alloc_member_key(
        const ASR::Function_t &kernel) {
    for (size_t ai = 0; ai < kernel.n_args; ai++) {
        ASR::Var_t *av = ASR::down_cast<ASR::Var_t>(kernel.m_args[ai]);
        ASR::Variable_t *avar = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(av->m_v));
        ASR::ttype_t *atype =
            ASRUtils::type_get_past_allocatable(avar->m_type);
        if (!ASR::is_a<ASR::Array_t>(*atype)) continue;
        ASR::Array_t *arr_t = ASR::down_cast<ASR::Array_t>(atype);
        if (!ASR::is_a<ASR::StructType_t>(*arr_t->m_type)) continue;
        if (!avar->m_type_declaration) continue;
        ASR::symbol_t *decl_sym = ASRUtils::symbol_get_past_external(
            avar->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*decl_sym)) continue;
        ASR::Struct_t *stype = ASR::down_cast<ASR::Struct_t>(decl_sym);
        for (auto &mem : stype->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*mem.second)) continue;
            ASR::Variable_t *mv =
                ASR::down_cast<ASR::Variable_t>(mem.second);
            if (!ASRUtils::is_allocatable(mv->m_type)) continue;
            ASR::ttype_t *mt =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            if (!ASR::is_a<ASR::Array_t>(*mt)) continue;
            return std::string(avar->m_name)
                + "." + std::string(mv->m_name);
        }
    }
    return "";
}

// The "struct_array.member" key of an extent that reads the size of a
// deferred-shape component, such as `size(a(i)%v)`. The host cannot evaluate
// such an extent itself, because the element index only exists on the device;
// it sizes the workspace from the component's own sizes instead.
inline bool dim_expr_struct_member_key(ASR::expr_t *dim, std::string &key) {
    if (dim == nullptr) return false;
    ASR::expr_t *e = ASRUtils::get_past_array_physical_cast(dim);
    while (ASR::is_a<ASR::Cast_t>(*e)) {
        e = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
    }
    if (!ASR::is_a<ASR::ArraySize_t>(*e)) return false;
    ASR::expr_t *arg = ASRUtils::get_past_array_physical_cast(
        ASR::down_cast<ASR::ArraySize_t>(e)->m_v);
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*arg)) return false;
    ASR::StructInstanceMember_t *sm =
        ASR::down_cast<ASR::StructInstanceMember_t>(arg);
    std::string member = ASRUtils::symbol_name(
        ASRUtils::symbol_get_past_external(sm->m_m));
    ASR::expr_t *base = sm->m_v;
    if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
        base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
    } else if (ASR::is_a<ASR::ArraySection_t>(*base)) {
        base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*base)) return false;
    key = std::string(ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(base)->m_v)) + "." + member;
    return true;
}

// The size in bytes of an array's element.
inline int gpu_vla_elem_size(ASR::Array_t *arr) {
    if (ASR::is_a<ASR::Real_t>(*arr->m_type)) {
        return ASR::down_cast<ASR::Real_t>(arr->m_type)->m_kind;
    }
    if (ASR::is_a<ASR::Integer_t>(*arr->m_type)) {
        return ASR::down_cast<ASR::Integer_t>(arr->m_type)->m_kind;
    }
    return 4;
}

// The argument of an Allocate statement that gives `var_name` its shape.
inline ASR::alloc_arg_t* find_alloc_arg_for_var(ASR::Allocate_t *alloc,
        const std::string &var_name) {
    for (size_t ai = 0; ai < alloc->n_args; ai++) {
        if (!alloc->m_args[ai].m_a) continue;
        if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
        std::string aname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(alloc->m_args[ai].m_a)->m_v);
        if (aname == var_name) return &alloc->m_args[ai];
    }
    return nullptr;
}

// Describes, as a per-thread workspace, the shape an `allocate` gives an
// array. Returns false when every extent is known at compile time, in which
// case the array needs no workspace: the device code declares it in thread
// memory instead.
inline bool alloc_shape_to_vla_workspace(ASR::alloc_arg_t &alloc_arg,
        ASR::Array_t *arr, const std::string &var_name,
        ASR::stmt_t **body, size_t n_body,
        const std::vector<std::string> &arg_names,
        GpuVlaWorkspace &ws, SymbolTable *symtab = nullptr) {
    bool has_runtime_dim = false;
    for (size_t d = 0; d < alloc_arg.n_dims; d++) {
        if (alloc_arg.m_dims[d].m_length &&
                !ASR::is_a<ASR::IntegerConstant_t>(
                    *alloc_arg.m_dims[d].m_length)) {
            has_runtime_dim = true;
            break;
        }
    }
    if (!has_runtime_dim) return false;
    ws.var_name = var_name;
    ws.elem_size = gpu_vla_elem_size(arr);
    for (size_t d = 0; d < alloc_arg.n_dims; d++) {
        ASR::expr_t *dim = alloc_arg.m_dims[d].m_length;
        GpuVlaDim vd;
        vd.dim_expr = dim;
        vd.is_constant = true;
        vd.constant_value = 1;
        vd.call_arg_index = 0;
        if (dim && ASR::is_a<ASR::IntegerConstant_t>(*dim)) {
            vd.constant_value =
                ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
        } else if (dim) {
            int64_t const_val;
            if (try_resolve_alloc_dim_constant(dim, body, n_body,
                    const_val)) {
                vd.constant_value = const_val;
            } else {
                vd.is_constant = false;
                vd.constant_value = 0;
                size_t idx = 0;
                std::string member_key;
                if (resolve_extent_to_dim_arg(dim, arg_names, idx)) {
                    vd.call_arg_index = idx;
                } else if (resolve_extent_to_arg_member(dim, arg_names, idx,
                        vd.member_path)) {
                    vd.call_arg_index = idx;
                } else if (gpu_extent_is_host_evaluable(dim, arg_names,
                        symtab, body, n_body)) {
                    vd.is_host_expr = true;
                } else if (find_arg_var_in_expr(dim, arg_names, idx)) {
                    vd.call_arg_index = idx;
                } else if (try_resolve_array_size_to_arg_var(dim, body,
                        n_body, arg_names, idx)) {
                    vd.call_arg_index = idx;
                } else if (dim_expr_struct_member_key(dim, member_key)) {
                    vd.is_struct_member_size = true;
                    vd.struct_member_key = member_key;
                } else {
                    // The host cannot size a workspace it cannot measure.
                    // Leave the array to the device language, which either
                    // declares it or reports that it cannot.
                    return false;
                }
            }
        }
        ws.dims.push_back(vd);
    }
    return true;
}

// Describes, as a per-thread workspace, the shape an array is declared with.
// Returns false when every extent is known at compile time, in which case the
// array needs no workspace: the device code declares it in thread memory
// instead.
inline bool declared_shape_to_vla_workspace(ASR::Array_t *arr,
        const std::string &var_name,
        const std::vector<std::string> &arg_names,
        GpuVlaWorkspace &ws, SymbolTable *symtab = nullptr,
        ASR::stmt_t **body = nullptr, size_t n_body = 0) {
    bool has_runtime_dim = false;
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_length &&
                !ASR::is_a<ASR::IntegerConstant_t>(
                    *arr->m_dims[d].m_length)) {
            has_runtime_dim = true;
            break;
        }
    }
    if (!has_runtime_dim) return false;
    ws.var_name = var_name;
    ws.elem_size = gpu_vla_elem_size(arr);
    for (size_t d = 0; d < arr->n_dims; d++) {
        ASR::expr_t *dim = arr->m_dims[d].m_length;
        GpuVlaDim vd;
        vd.dim_expr = dim;
        vd.is_constant = true;
        vd.constant_value = 1;
        vd.call_arg_index = 0;
        if (dim && ASR::is_a<ASR::IntegerConstant_t>(*dim)) {
            vd.constant_value =
                ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
        } else if (dim) {
            vd.is_constant = false;
            vd.constant_value = 0;
            size_t idx = 0;
            std::string member_key;
            if (resolve_extent_to_dim_arg(dim, arg_names, idx)) {
                vd.call_arg_index = idx;
            } else if (resolve_extent_to_arg_member(dim, arg_names, idx,
                    vd.member_path)) {
                vd.call_arg_index = idx;
            } else if (gpu_extent_is_host_evaluable(dim, arg_names,
                    symtab, body, n_body)) {
                vd.is_host_expr = true;
            } else if (find_arg_var_in_expr(dim, arg_names, idx)) {
                vd.call_arg_index = idx;
            } else if (dim_expr_struct_member_key(dim, member_key)) {
                vd.is_struct_member_size = true;
                vd.struct_member_key = member_key;
            } else {
                // The host cannot size a workspace it cannot measure. Leave
                // the array to the device language, which either declares it
                // or reports that it cannot.
                return false;
            }
        }
        ws.dims.push_back(vd);
    }
    return true;
}

// A per-thread workspace for an allocatable array whose size is only known
// on the device, sized from the struct member the array is copied from.
inline bool struct_member_vla_workspace(const ASR::Function_t &kernel,
        ASR::Array_t *arr, const std::string &var_name,
        GpuVlaWorkspace &ws) {
    std::string struct_key = find_struct_alloc_member_key(kernel);
    if (struct_key.empty()) return false;
    ws.var_name = var_name;
    ws.elem_size = gpu_vla_elem_size(arr);
    GpuVlaDim vd;
    vd.dim_expr = nullptr;
    vd.is_constant = false;
    vd.constant_value = 0;
    vd.call_arg_index = 0;
    vd.is_struct_member_size = true;
    vd.struct_member_key = struct_key;
    ws.dims.push_back(vd);
    return true;
}

// Scan kernel-scope Allocatable(Array) variables for VLA workspaces.
inline void scan_kernel_scope_alloc_vlas(
        const ASR::Function_t &kernel,
        const std::vector<std::string> &arg_names,
        int &buffer_idx,
        std::vector<GpuVlaWorkspace> &result) {
    std::set<std::string> arg_set(arg_names.begin(), arg_names.end());
    std::set<std::string> handled_names;
    for (auto &ws : result) handled_names.insert(ws.var_name);

    for (auto &item : kernel.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable(var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);
        std::string vname(var->m_name);
        if (arg_set.count(vname)) continue;
        if (handled_names.count(vname)) continue;
        GpuVlaWorkspace ws;
        bool have = false;
        if (!ASRUtils::is_allocatable(var->m_type)) {
            // An array declared with extents the device cannot evaluate
            // when it enters the kernel.
            have = declared_shape_to_vla_workspace(arr, vname, arg_names,
                ws, kernel.m_symtab, kernel.m_body, kernel.n_body);
            if (!have) continue;
            ws.buffer_index = buffer_idx++;
            result.push_back(std::move(ws));
            continue;
        }
        ASR::Allocate_t *alloc = find_allocate_for_var(
            kernel.m_body, kernel.n_body, vname);
        if (alloc) {
            ASR::alloc_arg_t *target_arg = find_alloc_arg_for_var(
                alloc, vname);
            if (!target_arg) continue;
            have = alloc_shape_to_vla_workspace(*target_arg, arr, vname,
                kernel.m_body, kernel.n_body, arg_names, ws,
                kernel.m_symtab);
        } else {
            // No Allocate: this is a function-call result temporary whose
            // size depends on a struct member's allocatable array.
            have = struct_member_vla_workspace(kernel, arr, vname, ws);
        }
        if (!have) continue;
        ws.buffer_index = buffer_idx++;
        result.push_back(std::move(ws));
    }
}

// Every per-thread workspace a GPU kernel needs, with buffer indices assigned
// sequentially from `buffer_idx`.
inline std::vector<GpuVlaWorkspace> collect_gpu_vla_workspaces(
        const ASR::Function_t &kernel, int buffer_idx) {
    // The kernel argument names, so that a workspace extent that is one of
    // them can be read on the host at dispatch time.
    std::vector<std::string> arg_names;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        arg_names.push_back(std::string(var->m_name));
    }

    std::vector<GpuVlaWorkspace> result;

    // Every BLOCK the kernel body opens, at whatever depth: a block inside
    // a serial loop of the kernel is entered by each thread just as a
    // top-level one is, so its locals need the same per-thread home.
    std::vector<ASR::Block_t*> blocks;
    std::function<void(ASR::stmt_t**, size_t)> collect_blocks =
        [&](ASR::stmt_t **stmts, size_t n_stmts) {
        for (size_t i = 0; i < n_stmts; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(bc->m_m);
                blocks.push_back(b);
                collect_blocks(b->m_body, b->n_body);
            } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                collect_blocks(dl->m_body, dl->n_body);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                ASR::WhileLoop_t *wl =
                    ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                collect_blocks(wl->m_body, wl->n_body);
            } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmts[i]);
                collect_blocks(ifs->m_body, ifs->n_body);
                collect_blocks(ifs->m_orelse, ifs->n_orelse);
            }
        }
    };
    collect_blocks(kernel.m_body, kernel.n_body);

    for (ASR::Block_t *block : blocks) {

        // An automatic array of the block, whose extents the device cannot
        // declare because they are not compile-time constants.
        for (auto &item : block->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                item.second);
            if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);

            GpuVlaWorkspace ws;
            if (!declared_shape_to_vla_workspace(arr, var->m_name,
                    arg_names, ws, block->m_symtab, block->m_body,
                    block->n_body)) {
                continue;
            }
            ws.buffer_index = buffer_idx++;
            result.push_back(std::move(ws));
        }

        // An allocatable array of the block: its shape comes from the
        // `allocate` that gives it one, or, for a temporary the
        // subroutine_from_function pass created, from the struct member it
        // is copied from.
        for (auto &item2 : block->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item2.second)) continue;
            ASR::Variable_t *var2 = ASR::down_cast<ASR::Variable_t>(
                item2.second);
            if (!ASRUtils::is_allocatable(var2->m_type)) continue;
            ASR::ttype_t *inner =
                ASRUtils::type_get_past_allocatable(var2->m_type);
            if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
            ASR::Array_t *arr2 = ASR::down_cast<ASR::Array_t>(inner);
            std::string vname(var2->m_name);
            bool already = false;
            for (auto &r : result) {
                if (r.var_name == vname) { already = true; break; }
            }
            if (already) continue;
            GpuVlaWorkspace ws;
            bool have = false;
            ASR::Allocate_t *alloc = find_allocate_for_var(
                block->m_body, block->n_body, vname);
            if (alloc) {
                ASR::alloc_arg_t *target_arg = find_alloc_arg_for_var(
                    alloc, vname);
                if (target_arg) {
                    have = alloc_shape_to_vla_workspace(*target_arg, arr2,
                        vname, block->m_body, block->n_body, arg_names, ws,
                        block->m_symtab);
                }
            }
            if (!have) {
                have = struct_member_vla_workspace(kernel, arr2, vname, ws);
            }
            if (!have) continue;
            ws.buffer_index = buffer_idx++;
            result.push_back(std::move(ws));
        }
    }

    scan_kernel_scope_alloc_vlas(kernel, arg_names, buffer_idx, result);

    return result;
}

// Count VLA workspaces in a kernel without assigning buffer indices.
inline int count_gpu_vla_workspaces(const ASR::Function_t &kernel) {
    return static_cast<int>(collect_gpu_vla_workspaces(kernel, 0).size());
}

static const int MAX_METAL_BUFFERS = 31;
static const int PACKED_BUFFER_ALIGN = 16;

// Determine whether a kernel needs buffer packing because its total
// buffer count exceeds Metal's 31-slot limit.
inline bool gpu_kernel_needs_buffer_packing(
        const ASR::Function_t &kernel) {
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    int n_vla = count_gpu_vla_workspaces(kernel);
    int total = n_buffer + (n_scalar > 0 ? 1 : 0) + n_vla;
    return total > MAX_METAL_BUFFERS;
}

// Compute the Metal buffer index where VLA workspace buffers start.
// Normal layout:  [buffer_args...] [scalar_struct?] [vla_workspaces...]
// Packed layout:  [packed_arrays(0)] [scalar_struct(1)] [vla_workspaces...]
inline int gpu_vla_buffer_start(const ASR::Function_t &kernel) {
    if (gpu_kernel_needs_buffer_packing(kernel)) {
        return 2;
    }
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    return n_buffer + (n_scalar > 0 ? 1 : 0);
}

// Analyze a GPU kernel function for the per-thread workspaces it needs, with
// buffer indices assigned sequentially after the kernel's packed arguments.
inline std::vector<GpuVlaWorkspace> analyze_gpu_vla_workspaces(
        const ASR::Function_t &kernel) {
    return collect_gpu_vla_workspaces(kernel, gpu_vla_buffer_start(kernel));
}

// Scan a kernel body for alloc-assign statements that write a VLA workspace
// array to a struct array member.  Returns a map from
// "struct_name.member_name" to the per-element size (number of elements)
// determined by the VLA workspace dimensions.
// The Allocate and ReAlloc statements a routine applies to a component of one
// of its variables, keyed by "variable.component".
class StructMemberShapeCollector:
    public ASR::BaseWalkVisitor<StructMemberShapeCollector> {
    public:

        std::map<std::string, ASR::alloc_arg_t*> shapes;

        void collect(ASR::alloc_arg_t *args, size_t n_args) {
            for (size_t i = 0; i < n_args; i++) {
                if (!args[i].m_a || args[i].n_dims == 0) continue;
                if (!ASR::is_a<ASR::StructInstanceMember_t>(*args[i].m_a)) {
                    continue;
                }
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(args[i].m_a);
                ASR::expr_t *base = sm->m_v;
                if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
                    base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
                } else if (ASR::is_a<ASR::ArraySection_t>(*base)) {
                    base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
                }
                if (!ASR::is_a<ASR::Var_t>(*base)) continue;
                std::string key =
                    std::string(ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(base)->m_v))
                    + "." + std::string(ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(sm->m_m)));
                shapes.emplace(key, &args[i]);
            }
        }

        void visit_Allocate(const ASR::Allocate_t &x) {
            collect(x.m_args, x.n_args);
        }

        void visit_ReAlloc(const ASR::ReAlloc_t &x) {
            collect(x.m_args, x.n_args);
        }

        void visit_Function(const ASR::Function_t &/*x*/) {
            // A nested routine shapes its own variables.
        }
};

inline std::map<std::string, ASR::alloc_arg_t*> struct_member_shapes(
        ASR::stmt_t **body, size_t n_body) {
    StructMemberShapeCollector collector;
    for (size_t i = 0; i < n_body; i++) {
        collector.visit_stmt(*body[i]);
    }
    return collector.shapes;
}

// Binds the variables of a routine to the arguments a call passes it, so that
// an extent the routine writes can be read at the call site.
struct GpuExtentContext {
    ASR::Function_t *callee = nullptr;
    ASR::call_arg_t *args = nullptr;
    size_t n_args = 0;
    const std::map<std::string, const GpuVlaWorkspace*> *workspaces = nullptr;

    // The argument a variable of the callee is bound to, if any.
    ASR::expr_t* bound_arg(ASR::symbol_t *sym) const {
        if (callee == nullptr) return nullptr;
        std::string name = ASRUtils::symbol_name(sym);
        for (size_t i = 0; i < callee->n_args && i < n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*callee->m_args[i])) continue;
            if (std::string(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v))
                        != name) {
                continue;
            }
            return args[i].m_value;
        }
        return nullptr;
    }
};

inline bool gpu_extent_value(ASR::expr_t *e, const GpuExtentContext &ctx,
    int64_t &out);

// The number of elements an array expression has along `dim` (all dimensions
// when `dim` is zero), read from the type it is declared with or from the
// workspace that backs it.
inline bool gpu_array_extent(ASR::expr_t *array, int64_t dim,
        const GpuExtentContext &ctx, int64_t &out) {
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(array);
    if (ASR::is_a<ASR::Var_t>(*v)) {
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
        ASR::expr_t *bound = ctx.bound_arg(sym);
        if (bound != nullptr) {
            GpuExtentContext caller_ctx;
            caller_ctx.workspaces = ctx.workspaces;
            return gpu_array_extent(bound, dim, caller_ctx, out);
        }
        if (ctx.workspaces != nullptr) {
            auto ws = ctx.workspaces->find(
                std::string(ASRUtils::symbol_name(sym)));
            if (ws != ctx.workspaces->end()) {
                int64_t total = 1;
                for (size_t d = 0; d < ws->second->dims.size(); d++) {
                    if (!ws->second->dims[d].is_constant) return false;
                    if (dim == 0 || dim == (int64_t) d + 1) {
                        total *= ws->second->dims[d].constant_value;
                    }
                }
                out = total;
                return true;
            }
        }
    }
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(v), dims);
    if (n_dims == 0) return false;
    int64_t total = 1;
    for (size_t d = 0; d < n_dims; d++) {
        if (dim != 0 && dim != (int64_t) d + 1) continue;
        if (dims[d].m_length == nullptr) return false;
        int64_t length;
        if (!gpu_extent_value(dims[d].m_length, ctx, length)) return false;
        total *= length;
    }
    out = total;
    return true;
}

// The value of an extent expression, with the variables of a routine bound to
// the arguments a call passes it.
inline bool gpu_extent_value(ASR::expr_t *e, const GpuExtentContext &ctx,
        int64_t &out) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    if (ASR::is_a<ASR::IntegerConstant_t>(*v)) {
        out = ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n;
        return true;
    }
    if (ASR::is_a<ASR::ArraySize_t>(*v)) {
        ASR::ArraySize_t *size = ASR::down_cast<ASR::ArraySize_t>(v);
        int64_t dim = 0;
        if (size->m_dim && !gpu_extent_value(size->m_dim, ctx, dim)) {
            return false;
        }
        return gpu_array_extent(size->m_v, dim, ctx, out);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*v)) {
        ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(v);
        int64_t left, right;
        if (!gpu_extent_value(op->m_left, ctx, left) ||
            !gpu_extent_value(op->m_right, ctx, right)) {
            return false;
        }
        switch (op->m_op) {
            case ASR::binopType::Add: out = left + right; return true;
            case ASR::binopType::Sub: out = left - right; return true;
            case ASR::binopType::Mul: out = left * right; return true;
            default: return false;
        }
    }
    if (ASR::is_a<ASR::Var_t>(*v)) {
        ASR::expr_t *bound = ctx.bound_arg(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        if (bound != nullptr) {
            GpuExtentContext caller_ctx;
            caller_ctx.workspaces = ctx.workspaces;
            return gpu_extent_value(bound, caller_ctx, out);
        }
    }
    int64_t value;
    if (try_eval_int_constant(v, value)) {
        out = value;
        return true;
    }
    return false;
}

// The "struct_array.component" an extent reads its size from, when the size
// is only known once the host has measured that component.
inline bool gpu_extent_member_key(ASR::expr_t *e, const GpuExtentContext &ctx,
        std::string &key) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    if (ASR::is_a<ASR::Var_t>(*v)) {
        // An extent the caller passes in as an argument of its own.
        ASR::expr_t *bound = ctx.bound_arg(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        if (bound == nullptr) return false;
        GpuExtentContext caller_ctx;
        caller_ctx.workspaces = ctx.workspaces;
        return gpu_extent_member_key(bound, caller_ctx, key);
    }
    if (!ASR::is_a<ASR::ArraySize_t>(*v)) return false;
    ASR::expr_t *array = ASRUtils::get_past_array_physical_cast(
        ASR::down_cast<ASR::ArraySize_t>(v)->m_v);
    if (ASR::is_a<ASR::Var_t>(*array)) {
        ASR::expr_t *bound = ctx.bound_arg(
            ASR::down_cast<ASR::Var_t>(array)->m_v);
        if (bound == nullptr) return false;
        array = ASRUtils::get_past_array_physical_cast(bound);
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*array)) return false;
    ASR::StructInstanceMember_t *sm =
        ASR::down_cast<ASR::StructInstanceMember_t>(array);
    ASR::expr_t *base = sm->m_v;
    if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
        base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
    } else if (ASR::is_a<ASR::ArraySection_t>(*base)) {
        base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*base)) return false;
    key = std::string(ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(base)->m_v))
        + "." + std::string(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(sm->m_m)));
    return true;
}

// Every routine a kernel calls with an element of one of its struct arrays,
// paired with the shapes that routine gives the components of that element.
// Reported as "struct_array.component" keys of the kernel's own arrays.
class KernelStructMemberShapes:
    public ASR::BaseWalkVisitor<KernelStructMemberShapes> {
    public:

        // key -> (shape, the context that reads the shape's extents)
        std::map<std::string, std::pair<ASR::alloc_arg_t*, GpuExtentContext>>
            shapes;
        const std::map<std::string, const GpuVlaWorkspace*> *workspaces;

        KernelStructMemberShapes(
            const std::map<std::string, const GpuVlaWorkspace*> *workspaces_):
            workspaces(workspaces_) {}

        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_name);
            if (!ASR::is_a<ASR::Function_t>(*sym)) return;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            std::map<std::string, ASR::alloc_arg_t*> callee_shapes =
                struct_member_shapes(fn->m_body, fn->n_body);
            if (callee_shapes.empty()) return;
            for (size_t i = 0; i < x.n_args && i < fn->n_args; i++) {
                if (!x.m_args[i].m_value) continue;
                ASR::expr_t *actual = ASRUtils::get_past_array_physical_cast(
                    x.m_args[i].m_value);
                if (!ASR::is_a<ASR::ArrayItem_t>(*actual)) continue;
                ASR::ArrayItem_t *item =
                    ASR::down_cast<ASR::ArrayItem_t>(actual);
                if (!ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(item->m_type))) {
                    continue;
                }
                if (!ASR::is_a<ASR::Var_t>(*item->m_v)) continue;
                std::string array_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(item->m_v)->m_v);
                if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) continue;
                std::string formal_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
                for (auto &shape: callee_shapes) {
                    size_t dot = shape.first.find('.');
                    if (shape.first.substr(0, dot) != formal_name) continue;
                    GpuExtentContext ctx;
                    ctx.callee = fn;
                    ctx.args = x.m_args;
                    ctx.n_args = x.n_args;
                    ctx.workspaces = workspaces;
                    shapes.emplace(array_name + shape.first.substr(dot),
                        std::make_pair(shape.second, ctx));
                }
            }
        }

        void visit_Function(const ASR::Function_t &/*x*/) {
            // Only the kernel's own calls reach the kernel's arrays.
        }
};

// The shape of every component of a kernel's struct arrays that the kernel
// writes without the caller having allocated it, gathered from the Allocate
// and ReAlloc statements that give the component its extents.
inline std::map<std::string,
        std::pair<ASR::alloc_arg_t*, GpuExtentContext>>
    kernel_struct_member_shapes(const ASR::Function_t &kernel,
        const std::map<std::string, const GpuVlaWorkspace*> &ws_by_name) {
    KernelStructMemberShapes visitor(&ws_by_name);
    for (size_t i = 0; i < kernel.n_body; i++) {
        visitor.visit_stmt(*kernel.m_body[i]);
    }
    // A component the kernel shapes itself, rather than through a call.
    GpuExtentContext ctx;
    ctx.workspaces = &ws_by_name;
    for (auto &shape: struct_member_shapes(kernel.m_body, kernel.n_body)) {
        visitor.shapes.emplace(shape.first,
            std::make_pair(shape.second, ctx));
    }
    return visitor.shapes;
}

// The number of elements a kernel writes into each component of its struct
// arrays that the caller left unallocated, where that number is known before
// the kernel is dispatched.
inline std::map<std::string, int64_t> find_struct_member_vla_write_sizes(
        const ASR::Function_t &kernel,
        const std::vector<GpuVlaWorkspace> &vla_workspaces) {
    std::map<std::string, const GpuVlaWorkspace*> ws_by_name;
    for (auto &ws : vla_workspaces) {
        ws_by_name[ws.var_name] = &ws;
    }
    std::map<std::string, int64_t> result;
    for (auto &shape: kernel_struct_member_shapes(kernel, ws_by_name)) {
        int64_t total = 1;
        bool known = true;
        for (size_t d = 0; d < shape.second.first->n_dims; d++) {
            int64_t length;
            if (!gpu_extent_value(shape.second.first->m_dims[d].m_length,
                    shape.second.second, length)) {
                known = false;
                break;
            }
            total *= length;
        }
        if (known && total > 0) {
            result[shape.first] = total;
        }
    }
    return result;
}

// The components whose size a kernel only learns from another component of a
// struct array, as a map from the written "struct.component" to the one it is
// sized from.
inline std::map<std::string, std::string>
    find_struct_member_vla_runtime_sources(const ASR::Function_t &kernel) {
    std::map<std::string, const GpuVlaWorkspace*> ws_by_name;
    std::map<std::string, std::string> result;
    for (auto &shape: kernel_struct_member_shapes(kernel, ws_by_name)) {
        if (shape.second.first->n_dims != 1) continue;
        std::string source;
        if (gpu_extent_member_key(shape.second.first->m_dims[0].m_length,
                shape.second.second, source)) {
            result[shape.first] = source;
        }
    }
    return result;
}

} // namespace LCompilers

#endif // LFORTRAN_GPU_UTILS_H
