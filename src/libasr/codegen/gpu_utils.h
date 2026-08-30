#ifndef LFORTRAN_GPU_UTILS_H
#define LFORTRAN_GPU_UTILS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_walk_visitor.h>

#include <set>
#include <string>
#include <vector>

namespace LCompilers {

// One node of a host-evaluable expression tree describing the extent of
// one VLA workspace dimension.  The tree lets the host reproduce the whole
// arithmetic of the dimension expression (e.g. `s%m_ + 1`, `2*n`) instead
// of collapsing it to a single kernel argument, which would silently
// compute a wrong extent.  Children are indices into
// `GpuVlaDim::expr_nodes`, so the tree stays copyable.
struct GpuVlaDimNode {
    enum class Kind { Constant, CallArg, StructMember, BinOp };
    Kind kind = Kind::Constant;
    int64_t constant_value = 0;
    // Kernel argument supplying the value (CallArg and StructMember).
    int64_t call_arg_index = -1;
    // Component chain arg%member_path[0]%member_path[1]%... (StructMember).
    std::vector<std::string> member_path;
    ASR::binopType binop = ASR::binopType::Add;
    int64_t left = -1, right = -1;
};

// Describes one dimension of a VLA workspace buffer.
struct GpuVlaDim {
    bool is_constant = false;
    int64_t constant_value = 0;
    // Index of the kernel argument that supplies this dimension's size.
    // -1 means the size could not be mapped to a kernel argument; the
    // host side must then report an error instead of silently reading
    // argument 0.
    int64_t call_arg_index = -1;
    // When non-empty, the size is the derived-type component chain
    // arg%member_path[0]%member_path[1]%... of kernel argument
    // `call_arg_index`.  A struct is passed to the kernel as a buffer,
    // so the host has to load the component from the struct at
    // kernel-launch time to size the workspace.
    std::vector<std::string> member_path;
    ASR::expr_t *dim_expr = nullptr; // original ASR dimension expression
    // Host-evaluable expression tree for this extent.  When non-empty it
    // takes precedence over `call_arg_index`/`member_path`; `expr_root`
    // indexes the root node in `expr_nodes`.
    std::vector<GpuVlaDimNode> expr_nodes;
    int64_t expr_root = -1;
    // When true, size is read from a struct member's allocatable
    // array size, resolved at dispatch time from the struct array's
    // per-element sizes. struct_member_key is "arr_name.member_name".
    bool is_struct_member_size = false;
    std::string struct_member_key;
};

// Describes a VLA workspace buffer required by a GPU kernel.
struct GpuVlaWorkspace {
    std::string var_name;
    int buffer_index;
    int elem_size;
    std::vector<GpuVlaDim> dims;
};

// A kernel array argument is reached through an array descriptor when it is
// allocatable/pointer or assumed-shape (at least one extent unknown at
// compile time).  Such an argument carries a per-dimension stride that is
// only known at run time: the actual argument may be a non-contiguous
// section (for example `a(3,:)`), in which case consecutive Fortran
// elements are NOT consecutive in the flat device buffer.  Both the host
// (which fills the scalar-argument struct) and the Metal code generator
// (which emits the struct and the index arithmetic) use this predicate so
// that they agree on the scalar-argument layout.
inline bool gpu_arg_is_descriptor_array(const ASR::Variable_t *var) {
    std::string name(var->m_name);
    // Synthetic kernel args (__data_*, __size_*, ...) are already flat.
    if (name.size() >= 2 && name[0] == '_' && name[1] == '_') return false;
    ASR::ttype_t *type = var->m_type;
    if (!ASRUtils::is_array(type)) return false;
    ASR::ttype_t *past = ASRUtils::type_get_past_allocatable_pointer(type);
    if (!ASR::is_a<ASR::Array_t>(*past)) return false;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(past);
    return arr->m_physical_type
        == ASR::array_physical_typeType::DescriptorArray;
}

// Name of the run-time stride scalar for dimension `d` (0-based) of the
// descriptor array kernel argument `name`.
inline std::string gpu_stride_arg_name(const std::string &name, size_t d) {
    return "__stride_" + name + "_dim" + std::to_string(d + 1);
}

// Classify kernel arguments into buffer (array/struct) and scalar categories.
// Returns the count of buffer args and scalar args respectively.
// For struct array args with allocatable array members, counts 3 extra
// buffers per member (data, offsets, sizes) as emitted by Metal codegen.
inline std::pair<int, int> classify_gpu_kernel_args(
        const ASR::GpuKernelFunction_t &kernel) {
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
                    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
                    // Data, offsets and sizes buffers per allocatable
                    // array component, inherited ones included
                    n_buffer += 3 * (int)
                        ASRUtils::collect_allocatable_array_members(
                            st).size();
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

// If `expr` is a derived-type component reference rooted at a kernel
// argument (e.g. `s%m_`, or `s%inner%n`), record the argument index and
// the component chain (outermost component first).  Structs are passed to
// a GPU kernel as buffers rather than as materialized scalars, so the host
// must load the component from the struct at kernel-launch time.
inline bool find_struct_member_arg_in_expr(ASR::expr_t *expr,
        const std::vector<std::string> &arg_names,
        size_t &arg_index, std::vector<std::string> &member_path) {
    if (!expr) return false;
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return find_struct_member_arg_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg,
            arg_names, arg_index, member_path);
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*expr)) return false;
    ASR::StructInstanceMember_t *sim =
        ASR::down_cast<ASR::StructInstanceMember_t>(expr);
    ASR::symbol_t *member = ASRUtils::symbol_get_past_external(sim->m_m);
    if (!member || !ASR::is_a<ASR::Variable_t>(*member)) return false;
    if (ASR::is_a<ASR::Var_t>(*sim->m_v)) {
        std::string base_name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(sim->m_v)->m_v);
        for (size_t a = 0; a < arg_names.size(); a++) {
            if (arg_names[a] == base_name) {
                arg_index = a;
                member_path.push_back(ASRUtils::symbol_name(member));
                return true;
            }
        }
        return false;
    }
    if (!find_struct_member_arg_in_expr(sim->m_v, arg_names,
            arg_index, member_path)) {
        return false;
    }
    member_path.push_back(ASRUtils::symbol_name(member));
    return true;
}

// Count every write to `target` in a statement tree, and remember the
// value of the single defining binding when there is exactly one.  An
// ASSOCIATE construct that the GPU offload pass splices into a kernel
// body leaves its name behind as an ordinary local scalar defined by one
// assignment, so a workspace extent naming it can only be sized on the
// host by substituting that value -- which is sound solely when nothing
// else writes the name.  A loop index and a subroutine actual argument
// therefore count as writes too, and the whole statement tree is walked
// (BLOCK and ASSOCIATE bodies included) so no write can be missed.
class GpuScalarBindingCounter :
        public ASR::BaseWalkVisitor<GpuScalarBindingCounter> {
public:
    ASR::symbol_t *target;
    size_t n_writes = 0;
    ASR::expr_t *value = nullptr;

    GpuScalarBindingCounter(ASR::symbol_t *target_) : target(target_) {}

    bool is_target(ASR::expr_t *e) {
        return e != nullptr && ASR::is_a<ASR::Var_t>(*e) &&
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v) == target;
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (is_target(x.m_target)) {
            n_writes++;
            value = x.m_value;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        if (is_target(x.m_target)) {
            n_writes++;
            value = x.m_value;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>::visit_Associate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (is_target(x.m_head.m_v)) n_writes++;
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) {
            if (is_target(x.m_head[i].m_v)) n_writes++;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>
            ::visit_DoConcurrentLoop(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_target(x.m_args[i].m_value)) n_writes++;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>
            ::visit_SubroutineCall(x);
    }

    // The generated walker stops at a BLOCK or ASSOCIATE call, but a
    // write hidden inside one must still be seen.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (b == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }
};

// The value bound to the kernel-local integer scalar `sym` in `body`, or
// nullptr when the name is not defined exactly once there.  This is how a
// workspace extent reaches through an ASSOCIATE name: after the offload
// pass splices the construct, `associate(rows => self%m_ + 2)` shows up as
// a local `rows` assigned `self%m_ + 2` once, and only the selector
// expression can be evaluated on the host.
inline ASR::expr_t* find_gpu_local_scalar_binding(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
    if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)) return nullptr;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(s);
    if (ASRUtils::is_array(var->m_type)) return nullptr;
    if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::extract_type(var->m_type))) {
        return nullptr;
    }
    GpuScalarBindingCounter counter(s);
    for (size_t i = 0; i < n_body; i++) {
        counter.visit_stmt(*body[i]);
    }
    if (counter.n_writes != 1) return nullptr;
    return counter.value;
}

// The extent expression of dimension `d` (0-based) of a kernel-local
// array.  An automatic array carries its extents in its own `Array_t`;
// an allocatable carries none of its own, so they come from the ALLOCATE
// that sizes it in the same statement list.  Returns nullptr when the
// dimension has no extent expression to be found there.
inline ASR::expr_t* find_gpu_local_array_extent(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body, size_t d) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
    if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)) return nullptr;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(s);
    ASR::ttype_t *inner =
        ASRUtils::type_get_past_allocatable_pointer(var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*inner)) return nullptr;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);
    if (d < arr->n_dims && arr->m_dims[d].m_length) {
        return arr->m_dims[d].m_length;
    }
    std::string vname(var->m_name);
    ASR::Allocate_t *alloc = find_allocate_for_var(body, n_body, vname);
    if (alloc == nullptr) return nullptr;
    for (size_t ai = 0; ai < alloc->n_args; ai++) {
        if (!alloc->m_args[ai].m_a) continue;
        if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
        if (ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(
                alloc->m_args[ai].m_a)->m_v) != vname) {
            continue;
        }
        if (d >= alloc->m_args[ai].n_dims) return nullptr;
        return alloc->m_args[ai].m_dims[d].m_length;
    }
    return nullptr;
}

// Build a host-evaluable expression tree for a workspace dimension.
// Every leaf must be either an integer constant, a scalar kernel argument,
// or a derived-type component chain rooted at a kernel argument; every
// interior node must be an arithmetic integer operation.  Returns the
// index of the root node in `nodes`, or -1 when the expression contains
// something the host cannot reproduce.  Returning -1 keeps the caller's
// `-1` sentinel discipline: an unresolvable extent must produce a clean
// error, never a plausible-but-wrong value.
inline int64_t build_gpu_vla_dim_expr(ASR::expr_t *expr,
        const std::vector<std::string> &arg_names,
        ASR::stmt_t **body, size_t n_body,
        std::set<ASR::symbol_t*> &substituted,
        std::vector<GpuVlaDimNode> &nodes) {
    if (!expr) return -1;
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return build_gpu_vla_dim_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg, arg_names,
            body, n_body, substituted, nodes);
    }
    int64_t const_val;
    if (try_eval_int_constant(expr, const_val)) {
        GpuVlaDimNode n;
        n.kind = GpuVlaDimNode::Kind::Constant;
        n.constant_value = const_val;
        nodes.push_back(n);
        return (int64_t)nodes.size() - 1;
    }
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        for (size_t a = 0; a < arg_names.size(); a++) {
            if (arg_names[a] == name) {
                GpuVlaDimNode n;
                n.kind = GpuVlaDimNode::Kind::CallArg;
                n.call_arg_index = (int64_t)a;
                nodes.push_back(n);
                return (int64_t)nodes.size() - 1;
            }
        }
        // Not a kernel argument.  It may still be a local scalar bound
        // once in this scope -- what an ASSOCIATE name becomes once the
        // construct is spliced into the kernel body, or a block-local set
        // before the ALLOCATE -- in which case the extent is whatever it
        // was bound to.  A name already substituted is not substituted
        // again, so a self-referential binding cannot loop.
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        if (substituted.count(sym)) return -1;
        ASR::expr_t *bound = find_gpu_local_scalar_binding(sym, body,
            n_body);
        if (bound == nullptr) return -1;
        substituted.insert(sym);
        int64_t root = build_gpu_vla_dim_expr(bound, arg_names, body,
            n_body, substituted, nodes);
        substituted.erase(sym);
        return root;
    }
    if (ASR::is_a<ASR::ArraySize_t>(*expr)) {
        // `size(a)` of an array that is itself sized by the kernel
        // arguments: a kernel-argument array, whose extents the host
        // already passes as `__dim_<name>_<d>` scalars, or a kernel-local
        // array whose own extent expression can be built here in turn.
        // A whole-array `size(a)` is the product of every dimension.
        ASR::ArraySize_t *as = ASR::down_cast<ASR::ArraySize_t>(expr);
        if (!as->m_v || !ASR::is_a<ASR::Var_t>(*as->m_v)) return -1;
        ASR::symbol_t *arr_sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(as->m_v)->m_v);
        if (arr_sym == nullptr || !ASR::is_a<ASR::Variable_t>(*arr_sym)) {
            return -1;
        }
        ASR::Variable_t *arr_var =
            ASR::down_cast<ASR::Variable_t>(arr_sym);
        ASR::ttype_t *arr_type =
            ASRUtils::type_get_past_allocatable_pointer(arr_var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*arr_type)) return -1;
        size_t n_dims = ASR::down_cast<ASR::Array_t>(arr_type)->n_dims;
        if (n_dims == 0) return -1;
        size_t d_begin = 0;
        size_t d_end = n_dims;
        if (as->m_dim) {
            int64_t dim_val;
            if (!try_eval_int_constant(as->m_dim, dim_val)) return -1;
            if (dim_val < 1 || (size_t)dim_val > n_dims) return -1;
            d_begin = (size_t)dim_val - 1;
            d_end = d_begin + 1;
        }
        std::string arr_name(arr_var->m_name);
        bool is_kernel_arg = false;
        for (size_t a = 0; a < arg_names.size(); a++) {
            if (arg_names[a] == arr_name) {
                is_kernel_arg = true;
                break;
            }
        }
        if (substituted.count(arr_sym)) return -1;
        substituted.insert(arr_sym);
        int64_t acc = -1;
        for (size_t d = d_begin; d < d_end; d++) {
            int64_t one = -1;
            if (is_kernel_arg) {
                std::string dim_arg = "__dim_" + arr_name + "_"
                    + std::to_string(d);
                for (size_t a = 0; a < arg_names.size(); a++) {
                    if (arg_names[a] != dim_arg) continue;
                    GpuVlaDimNode n;
                    n.kind = GpuVlaDimNode::Kind::CallArg;
                    n.call_arg_index = (int64_t)a;
                    nodes.push_back(n);
                    one = (int64_t)nodes.size() - 1;
                    break;
                }
            } else {
                ASR::expr_t *extent = find_gpu_local_array_extent(
                    arr_sym, body, n_body, d);
                if (extent) {
                    one = build_gpu_vla_dim_expr(extent, arg_names, body,
                        n_body, substituted, nodes);
                }
            }
            if (one < 0) {
                substituted.erase(arr_sym);
                return -1;
            }
            if (acc < 0) {
                acc = one;
            } else {
                GpuVlaDimNode n;
                n.kind = GpuVlaDimNode::Kind::BinOp;
                n.binop = ASR::binopType::Mul;
                n.left = acc;
                n.right = one;
                nodes.push_back(n);
                acc = (int64_t)nodes.size() - 1;
            }
        }
        substituted.erase(arr_sym);
        return acc;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        size_t arg_index = 0;
        std::vector<std::string> member_path;
        if (!find_struct_member_arg_in_expr(expr, arg_names, arg_index,
                member_path)) {
            return -1;
        }
        GpuVlaDimNode n;
        n.kind = GpuVlaDimNode::Kind::StructMember;
        n.call_arg_index = (int64_t)arg_index;
        n.member_path = member_path;
        nodes.push_back(n);
        return (int64_t)nodes.size() - 1;
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        switch (op->m_op) {
            case ASR::binopType::Add:
            case ASR::binopType::Sub:
            case ASR::binopType::Mul:
            case ASR::binopType::Div:
                break;
            default:
                return -1;
        }
        int64_t left = build_gpu_vla_dim_expr(op->m_left, arg_names,
            body, n_body, substituted, nodes);
        if (left < 0) return -1;
        int64_t right = build_gpu_vla_dim_expr(op->m_right, arg_names,
            body, n_body, substituted, nodes);
        if (right < 0) return -1;
        GpuVlaDimNode n;
        n.kind = GpuVlaDimNode::Kind::BinOp;
        n.binop = op->m_op;
        n.left = left;
        n.right = right;
        nodes.push_back(n);
        return (int64_t)nodes.size() - 1;
    }
    return -1;
}

// Resolve a workspace dimension by evaluating the whole dimension
// expression on the host.  Returns true when `vd` was filled in.
inline bool resolve_gpu_vla_dim_expr(ASR::expr_t *dim,
        const std::vector<std::string> &arg_names,
        ASR::stmt_t **body, size_t n_body, GpuVlaDim &vd) {
    std::vector<GpuVlaDimNode> nodes;
    std::set<ASR::symbol_t*> substituted;
    int64_t root = build_gpu_vla_dim_expr(dim, arg_names, body, n_body,
        substituted, nodes);
    if (root < 0) return false;
    if (nodes[root].kind == GpuVlaDimNode::Kind::Constant) {
        vd.is_constant = true;
        vd.constant_value = nodes[root].constant_value;
        return true;
    }
    vd.is_constant = false;
    vd.constant_value = 0;
    vd.expr_nodes = std::move(nodes);
    vd.expr_root = root;
    return true;
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
                    std::string dim_arg = "__dim_" + base_name
                        + "_" + std::to_string(d);
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
        const ASR::GpuKernelFunction_t &kernel) {
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

// Element size in bytes of a workspace array's element type.
inline int gpu_workspace_elem_size(ASR::Array_t *arr) {
    if (ASR::is_a<ASR::Real_t>(*arr->m_type)) {
        return ASR::down_cast<ASR::Real_t>(arr->m_type)->m_kind;
    } else if (ASR::is_a<ASR::Integer_t>(*arr->m_type)) {
        return ASR::down_cast<ASR::Integer_t>(arr->m_type)->m_kind;
    }
    return 4;
}

// Resolve one workspace extent expression, in order of preference:
// a compile-time constant, a host-evaluable expression tree over the
// kernel arguments, or a single kernel argument.  When nothing matches,
// `call_arg_index` is left at -1 so the host reports a clean error rather
// than sizing the workspace from a plausible-but-wrong value.
inline GpuVlaDim resolve_gpu_workspace_dim(ASR::expr_t *dim,
        ASR::stmt_t **body, size_t n_body,
        const std::vector<std::string> &arg_names) {
    GpuVlaDim vd;
    vd.dim_expr = dim;
    if (!dim) {
        vd.is_constant = true;
        vd.constant_value = 1;
        return vd;
    }
    int64_t const_val;
    if (try_resolve_alloc_dim_constant(dim, body, n_body, const_val)) {
        vd.is_constant = true;
        vd.constant_value = const_val;
        return vd;
    }
    vd.is_constant = false;
    vd.constant_value = 0;
    if (resolve_gpu_vla_dim_expr(dim, arg_names, body, n_body, vd)) {
        return vd;
    }
    size_t idx = 0;
    if (find_arg_var_in_expr(dim, arg_names, idx)) {
        vd.call_arg_index = (int64_t)idx;
        return vd;
    }
    if (try_resolve_array_size_to_arg_var(dim, body, n_body, arg_names,
            idx)) {
        vd.call_arg_index = (int64_t)idx;
    }
    return vd;
}

// Find the ALLOCATE argument that gives the extents of `var_name`.
inline ASR::alloc_arg_t* find_alloc_arg_for_var(ASR::stmt_t **body,
        size_t n_body, const std::string &var_name) {
    ASR::Allocate_t *alloc = find_allocate_for_var(body, n_body, var_name);
    if (!alloc) return nullptr;
    for (size_t ai = 0; ai < alloc->n_args; ai++) {
        if (!alloc->m_args[ai].m_a) continue;
        if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
        std::string aname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(alloc->m_args[ai].m_a)->m_v);
        if (aname == var_name) return &alloc->m_args[ai];
    }
    return nullptr;
}

// Scan one scope (the kernel itself or a BLOCK inside it) for local
// arrays that need a per-thread device workspace buffer.
//
// Two shapes need one.  An automatic array (`real :: t(m)`) carries the
// run-time extent in its own `ASR::Array_t` dimensions.  An allocatable
// array carries no shape of its own -- its type is Allocatable(Array)
// with deferred `dimension_t` -- so its extents come from the ALLOCATE in
// the same scope; every temporary the array passes materialise for a
// spliced or blocked body has this shape.  A third shape, an allocatable
// with no ALLOCATE at all, is a function-result temporary whose size
// follows a struct member's allocatable array.
//
// `include_automatic_arrays` is false for the kernel scope: a kernel-scope
// automatic array has never been given a workspace, and adding one there
// would change the buffer accounting that `classify_gpu_kernel_args` and
// `gpu_vla_buffer_start` agree on.
inline void scan_gpu_scope_vlas(
        const ASR::GpuKernelFunction_t &kernel,
        SymbolTable *symtab,
        ASR::stmt_t **body, size_t n_body,
        const std::vector<std::string> &arg_names,
        bool include_automatic_arrays,
        int &buffer_idx,
        std::vector<GpuVlaWorkspace> &result) {
    if (!symtab) return;
    std::set<std::string> arg_set(arg_names.begin(), arg_names.end());
    std::set<std::string> handled_names;
    for (auto &ws : result) handled_names.insert(ws.var_name);

    for (auto &item : symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        std::string vname(var->m_name);
        if (arg_set.count(vname)) continue;
        if (handled_names.count(vname)) continue;
        bool is_alloc = ASRUtils::is_allocatable(var->m_type);
        if (!is_alloc && !include_automatic_arrays) continue;
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable(var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);

        // Extents declared with the array itself (an automatic array).
        ASR::dimension_t *dims = arr->m_dims;
        size_t n_dims = arr->n_dims;
        bool has_runtime_dim = false;
        for (size_t d = 0; d < n_dims; d++) {
            if (dims[d].m_length && !ASR::is_a<ASR::IntegerConstant_t>(
                    *dims[d].m_length)) {
                has_runtime_dim = true;
                break;
            }
        }
        bool from_allocate = false;
        if (!has_runtime_dim && is_alloc) {
            ASR::alloc_arg_t *aa = find_alloc_arg_for_var(body, n_body,
                vname);
            if (aa) {
                from_allocate = true;
                dims = aa->m_dims;
                n_dims = aa->n_dims;
                for (size_t d = 0; d < n_dims; d++) {
                    if (dims[d].m_length &&
                            !ASR::is_a<ASR::IntegerConstant_t>(
                                *dims[d].m_length)) {
                        has_runtime_dim = true;
                        break;
                    }
                }
            }
        }

        if (has_runtime_dim) {
            GpuVlaWorkspace ws;
            ws.var_name = vname;
            ws.buffer_index = buffer_idx++;
            ws.elem_size = gpu_workspace_elem_size(arr);
            for (size_t d = 0; d < n_dims; d++) {
                ws.dims.push_back(resolve_gpu_workspace_dim(
                    dims[d].m_length, body, n_body, arg_names));
            }
            handled_names.insert(vname);
            result.push_back(std::move(ws));
            continue;
        }
        if (from_allocate || !is_alloc) continue;

        // An allocatable with no ALLOCATE: a function-result temporary
        // sized from a struct member's allocatable array.
        std::string struct_key = find_struct_alloc_member_key(kernel);
        if (struct_key.empty()) continue;
        GpuVlaWorkspace ws;
        ws.var_name = vname;
        ws.buffer_index = buffer_idx++;
        ws.elem_size = gpu_workspace_elem_size(arr);
        GpuVlaDim vd;
        vd.dim_expr = nullptr;
        vd.is_constant = false;
        vd.constant_value = 0;
        vd.is_struct_member_size = true;
        vd.struct_member_key = struct_key;
        ws.dims.push_back(vd);
        handled_names.insert(vname);
        result.push_back(std::move(ws));
    }
}

// Collect every VLA workspace a kernel needs, assigning buffer indices
// from `buffer_idx`.  Both `count_gpu_vla_workspaces` (which must not
// know the buffer base yet) and `analyze_gpu_vla_workspaces` go through
// here, so the count used for the buffer accounting and the workspaces
// actually bound can never disagree.
inline void collect_gpu_vla_workspaces(
        const ASR::GpuKernelFunction_t &kernel,
        int &buffer_idx,
        std::vector<GpuVlaWorkspace> &result) {
    std::vector<std::string> arg_names;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        arg_names.push_back(std::string(var->m_name));
    }
    for (size_t i = 0; i < kernel.n_body; i++) {
        if (!ASR::is_a<ASR::BlockCall_t>(*kernel.m_body[i])) continue;
        ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
            kernel.m_body[i]);
        if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
        scan_gpu_scope_vlas(kernel, block->m_symtab, block->m_body,
            block->n_body, arg_names, true, buffer_idx, result);
    }
    scan_gpu_scope_vlas(kernel, kernel.m_symtab, kernel.m_body,
        kernel.n_body, arg_names, false, buffer_idx, result);
}

// Count VLA workspaces in a kernel without assigning buffer indices.
inline int count_gpu_vla_workspaces(const ASR::GpuKernelFunction_t &kernel) {
    int buffer_idx = 0;
    std::vector<GpuVlaWorkspace> result;
    collect_gpu_vla_workspaces(kernel, buffer_idx, result);
    return (int)result.size();
}

static const int MAX_METAL_BUFFERS = 31;
static const int PACKED_BUFFER_ALIGN = 16;

// Determine whether a kernel needs buffer packing because its total
// buffer count exceeds Metal's 31-slot limit.
inline bool gpu_kernel_needs_buffer_packing(
        const ASR::GpuKernelFunction_t &kernel) {
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    int n_vla = count_gpu_vla_workspaces(kernel);
    int total = n_buffer + (n_scalar > 0 ? 1 : 0) + n_vla;
    return total > MAX_METAL_BUFFERS;
}

// Compute the Metal buffer index where VLA workspace buffers start.
// Normal layout:  [buffer_args...] [scalar_struct?] [vla_workspaces...]
// Packed layout:  [packed_arrays(0)] [scalar_struct(1)] [vla_workspaces...]
inline int gpu_vla_buffer_start(const ASR::GpuKernelFunction_t &kernel) {
    if (gpu_kernel_needs_buffer_packing(kernel)) {
        return 2;
    }
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    return n_buffer + (n_scalar > 0 ? 1 : 0);
}

// Analyze a GPU kernel function for variable-length arrays in blocks.
// Returns workspace metadata for each VLA found, with buffer indices
// assigned sequentially starting after the kernel's packed arguments.
inline std::vector<GpuVlaWorkspace> analyze_gpu_vla_workspaces(
        const ASR::GpuKernelFunction_t &kernel) {
    int buffer_idx = gpu_vla_buffer_start(kernel);
    std::vector<GpuVlaWorkspace> result;
    collect_gpu_vla_workspaces(kernel, buffer_idx, result);
    return result;
}

// Scan a kernel body for alloc-assign statements that write a VLA workspace
// array to a struct array member.  Returns a map from
// "struct_name.member_name" to the per-element size (number of elements)
// determined by the VLA workspace dimensions.
inline std::map<std::string, int64_t> find_struct_member_vla_write_sizes(
        const ASR::GpuKernelFunction_t &kernel,
        const std::vector<GpuVlaWorkspace> &vla_workspaces) {
    std::map<std::string, int64_t> result;
    std::map<std::string, const GpuVlaWorkspace*> ws_by_name;
    for (auto &ws : vla_workspaces) {
        ws_by_name[ws.var_name] = &ws;
    }
    for (size_t si = 0; si < kernel.n_body; si++) {
        ASR::stmt_t *stmt = kernel.m_body[si];
        if (stmt->type == ASR::stmtType::Assignment) {
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::StructInstanceMember_t>(*asgn->m_target))
                continue;
            if (!ASR::is_a<ASR::Var_t>(*asgn->m_value)) continue;
            ASR::StructInstanceMember_t *sm =
                ASR::down_cast<ASR::StructInstanceMember_t>(
                    asgn->m_target);
            std::string mem_name = ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(sm->m_m));
            std::string struct_name;
            if (ASR::is_a<ASR::ArrayItem_t>(*sm->m_v)) {
                ASR::ArrayItem_t *ai =
                    ASR::down_cast<ASR::ArrayItem_t>(sm->m_v);
                if (ASR::is_a<ASR::Var_t>(*ai->m_v)) {
                    struct_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(ai->m_v)->m_v);
                }
            }
            if (struct_name.empty()) continue;
            std::string val_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(asgn->m_value)->m_v);
            auto ws_it = ws_by_name.find(val_name);
            if (ws_it == ws_by_name.end()) continue;
            int64_t per_elem = 1;
            bool all_const = true;
            for (auto &dim : ws_it->second->dims) {
                if (dim.is_constant) {
                    per_elem *= dim.constant_value;
                } else {
                    all_const = false;
                    break;
                }
            }
            if (all_const && per_elem > 0) {
                result[struct_name + "." + mem_name] = per_elem;
            }
        } else if (stmt->type == ASR::stmtType::SubroutineCall) {
            // Look through subroutine calls for writes to struct
            // allocatable members (e.g., construct(x, a(i)) where
            // the function body does r%v = x).
            ASR::SubroutineCall_t *sc =
                ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            ASR::symbol_t *fn_sym =
                ASRUtils::symbol_get_past_external(sc->m_name);
            if (!ASR::is_a<ASR::Function_t>(*fn_sym)) continue;
            ASR::Function_t *fn =
                ASR::down_cast<ASR::Function_t>(fn_sym);
            // Find which actual args are struct array elements
            for (size_t ai = 0; ai < sc->n_args; ai++) {
                if (!sc->m_args[ai].m_value) continue;
                if (!ASR::is_a<ASR::ArrayItem_t>(
                        *sc->m_args[ai].m_value)) continue;
                ASR::ArrayItem_t *arr_item =
                    ASR::down_cast<ASR::ArrayItem_t>(
                        sc->m_args[ai].m_value);
                ASR::ttype_t *elem_type = arr_item->m_type;
                if (!ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(elem_type)))
                    continue;
                if (!ASR::is_a<ASR::Var_t>(*arr_item->m_v)) continue;
                std::string arr_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        arr_item->m_v)->m_v);
                // Get the formal parameter name for this arg
                if (ai >= fn->n_args) continue;
                ASR::Variable_t *formal =
                    ASR::down_cast<ASR::Variable_t>(
                        ASR::down_cast<ASR::Var_t>(
                            fn->m_args[ai])->m_v);
                std::string formal_name(formal->m_name);
                // Scan function body for assignments to
                // formal%member = some_array_param
                for (size_t fi = 0; fi < fn->n_body; fi++) {
                    if (fn->m_body[fi]->type !=
                            ASR::stmtType::Assignment) continue;
                    ASR::Assignment_t *fa =
                        ASR::down_cast<ASR::Assignment_t>(
                            fn->m_body[fi]);
                    if (!ASR::is_a<ASR::StructInstanceMember_t>(
                            *fa->m_target)) continue;
                    ASR::StructInstanceMember_t *fsm =
                        ASR::down_cast<ASR::StructInstanceMember_t>(
                            fa->m_target);
                    // Check target struct var matches the formal
                    if (!ASR::is_a<ASR::Var_t>(*fsm->m_v)) continue;
                    std::string tgt_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            fsm->m_v)->m_v);
                    if (tgt_name != formal_name) continue;
                    std::string mem_name = ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(
                            fsm->m_m));
                    std::string key = arr_name + "." + mem_name;
                    if (result.count(key)) continue;
                    // RHS is a Var — find its size from the actual
                    // argument at the call site
                    if (!ASR::is_a<ASR::Var_t>(*fa->m_value))
                        continue;
                    std::string rhs_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            fa->m_value)->m_v);
                    // Find which formal param index this is
                    for (size_t pi = 0; pi < fn->n_args; pi++) {
                        ASR::Variable_t *fp =
                            ASR::down_cast<ASR::Variable_t>(
                                ASR::down_cast<ASR::Var_t>(
                                    fn->m_args[pi])->m_v);
                        if (std::string(fp->m_name) != rhs_name)
                            continue;
                        if (pi >= sc->n_args) break;
                        // Get the actual arg's size
                        ASR::expr_t *actual =
                            sc->m_args[pi].m_value;
                        if (!actual) break;
                        ASR::ttype_t *at =
                            ASRUtils::expr_type(actual);
                        ASR::ttype_t *past =
                            ASRUtils::type_get_past_allocatable(at);
                        if (!ASR::is_a<ASR::Array_t>(*past)) break;
                        ASR::Array_t *arr =
                            ASR::down_cast<ASR::Array_t>(past);
                        int64_t sz = 1;
                        bool all_c = true;
                        for (size_t d = 0; d < arr->n_dims; d++) {
                            if (arr->m_dims[d].m_length &&
                                    ASR::is_a<ASR::IntegerConstant_t>(
                                        *arr->m_dims[d].m_length)) {
                                sz *= ASR::down_cast<
                                    ASR::IntegerConstant_t>(
                                    arr->m_dims[d].m_length)->m_n;
                            } else {
                                all_c = false;
                                break;
                            }
                        }
                        if (all_c && sz > 0) {
                            result[key] = sz;
                        } else if (!all_c &&
                                ASR::is_a<ASR::Var_t>(*actual)) {
                            std::string act_name =
                                ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(
                                        actual)->m_v);
                            auto ws_it2 =
                                ws_by_name.find(act_name);
                            if (ws_it2 != ws_by_name.end()) {
                                int64_t ws_sz = 1;
                                bool ws_all_c = true;
                                for (auto &dim :
                                        ws_it2->second->dims) {
                                    if (dim.is_constant) {
                                        ws_sz *=
                                            dim.constant_value;
                                    } else {
                                        ws_all_c = false;
                                        break;
                                    }
                                }
                                if (ws_all_c && ws_sz > 0) {
                                    result[key] = ws_sz;
                                }
                            }
                        }
                        break;
                    }
                }
            }
        }
    }
    return result;
}

// Find struct member VLA writes whose size is determined at runtime from
// another struct array member (e.g., b(i)%v = x where x comes from a(i)%v).
// Returns a map from target "struct.member" key to source "struct.member" key.
inline std::map<std::string, std::string> find_struct_member_vla_runtime_sources(
        const ASR::GpuKernelFunction_t &kernel) {
    std::map<std::string, std::string> result;
    for (size_t si = 0; si < kernel.n_body; si++) {
        ASR::stmt_t *stmt = kernel.m_body[si];
        if (stmt->type != ASR::stmtType::SubroutineCall) continue;
        ASR::SubroutineCall_t *sc =
            ASR::down_cast<ASR::SubroutineCall_t>(stmt);
        ASR::symbol_t *fn_sym =
            ASRUtils::symbol_get_past_external(sc->m_name);
        if (!ASR::is_a<ASR::Function_t>(*fn_sym)) continue;
        ASR::Function_t *fn =
            ASR::down_cast<ASR::Function_t>(fn_sym);
        // Find which actual args are struct array elements (output)
        for (size_t ai = 0; ai < sc->n_args; ai++) {
            if (!sc->m_args[ai].m_value) continue;
            if (!ASR::is_a<ASR::ArrayItem_t>(
                    *sc->m_args[ai].m_value)) continue;
            ASR::ArrayItem_t *arr_item =
                ASR::down_cast<ASR::ArrayItem_t>(
                    sc->m_args[ai].m_value);
            ASR::ttype_t *elem_type = arr_item->m_type;
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(elem_type)))
                continue;
            if (!ASR::is_a<ASR::Var_t>(*arr_item->m_v)) continue;
            std::string arr_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(
                    arr_item->m_v)->m_v);
            if (ai >= fn->n_args) continue;
            ASR::Variable_t *formal =
                ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(
                        fn->m_args[ai])->m_v);
            std::string formal_name(formal->m_name);
            // Scan function body for formal%member = some_param
            for (size_t fi = 0; fi < fn->n_body; fi++) {
                if (fn->m_body[fi]->type !=
                        ASR::stmtType::Assignment) continue;
                ASR::Assignment_t *fa =
                    ASR::down_cast<ASR::Assignment_t>(
                        fn->m_body[fi]);
                if (!ASR::is_a<ASR::StructInstanceMember_t>(
                        *fa->m_target)) continue;
                ASR::StructInstanceMember_t *fsm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(
                        fa->m_target);
                if (!ASR::is_a<ASR::Var_t>(*fsm->m_v)) continue;
                std::string tgt_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        fsm->m_v)->m_v);
                if (tgt_name != formal_name) continue;
                std::string mem_name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(
                        fsm->m_m));
                std::string tgt_key = arr_name + "." + mem_name;
                if (result.count(tgt_key)) continue;
                // RHS is a Var — find its actual arg
                if (!ASR::is_a<ASR::Var_t>(*fa->m_value))
                    continue;
                std::string rhs_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        fa->m_value)->m_v);
                for (size_t pi = 0; pi < fn->n_args; pi++) {
                    ASR::Variable_t *fp =
                        ASR::down_cast<ASR::Variable_t>(
                            ASR::down_cast<ASR::Var_t>(
                                fn->m_args[pi])->m_v);
                    if (std::string(fp->m_name) != rhs_name)
                        continue;
                    if (pi >= sc->n_args) break;
                    ASR::expr_t *actual =
                        sc->m_args[pi].m_value;
                    if (!actual) break;
                    // Check if actual is a StructInstanceMember
                    // of a struct array element (e.g., a(i)%v)
                    if (!ASR::is_a<ASR::StructInstanceMember_t>(
                            *actual)) break;
                    ASR::StructInstanceMember_t *src_sm =
                        ASR::down_cast<ASR::StructInstanceMember_t>(
                            actual);
                    std::string src_mem = ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(
                            src_sm->m_m));
                    if (ASR::is_a<ASR::ArrayItem_t>(
                            *src_sm->m_v)) {
                        ASR::ArrayItem_t *src_ai =
                            ASR::down_cast<ASR::ArrayItem_t>(
                                src_sm->m_v);
                        if (ASR::is_a<ASR::Var_t>(
                                *src_ai->m_v)) {
                            std::string src_arr =
                                ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(
                                        src_ai->m_v)->m_v);
                            result[tgt_key] =
                                src_arr + "." + src_mem;
                        }
                    }
                    break;
                }
            }
        }
    }
    return result;
}

} // namespace LCompilers

#endif // LFORTRAN_GPU_UTILS_H
