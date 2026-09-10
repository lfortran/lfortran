#ifndef LIBASR_PASS_GPU_OFFLOAD_PREFLIGHT_H
#define LIBASR_PASS_GPU_OFFLOAD_PREFLIGHT_H

#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

// The scalar kernel parameter carrying one extent of an array parameter.
// The kernel extraction creates it and the workspace resolver looks it up
// again by name, so both spell it here.
// The pieces of `size(s%m, d)` and `size(a(i)%m, d)`: the extent of one
// dimension of an allocatable array component of a derived type, reached
// either directly from a kernel argument or through a subscript into an
// array of them.  Neither side of a kernel launch can reproduce such an
// extent as it stands -- the kernel is handed the component as one flat
// data buffer plus a per-element total size, never the component's
// individual extents -- so the GPU offload pass turns it into a scalar
// kernel argument instead.  The pass and the workspace resolver
// recognise the shape through this one function, so what the pass
// rewrites and what the resolver accepts cannot drift apart.
struct GpuStructArrayMemberExtentRef {
    // The subscript into an array of derived types, or nullptr when the
    // component hangs off the kernel argument directly.
    ASR::ArrayItem_t *item = nullptr;
    // The kernel argument the component path is rooted at.
    ASR::Var_t *base = nullptr;
    ASR::StructInstanceMember_t *member = nullptr;
    int64_t dim = 0; // 1-based
};

// The same shape, given the component reference and the 1-based
// dimension on their own rather than wrapped in an `ArraySize`.  A
// dimension of an array-valued intrinsic's operand is asked for this way:
// the extent is derived from the operand, and no `size(...)` naming it
// exists anywhere for the caller to hand over.
bool match_gpu_struct_array_member_extent_base(ASR::expr_t *expr,
        int64_t d, const std::vector<std::string> &arg_names,
        GpuStructArrayMemberExtentRef &out);

bool match_gpu_struct_array_member_extent(ASR::expr_t *expr,
        const std::vector<std::string> &arg_names,
        GpuStructArrayMemberExtentRef &out);

// The designator whose shape is the shape of the local array `var` of a
// BLOCK or ASSOCIATE scope, or nullptr when the shape is nowhere to be
// found.
//
// The temporary the frontend materialises for an array-valued ASSOCIATE
// selector -- `associate(r => sqrt((x-x0)**2 + ...))` -- is an allocatable
// with deferred extents and no ALLOCATE: the only place its shape is
// written is the expression assigned to it, and that expression is
// elementwise, so its shape in turn is that of one of its array operands.
// Follow the chain down to the designator that actually has extents.
ASR::expr_t* gpu_scope_array_shape_source(const ASR::Variable_t *var,
        ASR::stmt_t **body, size_t n_body);

// The same question for the whole loop body: the scopes it opens hold the
// workspaces, so each one is asked in turn.
bool gpu_block_workspace_extents_resolvable(
        ASR::stmt_t **body, size_t n_body,
        const std::vector<std::string> &arg_names,
        std::string &unresolved_name);

bool gpu_kernel_workspace_extents_resolvable(const ASR::Function_t &kernel,
    std::string &unresolved_name);

bool gpu_function_result_allocation_is_supported(const ASR::Function_t &fn);

// A derived type is representable only when every one of its data members
// is, because the device struct is laid out member by member: a single
// unsupported member anywhere in the type changes the element size the
// kernel would have to stride by, while the host buffer keeps the wider
// layout. The members inherited through `extends` live in the parent
// Struct (they are reached at run time through the `__parent` member), so
// the parent chain has to be walked as well. `visited` guards against
// self-referential types such as `type(node), pointer :: next`, whose
// member graph is cyclic.
bool gpu_struct_members_ok(ASR::symbol_t *struct_sym,
        std::set<ASR::Struct_t*> &visited,
        const GpuDeviceCapabilities &caps, ASR::ttype_t **unsupported_type = nullptr);

// Answers whether the selected device can represent the type of `e` with
// the same in-memory width the host uses. A device whose type set is
// narrower than the host's -- no 64-bit floating point type, no 64-bit
// boolean, no complex type -- lowers such data to a narrower (or bogus)
// type. Offloading a `do concurrent` that touches it would make the kernel
// reinterpret the host buffers and the by-value scalar-argument struct at
// the wrong element size, silently producing wrong results, so such a loop
// has to stay on the CPU.
//
// The rule itself is the capability descriptor's, so that the decline this
// raises and the class that decline is given cannot disagree about what the
// device has a type for.
bool gpu_device_can_represent_type(const GpuDeviceCapabilities &caps,
        ASR::ttype_t *t, ASR::expr_t *e, ASR::ttype_t **unsupported_type = nullptr);

// The scalar element type behind `t`, for a decline that has to be
// classified against what the device has a type for. A derived type has no
// single element type -- the width that offends is one member's -- so it
// answers with nothing, and the decline is classified on its reason alone.

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
                ASR::Allocate_t *alloc = find_allocate_for_var(body,
                    n_body, std::string(var->m_name));
                ASR::alloc_arg_t *aa = alloc
                    ? find_alloc_arg_for_var(alloc,
                        std::string(var->m_name))
                    : nullptr;
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

// A BLOCK-local is not a kernel argument, so GpuSymbolCollector skips it
// and the representability sweep over kernel-reaching symbols never sees
// it. The device's type set still has to apply: a real(16) local used to
// compile as float at a 16-byte host stride, and a derived-type local whose
// member is real(8) or complex is the same hole.
class GpuLocalWidthChecker :
        public ASR::BaseWalkVisitor<GpuLocalWidthChecker> {
public:
    bool unsupported = false;
    std::string bad_name;
    // The element type that was turned down, when it is a scalar one. A
    // derived type leaves this null: the width that offends is a member's,
    // and the message names the local rather than a type.
    ASR::ttype_t *bad_type = nullptr;
    // What the selected device has a scalar type of.
    GpuDeviceCapabilities caps;

    bool type_ok(ASR::Variable_t *var, ASR::ttype_t *&offending) {
        ASR::ttype_t *base = ASRUtils::extract_type(var->m_type);
        if (ASR::is_a<ASR::StructType_t>(*base)) {
            // A BLOCK-local is not a kernel argument, so the symbol
            // collector never hands this type to
            // gpu_device_can_represent_type. Walk the members here: a
            // real(8) or complex component is the same silent-wrong-width
            // hole the scalar path already closed.
            if (!var->m_type_declaration) return false;
            std::set<ASR::Struct_t*> visited;
            return gpu_struct_members_ok(var->m_type_declaration, visited,
                caps);
        }
        offending = base;
        return caps.has_scalar_type(base);
    }

    void check_scope(SymbolTable *symtab) {
        if (!symtab) return;
        for (auto &item : symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                item.second);
            ASR::ttype_t *offending = nullptr;
            if (!type_ok(var, offending)) {
                unsupported = true;
                if (bad_name.empty()) {
                    bad_name = item.first;
                    bad_type = offending;
                }
            }
        }
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::Block_t>(*b)) return;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        check_scope(blk->m_symtab);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) return;
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        check_scope(blk->m_symtab);
        for (size_t i = 0; i < blk->n_body; i++) {
            visit_stmt(*blk->m_body[i]);
        }
    }
};

// A statement the device has no way to run. A kernel that held one would
// simply not run it, so the effect the program asked for would go missing
// with nothing to show for it.
GpuDeclineReason unsupported_on_device(const ASR::stmt_t &s);

// The first statement of a body that the device cannot run, and where it is.
class GpuUnsupportedStatementFinder
        : public ASRUtils::BlockBodyWalkVisitor<GpuUnsupportedStatementFinder> {
public:
    GpuDeclineReason reason;
    Location loc;

    GpuUnsupportedStatementFinder() : reason(GpuDeclineReason::None) {}

    void visit_stmt(const ASR::stmt_t &s) {
        if (reason != GpuDeclineReason::None) return;
        GpuDeclineReason why = unsupported_on_device(s);
        if (why != GpuDeclineReason::None) {
            reason = why;
            loc = s.base.loc;
            return;
        }
        ASR::BaseWalkVisitor<GpuUnsupportedStatementFinder>::visit_stmt(s);
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
        public ASRUtils::BlockBodyWalkVisitor<GpuDeviceFunctionArrayTempChecker> {
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
            public ASRUtils::BlockBodyWalkVisitor<RuntimeAllocCollector> {
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
        // The result is not a dummy yet. subroutine_from_function later
        // turns it into one and plants a VLA temporary at the call site
        // in the kernel. That temporary has to be spliced into the
        // kernel (or the loop declined) so pre-flight can size it.
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
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_PREFLIGHT_H
