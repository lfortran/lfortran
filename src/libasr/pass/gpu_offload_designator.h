#ifndef LIBASR_PASS_GPU_OFFLOAD_DESIGNATOR_H
#define LIBASR_PASS_GPU_OFFLOAD_DESIGNATOR_H

#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

namespace LCompilers {

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

GpuDesignatorBase gpu_designator_base(ASR::expr_t *e);

// Whether any designator inside an expression hangs off a given root.
class GpuDesignatorBaseFinder :
        public ASR::BaseWalkVisitor<GpuDesignatorBaseFinder> {
public:
    GpuDesignatorBase wanted;
    bool found = false;

    void visit_Var(const ASR::Var_t &x) {
        if (gpu_designator_base(
                const_cast<ASR::expr_t*>(&x.base)) == wanted) {
            found = true;
        }
    }
};

// True when two designators are provably the same storage in the same
// element order, so an element-by-element copy between them reads only
// what it has already written to the same element.
//
// A derived-type component chain has to be recognised here, not just a
// bare variable: `self%points_(k)` written twice is two ASR nodes for
// one object, and answering false for them makes every occurrence of
// such a chain look like a different object.
bool gpu_same_designator(ASR::expr_t *a, ASR::expr_t *b);

// True when `outer` designates storage inside `inner`, or `inner`
// itself: the descent from `outer` towards its root passes through a
// designator naming the very same element. `x%c_(k)%v_(:,j)` is within
// `x%c_(k)`; `x%c_` is not, and neither is `x%c_(m)`.
bool gpu_designator_within(ASR::expr_t *outer, ASR::expr_t *inner);

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

    // A bound or a size reads the array's shape, never its elements, so
    // the array named in one cannot alias what the assignment writes.
    // `a(:,i)` on an array whose extents are not known until run time is
    // lowered to `a(lbound(a,1):ubound(a,1):1, i)`, so without this every
    // such section reports itself as aliasing its own subscripts.
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    // The second operand of a broadcast is the shape the scalar is spread
    // over, not data the assignment reads: `b(1:n) = 1.0` is lowered with
    // the target's own shape there, which would otherwise report every
    // such assignment as reading what it writes.
    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t &x) {
        visit_expr(*x.m_array);
    }
};

// Every symbol whose storage a statement list may modify, identified by
// the root of the designator written to.  A gather may only be hoisted
// out of a loop when nothing in the loop can change what it copied, so
// this errs towards reporting a write: an actual argument bound to a
// dummy that is not `intent(in)`, or to a callee that cannot be
// resolved, counts as written.
class GpuWrittenRootCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuWrittenRootCollector> {
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


    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        note_call_args(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        note_call_args(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<GpuWrittenRootCollector>::visit_FunctionCall(x);
    }
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_DESIGNATOR_H
