#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/hoist_realloc.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <cstring>
#include <map>
#include <set>
#include <vector>

namespace LCompilers {

/*
Hoisting of loop-invariant reallocations out of DO loops.

An intrinsic assignment to an allocatable array reallocates it first when it
is unallocated or has another shape (F2018 10.2.1.3). The array_op pass
writes that check as a ReAlloc before the loop of the assignment, so that an
assignment in a loop,

    do i = 1, n
        a(1) = a(1) + 1
        c = a + b
    end do

checks, and may reallocate, `c` in every iteration:

    do i = 1, n
        a(1) = a(1) + 1
        ReAlloc(c, [size(a, 1)])
        do k = lbound(c, 1), ubound(c, 1)
            c(k) = a(k) + b(k)
        end do
    end do

The check is cheap, but the allocation it may call can change any memory, so
the code generator can no longer keep the descriptors of the arrays of the
loop, or anything derived from them, out of the loop. When nothing in the
loop changes the allocation of `c` or what its shape is computed from, the
ReAlloc gives `c` the same shape in every iteration and does nothing after
the first one. It then runs once before the loop, when the loop runs at all:

    if (1 <= n) ReAlloc(c, [size(a, 1)])
    do i = 1, n
        a(1) = a(1) + 1
        do k = lbound(c, 1), ubound(c, 1)
            c(k) = a(k) + b(k)
        end do
    end do

A statement H of the loop body (a ReAlloc, or an IF without ELSE of such
statements, as this pass leaves before an inner loop) moves before the loop
only when all of the following hold, so that the program does the same:

* H is a statement of the body itself, not nested in another statement, and
  the statements before it in the body cannot leave the iteration (EXIT,
  CYCLE or RETURN of the loop) or stop the program, and call no procedure,
  which could do either: H then runs in the first iteration when the loop
  runs at all, and runs there first among what the body does to the arrays
  H reallocates;
* the statements before H do not name an array H reallocates, so moving H
  before them in the first iteration cannot change what they do;
* no statement of the body other than H and the statements before H that
  move before the loop too can change the allocation, shape or bounds of an
  array H reallocates, or the allocation or shape of an array whose shape H
  reads, or any value H reads, and H does not read the loop variable: every
  later execution of H then finds its arrays allocated with the shape it
  would give them, and does nothing. So in `t1 = a + b; t2 = 2*t1`, the
  reallocation of `t2` to the shape of `t1` moves after that of `t1`;
* a variable that something can reach by another name (TARGET, POINTER,
  EQUIVALENCE, COMMON, the target of a pointer assignment, a variable of an
  enclosing scope) is not one H reallocates, nor one whose value H reads,
  and when the body calls a procedure, every variable H reallocates or reads
  is local to this procedure (or a dummy argument of it), with no internal
  procedure that could reach it: such a variable can be changed by a
  procedure only when it is an actual argument of the call, which the body
  does name (F2018 15.5.2.13);
* the loop has a constant step, its bounds call no procedure and do not
  name an array H reallocates (but may inquire about the shape of a
  variable a component of which H reallocates, as in `do j = 1, size(p)`
  around `p(i)%d = ...`), so that they can be evaluated again for the test
  that the loop runs at all, with the same value.

Every other statement, and every statement this analysis does not know,
keeps H in the loop. Loops are processed innermost first, so that a ReAlloc
that leaves an inner loop can leave an enclosing loop as well.

A DO WHILE loop is handled as a DO loop, with its condition, which calls no
procedure, as the test that it runs at least once.

The target of H may also be a component that only one variable reaches
(`q%x`, see component_root), which then stands for the target.

H may also allocate an unallocated array, as a work array that keeps its
allocation between calls is allocated, `if (.not. allocated(w))
allocate(w(n))`. It does nothing once `w` is allocated, whatever shape the
loop gives it then, so the statements of the body may reallocate `w`, as
`w = a + b` does, when none of them can deallocate it.

The shape H gives may be computed by a function that reads only its
arguments (see reads_only_its_arguments), such as the `count` of
`c = pack(a, m)`, and a procedure does not change an INTENT(IN) dummy
argument. A call of such a function elsewhere in the body, such as of the
implementation of `mod`, is no call that could change anything.

What cannot move before its loop may still be removed from a copy of it,
see version_loop.
*/

namespace {

using SymbolSet = std::set<ASR::symbol_t*>;

ASR::symbol_t* past_external(ASR::symbol_t* s) {
    return ASRUtils::symbol_get_past_external(s);
}

bool intersects(const SymbolSet& a, const SymbolSet& b) {
    const SymbolSet& smaller = a.size() <= b.size() ? a : b;
    const SymbolSet& larger = a.size() <= b.size() ? b : a;
    for (ASR::symbol_t* s : smaller) {
        if (larger.count(s) > 0) {
            return true;
        }
    }
    return false;
}

// The variable the designator `e` is a part of, or nullptr when `e` is not
// a designator.
ASR::symbol_t* designator_root(ASR::expr_t* e) {
    while (true) {
        switch (e->type) {
            case ASR::exprType::Var:
                return past_external(ASR::down_cast<ASR::Var_t>(e)->m_v);
            case ASR::exprType::ArrayItem:
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            case ASR::exprType::ArraySection:
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            case ASR::exprType::StructInstanceMember:
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                break;
            case ASR::exprType::UnionInstanceMember:
                e = ASR::down_cast<ASR::UnionInstanceMember_t>(e)->m_v;
                break;
            case ASR::exprType::ArrayPhysicalCast:
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            case ASR::exprType::StringItem:
                e = ASR::down_cast<ASR::StringItem_t>(e)->m_arg;
                break;
            case ASR::exprType::StringSection:
                e = ASR::down_cast<ASR::StringSection_t>(e)->m_arg;
                break;
            default:
                return nullptr;
        }
    }
}

class VarCollector : public ASR::BaseWalkVisitor<VarCollector> {
public:
    SymbolSet& vars;

    explicit VarCollector(SymbolSet& vars_) : vars(vars_) {}

    void visit_Var(const ASR::Var_t& x) {
        vars.insert(past_external(x.m_v));
    }
};

void collect_vars(ASR::expr_t* e, SymbolSet& vars) {
    if (e != nullptr) {
        VarCollector c(vars);
        c.visit_expr(*e);
    }
}

bool is_derived_type(ASR::expr_t* e) {
    ASR::ttype_t* t = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(e)));
    return ASR::is_a<ASR::StructType_t>(*t);
}

// The variable of which `e` is the whole or a component of a component...
// that only that variable can reach: `e` is a variable, or a component of
// such a designator that is a scalar of a derived type that is not
// polymorphic and not a pointer, as `q%x` or `q%s%x`. Otherwise nullptr.
// Anything that changes the allocation of `e` then changes the allocation of
// a part of that variable. With `subscripts`, the scalar may also be an
// element of such an array that is not a pointer, as `p(i)%x`, and the
// subscripts are appended to `subscripts`: `e` then designates the same
// object as long as their values do not change.
ASR::symbol_t* component_root(ASR::expr_t* e,
        std::vector<ASR::expr_t*>* subscripts = nullptr) {
    if (ASR::is_a<ASR::Var_t>(*e)) {
        return past_external(ASR::down_cast<ASR::Var_t>(e)->m_v);
    }
    if (subscripts != nullptr && ASR::is_a<ASR::ArrayItem_t>(*e)) {
        ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(e);
        ASR::ttype_t* array_type = ASRUtils::expr_type(item->m_v);
        ASR::ttype_t* element_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(array_type));
        if (ASRUtils::is_pointer(array_type) ||
                !ASR::is_a<ASR::StructType_t>(*element_type) ||
                ASRUtils::is_class_type(element_type)) {
            return nullptr;
        }
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left != nullptr || item->m_args[i].m_step != nullptr ||
                    item->m_args[i].m_right == nullptr) {
                return nullptr;
            }
            subscripts->push_back(item->m_args[i].m_right);
        }
        return component_root(item->m_v, subscripts);
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        return nullptr;
    }
    ASR::expr_t* base = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
    ASR::ttype_t* base_type = ASRUtils::expr_type(base);
    if (ASRUtils::is_array(base_type) || ASRUtils::is_pointer(base_type) ||
            !ASR::is_a<ASR::StructType_t>(
                *ASRUtils::type_get_past_allocatable(base_type)) ||
            ASRUtils::is_class_type(
                ASRUtils::type_get_past_allocatable(base_type))) {
        return nullptr;
    }
    return component_root(base, subscripts);
}

// Whether an input/output statement may run a defined input/output
// procedure: it transfers a derived-type value.
class DerivedTypeFinder : public ASR::BaseWalkVisitor<DerivedTypeFinder> {
public:
    bool found = false;

    void visit_expr(const ASR::expr_t& x) {
        ASR::expr_t* e = const_cast<ASR::expr_t*>(&x);
        if (ASRUtils::expr_type(e) != nullptr && is_derived_type(e)) {
            found = true;
            return;
        }
        ASR::BaseWalkVisitor<DerivedTypeFinder>::visit_expr(x);
    }
};

// Whether the dummy argument `arg` of a procedure has INTENT(IN).
bool is_intent_in(ASR::expr_t* arg) {
    if (!ASR::is_a<ASR::Var_t>(*arg)) {
        return false;
    }
    ASR::symbol_t* s = past_external(ASR::down_cast<ASR::Var_t>(arg)->m_v);
    return ASR::is_a<ASR::Variable_t>(*s) &&
        ASR::down_cast<ASR::Variable_t>(s)->m_intent == ASR::intentType::In;
}

// Whether the procedure `s` called as a function is declared pure.
bool is_pure_function(ASR::symbol_t* s) {
    s = past_external(s);
    ASR::ttype_t* type = nullptr;
    if (ASR::is_a<ASR::Function_t>(*s)) {
        type = ASR::down_cast<ASR::Function_t>(s)->m_function_signature;
    } else if (ASR::is_a<ASR::Variable_t>(*s)) {
        type = ASRUtils::type_get_past_pointer(
            ASR::down_cast<ASR::Variable_t>(s)->m_type);
    } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*s)) {
        // An overriding binding of a pure binding is pure (F2018 7.5.7.3).
        return is_pure_function(ASR::down_cast<ASR::StructMethodDeclaration_t>(s)->m_proc);
    }
    return type != nullptr && ASR::is_a<ASR::FunctionType_t>(*type) &&
        ASR::down_cast<ASR::FunctionType_t>(type)->m_pure;
}

// What statements may do to variables. The sets hold the variables past
// their external symbols.
struct Effects {
    // Variables whose allocation status, shape, bounds or pointer
    // association may change.
    SymbolSet alloc_modified;
    // Variables that may be left unallocated or disassociated, a subset of
    // alloc_modified: an assignment, ALLOCATE or ReAlloc leaves an
    // allocatable array of an intrinsic type allocated.
    SymbolSet deallocated;
    // Variables any part of whose value may change, a superset of
    // alloc_modified.
    SymbolSet value_modified;
    // Every variable named.
    SymbolSet referenced;
    // A procedure is called that may change what it can reach.
    bool calls = false;
    // A procedure is called that may read what it can reach.
    bool reads_through_call = false;
    // Control may leave the iteration of the enclosing loop other than by
    // reaching the end of its body.
    bool escapes = false;
    // The program may stop.
    bool terminates = false;
    // A statement this analysis does not know.
    bool unknown = false;

    void merge(const Effects& o) {
        alloc_modified.insert(o.alloc_modified.begin(), o.alloc_modified.end());
        deallocated.insert(o.deallocated.begin(), o.deallocated.end());
        value_modified.insert(o.value_modified.begin(), o.value_modified.end());
        referenced.insert(o.referenced.begin(), o.referenced.end());
        calls = calls || o.calls;
        reads_through_call = reads_through_call || o.reads_through_call;
        escapes = escapes || o.escapes;
        terminates = terminates || o.terminates;
        unknown = unknown || o.unknown;
    }
};

bool reads_only_its_arguments(const ASR::Function_t& f);

// Collects the Effects of a statement of a loop body, with everything
// nested in it.
class EffectsCollector : public ASR::BaseWalkVisitor<EffectsCollector> {
public:
    Effects& e;
    // The names of the loops nested in the statement around the statement
    // being visited (nullptr for a loop without a name).
    std::vector<char*> loops;
    // Reallocations whose effects are not collected, or nullptr
    const std::set<const ASR::stmt_t*>* ignored;

    explicit EffectsCollector(Effects& e_,
            const std::set<const ASR::stmt_t*>* ignored_ = nullptr)
        : e(e_), ignored(ignored_) {}

    void modify_value(ASR::expr_t* x) {
        if (x == nullptr) {
            return;
        }
        ASR::symbol_t* root = designator_root(x);
        if (root == nullptr) {
            e.unknown = true;
            return;
        }
        e.value_modified.insert(root);
    }

    void modify_allocation(ASR::expr_t* x) {
        if (x == nullptr) {
            return;
        }
        ASR::symbol_t* root = designator_root(x);
        if (root == nullptr) {
            e.unknown = true;
            return;
        }
        e.alloc_modified.insert(root);
        e.value_modified.insert(root);
    }

    void deallocate(ASR::expr_t* x) {
        modify_allocation(x);
        ASR::symbol_t* root = x == nullptr ? nullptr : designator_root(x);
        if (root != nullptr) {
            e.deallocated.insert(root);
        }
    }

    // Every variable in `x` may be changed in any way, as by a procedure
    // that takes `x` as an actual argument.
    void clobber(ASR::expr_t* x) {
        SymbolSet vars;
        collect_vars(x, vars);
        e.alloc_modified.insert(vars.begin(), vars.end());
        e.deallocated.insert(vars.begin(), vars.end());
        e.value_modified.insert(vars.begin(), vars.end());
    }

    void visit_stmt(const ASR::stmt_t& x) {
        switch (x.type) {
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc:
            case ASR::stmtType::Assignment:
            case ASR::stmtType::DebugCheckArrayBounds:
            case ASR::stmtType::Associate:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate:
            case ASR::stmtType::DoLoop:
            case ASR::stmtType::ErrorStop:
            case ASR::stmtType::Exit:
            case ASR::stmtType::If:
            case ASR::stmtType::Print:
            case ASR::stmtType::FileRead:
            case ASR::stmtType::FileWrite:
            case ASR::stmtType::Return:
            case ASR::stmtType::Select:
            case ASR::stmtType::Stop:
            case ASR::stmtType::Assert:
            case ASR::stmtType::SubroutineCall:
            case ASR::stmtType::IntrinsicImpureSubroutine:
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::Nullify:
            case ASR::stmtType::BlockCall:
                break;
            default:
                e.unknown = true;
                return;
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_stmt(x);
    }

    void visit_Var(const ASR::Var_t& x) {
        e.referenced.insert(past_external(x.m_v));
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        // An element or a section of an array is assigned in place; a whole
        // variable or component may be reallocated. Assigning an element of
        // a derived type reallocates its allocatable components.
        if ((ASR::is_a<ASR::ArrayItem_t>(*x.m_target) ||
                ASR::is_a<ASR::ArraySection_t>(*x.m_target) ||
                ASR::is_a<ASR::StringItem_t>(*x.m_target) ||
                ASR::is_a<ASR::StringSection_t>(*x.m_target)) &&
                !is_derived_type(x.m_target)) {
            modify_value(x.m_target);
        } else if (is_derived_type(x.m_target)) {
            // The allocatable components of a derived type take the
            // allocation of those of the value.
            deallocate(x.m_target);
        } else {
            modify_allocation(x.m_target);
        }
        if (x.m_move_allocation) {
            deallocate(x.m_value);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t& x) {
        deallocate(x.m_target);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Associate(x);
    }

    void visit_Allocate(const ASR::Allocate_t& x) {
        for (size_t i = 0; i < x.n_args; i++) {
            modify_allocation(x.m_args[i].m_a);
        }
        modify_value(x.m_stat);
        modify_value(x.m_errmsg);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Allocate(x);
    }

    void visit_ReAlloc(const ASR::ReAlloc_t& x) {
        if (ignored != nullptr && ignored->count(&x.base) > 0) {
            return;
        }
        for (size_t i = 0; i < x.n_args; i++) {
            modify_allocation(x.m_args[i].m_a);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ReAlloc(x);
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            deallocate(x.m_vars[i]);
        }
        modify_value(x.m_stat);
        modify_value(x.m_errmsg);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ExplicitDeallocate(x);
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            deallocate(x.m_vars[i]);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ImplicitDeallocate(x);
    }

    void visit_Nullify(const ASR::Nullify_t& x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            deallocate(x.m_vars[i]);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Nullify(x);
    }

    // The effects of a call of `name` with `args` on its actual arguments.
    // Through a dummy argument that is neither allocatable nor a pointer, a
    // procedure can change the value of the actual argument but not its
    // allocation, except for the allocatable components of a derived type,
    // which are part of its value; the variables of an actual argument that
    // is not a variable are only read.
    void pass_arguments(ASR::symbol_t* name, ASR::call_arg_t* args,
            size_t n_args, ASR::expr_t* dt) {
        ASR::Function_t* callee = nullptr;
        ASR::symbol_t* s = past_external(name);
        if (dt == nullptr && ASR::is_a<ASR::Function_t>(*s)) {
            callee = ASR::down_cast<ASR::Function_t>(s);
        }
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t* arg = args[i].m_value;
            if (arg == nullptr) {
                continue;
            }
            if (callee == nullptr || i >= callee->n_args ||
                    ASRUtils::is_allocatable(callee->m_args[i]) ||
                    ASRUtils::is_pointer(ASRUtils::expr_type(callee->m_args[i]))) {
                clobber(arg);
            } else if (is_intent_in(callee->m_args[i])) {
                // An INTENT(IN) dummy argument that is neither allocatable
                // nor a pointer is not changed (F2018 8.5.10).
            } else if (is_derived_type(arg)) {
                clobber(arg);
            } else if (designator_root(arg) != nullptr) {
                modify_value(arg);
            }
        }
        clobber(dt);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        e.calls = true;
        e.reads_through_call = true;
        pass_arguments(x.m_name, x.m_args, x.n_args, x.m_dt);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        // A function that computes its result from its arguments alone, as
        // the implementations of intrinsic functions such as `mod` do, reads
        // only its arguments and changes nothing.
        ASR::symbol_t* f = past_external(x.m_name);
        if (x.m_dt == nullptr && ASR::is_a<ASR::Function_t>(*f) &&
                reads_only_its_arguments(*ASR::down_cast<ASR::Function_t>(f))) {
            ASR::BaseWalkVisitor<EffectsCollector>::visit_FunctionCall(x);
            return;
        }
        e.reads_through_call = true;
        // A pure function changes none of its arguments (F2018 C1590) and
        // nothing else it can reach (C1594).
        if (!is_pure_function(x.m_name)) {
            e.calls = true;
            pass_arguments(x.m_name, x.m_args, x.n_args, x.m_dt);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_FunctionCall(x);
    }

    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t& x) {
        if (x.m_impure_intrinsic_id != static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated)) {
            for (size_t i = 0; i < x.n_args; i++) {
                clobber(x.m_args[i]);
            }
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_IntrinsicImpureFunction(x);
    }

    void visit_IntrinsicImpureSubroutine(const ASR::IntrinsicImpureSubroutine_t& x) {
        for (size_t i = 0; i < x.n_args; i++) {
            clobber(x.m_args[i]);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_IntrinsicImpureSubroutine(x);
    }

    // A defined input/output procedure is a call.
    void note_transfer_of_derived_type(const ASR::stmt_t& x) {
        DerivedTypeFinder f;
        f.visit_stmt(x);
        if (f.found) {
            e.calls = true;
            e.reads_through_call = true;
        }
    }

    void visit_Print(const ASR::Print_t& x) {
        note_transfer_of_derived_type(x.base);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Print(x);
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        note_transfer_of_derived_type(x.base);
        // An internal file is written.
        if (x.m_unit != nullptr && designator_root(x.m_unit) != nullptr) {
            modify_value(x.m_unit);
        }
        modify_value(x.m_iostat);
        modify_value(x.m_iomsg);
        modify_value(x.m_id);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_FileWrite(x);
    }

    void visit_FileRead(const ASR::FileRead_t& x) {
        note_transfer_of_derived_type(x.base);
        SymbolSet vars;
        VarCollector c(vars);
        c.visit_stmt(x.base);
        e.alloc_modified.insert(vars.begin(), vars.end());
        e.value_modified.insert(vars.begin(), vars.end());
        ASR::BaseWalkVisitor<EffectsCollector>::visit_FileRead(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t& x) {
        ASR::symbol_t* m = past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*m)) {
            e.unknown = true;
            return;
        }
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        modify_value(x.m_head.m_v);
        loops.push_back(x.m_name);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_DoLoop(x);
        loops.pop_back();
    }

    void visit_WhileLoop(const ASR::WhileLoop_t& x) {
        loops.push_back(x.m_name);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_WhileLoop(x);
        loops.pop_back();
    }

    // Whether an EXIT or CYCLE of the loop named `name` (of the innermost
    // loop for nullptr) belongs to a loop nested in the statement.
    bool is_nested_loop(const char* name) {
        if (name == nullptr) {
            return !loops.empty();
        }
        for (char* loop : loops) {
            if (loop != nullptr && std::strcmp(loop, name) == 0) {
                return true;
            }
        }
        return false;
    }

    void visit_Exit(const ASR::Exit_t& x) {
        if (!is_nested_loop(x.m_stmt_name)) {
            e.escapes = true;
        }
    }

    void visit_Cycle(const ASR::Cycle_t& x) {
        if (!is_nested_loop(x.m_stmt_name)) {
            e.escapes = true;
        }
    }

    void visit_Return(const ASR::Return_t& /*x*/) {
        e.escapes = true;
    }

    void visit_Stop(const ASR::Stop_t& x) {
        e.terminates = true;
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Stop(x);
    }

    void visit_ErrorStop(const ASR::ErrorStop_t& x) {
        e.terminates = true;
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ErrorStop(x);
    }

    void visit_Assert(const ASR::Assert_t& x) {
        e.terminates = true;
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Assert(x);
    }
};

// Whether the function `f` computes its result from the values of its
// arguments alone: its body references only its own variables (dummy
// arguments, locals and result), changes no dummy argument, calls no
// procedure and does no input/output. Such a function gives the same result
// for the same argument values wherever it is called, and changes nothing
// else.
class SelfContainedChecker : public ASR::BaseWalkVisitor<SelfContainedChecker> {
public:
    const ASR::Function_t& f;
    bool ok = true;

    explicit SelfContainedChecker(const ASR::Function_t& f_) : f(f_) {}

    bool is_own_local(ASR::symbol_t* s) {
        s = past_external(s);
        if (!ASR::is_a<ASR::Variable_t>(*s)) {
            return false;
        }
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(s);
        return v->m_parent_symtab == f.m_symtab &&
            (v->m_intent == ASR::intentType::Local ||
             v->m_intent == ASR::intentType::ReturnVar);
    }

    void visit_stmt(const ASR::stmt_t& x) {
        switch (x.type) {
            case ASR::stmtType::Assignment:
            case ASR::stmtType::DoLoop:
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::If:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::Return:
                break;
            default:
                ok = false;
                return;
        }
        ASR::BaseWalkVisitor<SelfContainedChecker>::visit_stmt(x);
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        ASR::symbol_t* root = designator_root(x.m_target);
        if (root == nullptr || !is_own_local(root) || x.m_move_allocation ||
                x.m_overloaded != nullptr) {
            ok = false;
            return;
        }
        ASR::BaseWalkVisitor<SelfContainedChecker>::visit_Assignment(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        if (x.m_head.m_v == nullptr || designator_root(x.m_head.m_v) == nullptr ||
                !is_own_local(designator_root(x.m_head.m_v))) {
            ok = false;
            return;
        }
        ASR::BaseWalkVisitor<SelfContainedChecker>::visit_DoLoop(x);
    }

    void visit_Var(const ASR::Var_t& x) {
        ASR::symbol_t* s = past_external(x.m_v);
        if (!ASR::is_a<ASR::Variable_t>(*s) ||
                ASR::down_cast<ASR::Variable_t>(s)->m_parent_symtab != f.m_symtab) {
            ok = false;
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& /*x*/) {
        ok = false;
    }

    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t& /*x*/) {
        ok = false;
    }
};

bool reads_only_its_arguments(const ASR::Function_t& f) {
    if (f.m_return_var == nullptr || f.m_symtab == nullptr ||
            ASRUtils::get_FunctionType(f)->m_deftype !=
                ASR::deftypeType::Implementation) {
        return false;
    }
    for (auto& item : f.m_symtab->get_scope()) {
        // An internal procedure or a variable with an initializer, which
        // could name another entity
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
            return false;
        }
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
        if (v->m_symbolic_value != nullptr || v->m_value != nullptr ||
                v->m_storage == ASR::storage_typeType::Save ||
                ASRUtils::is_pointer(v->m_type) || v->m_target_attr) {
            return false;
        }
    }
    SelfContainedChecker c(f);
    for (size_t i = 0; i < f.n_body && c.ok; i++) {
        c.visit_stmt(*f.m_body[i]);
    }
    return c.ok;
}

// What the head of a loop (its bounds, or the condition of a DO WHILE loop)
// reads: the variables whose shape or allocation as a whole it inquires
// about, and those whose value or parts it reads. Reallocating a component
// of a variable changes neither the shape nor the allocation of the variable
// as a whole.
struct HeadReads {
    SymbolSet whole;
    SymbolSet parts;
};

// The variables an expression of a hoisted statement reads: those whose
// allocation, shape or bounds only it inquires about, and those whose value
// it reads.
class SourceCollector : public ASR::BaseWalkVisitor<SourceCollector> {
public:
    SymbolSet inquired;
    // The variables whose shape or allocation as a whole an inquiry reads
    SymbolSet inquired_whole;
    // The variables whose components an inquiry reads
    SymbolSet inquired_parts;
    SymbolSet valued;
    bool calls = false;

    // What a loop head that this collected reads
    HeadReads head_reads() const {
        HeadReads r;
        r.whole = inquired_whole;
        r.parts = inquired_parts;
        r.parts.insert(valued.begin(), valued.end());
        return r;
    }

    // An inquiry about the allocation, shape or bounds of a variable or of
    // a component that only it reaches (see component_root) reads only the
    // allocation of a part of that variable.
    bool inquire(ASR::expr_t* v) {
        std::vector<ASR::expr_t*> subscripts;
        ASR::symbol_t* root = component_root(v, &subscripts);
        if (root != nullptr) {
            inquired.insert(root);
            if (ASR::is_a<ASR::Var_t>(*v)) {
                inquired_whole.insert(root);
            } else {
                inquired_parts.insert(root);
            }
            for (ASR::expr_t* subscript : subscripts) {
                visit_expr(*subscript);
            }
            return true;
        }
        return false;
    }

    void visit_Var(const ASR::Var_t& x) {
        valued.insert(past_external(x.m_v));
    }

    void visit_ArraySize(const ASR::ArraySize_t& x) {
        if (!inquire(x.m_v)) {
            visit_expr(*x.m_v);
        }
        if (x.m_dim) {
            visit_expr(*x.m_dim);
        }
    }

    void visit_ArrayBound(const ASR::ArrayBound_t& x) {
        if (!inquire(x.m_v)) {
            visit_expr(*x.m_v);
        }
        if (x.m_dim) {
            visit_expr(*x.m_dim);
        }
    }

    void visit_StringLen(const ASR::StringLen_t& x) {
        if (!inquire(x.m_arg)) {
            visit_expr(*x.m_arg);
        }
    }

    // A function that computes its result from its arguments alone, such
    // as the `count` of the size of `pack(a, m)`, reads only the values of
    // its actual arguments.
    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        ASR::symbol_t* f = past_external(x.m_name);
        if (x.m_dt != nullptr || !ASR::is_a<ASR::Function_t>(*f) ||
                !reads_only_its_arguments(*ASR::down_cast<ASR::Function_t>(f))) {
            calls = true;
            return;
        }
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value != nullptr) {
                visit_expr(*x.m_args[i].m_value);
            }
        }
    }

    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t& x) {
        if (x.m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) &&
                x.n_args == 1 && inquire(x.m_args[0])) {
            return;
        }
        calls = true;
    }
};

// The allocatable designators whose shape or bounds an expression inquires
// about, which must be allocated for the expression to be evaluated; `ok` is
// false when it inquires about a pointer, which may be disassociated.
class InquiryCollector : public ASR::BaseWalkVisitor<InquiryCollector> {
public:
    std::vector<ASR::expr_t*> designators;
    bool ok = true;

    void inquire(ASR::expr_t* v) {
        v = ASRUtils::get_past_array_physical_cast(v);
        ASR::ttype_t* type = ASRUtils::expr_type(v);
        if (ASRUtils::is_pointer(type)) {
            ok = false;
        } else if (ASRUtils::is_allocatable(type)) {
            designators.push_back(v);
        } else {
            visit_expr(*v);
        }
    }

    void visit_ArraySize(const ASR::ArraySize_t& x) {
        inquire(x.m_v);
        if (x.m_dim) {
            visit_expr(*x.m_dim);
        }
    }

    void visit_ArrayBound(const ASR::ArrayBound_t& x) {
        inquire(x.m_v);
        if (x.m_dim) {
            visit_expr(*x.m_dim);
        }
    }
};

// A statement that can move before its loop: a ReAlloc, or an IF without
// ELSE whose body is such statements, with what it reallocates and reads.
struct Hoistable {
    SymbolSet targets;
    // The targets that are reallocated as whole variables, not components
    SymbolSet whole_targets;
    SymbolSet inquired;
    SymbolSet valued;
    // An allocation of an unallocated variable, `if (.not. allocated(v))
    // allocate(v(n))`, which does nothing once `v` is allocated, whatever
    // shape an assignment or a reallocation gives it then: only a
    // deallocation lets it run again.
    bool allocates_once = false;
};


bool is_reallocatable_element_type(ASR::ttype_t* type) {
    ASR::ttype_t* element = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(type));
    switch (element->type) {
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::UnsignedInteger:
        case ASR::ttypeType::Real:
        case ASR::ttypeType::Complex:
        case ASR::ttypeType::Logical:
        case ASR::ttypeType::String:
            return true;
        default:
            return false;
    }
}

// Whether `x` is `if (.not. allocated(v)) allocate(v(dims))` for an
// allocatable array variable `v`, and what it allocates and reads.
bool analyze_allocation_once(const ASR::If_t& x, Hoistable& h) {
    if (x.n_orelse != 0 || x.n_body != 1 ||
            !ASR::is_a<ASR::LogicalNot_t>(*x.m_test) ||
            !ASR::is_a<ASR::Allocate_t>(*x.m_body[0])) {
        return false;
    }
    ASR::expr_t* test = ASR::down_cast<ASR::LogicalNot_t>(x.m_test)->m_arg;
    if (!ASR::is_a<ASR::IntrinsicImpureFunction_t>(*test)) {
        return false;
    }
    ASR::IntrinsicImpureFunction_t* allocated =
        ASR::down_cast<ASR::IntrinsicImpureFunction_t>(test);
    ASR::Allocate_t* a = ASR::down_cast<ASR::Allocate_t>(x.m_body[0]);
    if (allocated->m_impure_intrinsic_id != static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) ||
            allocated->n_args != 1 || a->n_args != 1 || a->m_stat != nullptr ||
            a->m_errmsg != nullptr || a->m_source != nullptr) {
        return false;
    }
    const ASR::alloc_arg_t& arg = a->m_args[0];
    ASR::expr_t* v = ASRUtils::get_past_array_physical_cast(allocated->m_args[0]);
    if (arg.m_type != nullptr || arg.m_sym_subclass != nullptr ||
            arg.n_codims != 0 || !ASR::is_a<ASR::Var_t>(*v) ||
            !ASR::is_a<ASR::Var_t>(*arg.m_a) ||
            past_external(ASR::down_cast<ASR::Var_t>(v)->m_v) !=
                past_external(ASR::down_cast<ASR::Var_t>(arg.m_a)->m_v)) {
        return false;
    }
    ASR::symbol_t* target = past_external(ASR::down_cast<ASR::Var_t>(v)->m_v);
    ASR::ttype_t* type = ASRUtils::expr_type(arg.m_a);
    if (!ASR::is_a<ASR::Variable_t>(*target) ||
            !ASRUtils::is_allocatable(type) || !ASRUtils::is_array(type) ||
            !is_reallocatable_element_type(type)) {
        return false;
    }
    SourceCollector c;
    for (size_t i = 0; i < arg.n_dims; i++) {
        if (arg.m_dims[i].m_start) {
            c.visit_expr(*arg.m_dims[i].m_start);
        }
        if (arg.m_dims[i].m_length) {
            c.visit_expr(*arg.m_dims[i].m_length);
        }
    }
    if (arg.m_len_expr) {
        c.visit_expr(*arg.m_len_expr);
    }
    if (c.calls || c.valued.count(target) > 0 || c.inquired.count(target) > 0) {
        return false;
    }
    h.targets.insert(target);
    h.whole_targets.insert(target);
    h.inquired = c.inquired;
    h.valued = c.valued;
    h.allocates_once = true;
    return true;
}

bool analyze_hoistable(ASR::stmt_t* s, Hoistable& h) {
    if (ASR::is_a<ASR::ReAlloc_t>(*s)) {
        ASR::ReAlloc_t* r = ASR::down_cast<ASR::ReAlloc_t>(s);
        if (r->n_args != 1) {
            return false;
        }
        const ASR::alloc_arg_t& arg = r->m_args[0];
        if (arg.m_type != nullptr || arg.m_sym_subclass != nullptr ||
                arg.n_codims != 0) {
            return false;
        }
        // The target is a variable or a component that only it reaches,
        // which stands for it below, with subscripts that the analysis below
        // takes as read.
        std::vector<ASR::expr_t*> target_subscripts;
        ASR::symbol_t* target = component_root(arg.m_a, &target_subscripts);
        if (target == nullptr) {
            return false;
        }
        ASR::ttype_t* type = ASRUtils::expr_type(arg.m_a);
        if (!ASR::is_a<ASR::Variable_t>(*target) ||
                !ASRUtils::is_allocatable(type) || !ASRUtils::is_array(type) ||
                !is_reallocatable_element_type(type)) {
            return false;
        }
        SourceCollector c;
        for (ASR::expr_t* subscript : target_subscripts) {
            c.visit_expr(*subscript);
        }
        for (size_t i = 0; i < arg.n_dims; i++) {
            if (arg.m_dims[i].m_start) {
                c.visit_expr(*arg.m_dims[i].m_start);
            }
            if (arg.m_dims[i].m_length) {
                c.visit_expr(*arg.m_dims[i].m_length);
            }
        }
        if (arg.m_len_expr) {
            c.visit_expr(*arg.m_len_expr);
        }
        c.visit_expr(*r->m_shape_differs);
        if (c.calls || c.valued.count(target) > 0) {
            return false;
        }
        c.inquired.erase(target);
        h.targets.insert(target);
        if (ASR::is_a<ASR::Var_t>(*arg.m_a)) {
            h.whole_targets.insert(target);
        }
        h.inquired = c.inquired;
        h.valued = c.valued;
        return true;
    }
    if (ASR::is_a<ASR::If_t>(*s) && analyze_allocation_once(
            *ASR::down_cast<ASR::If_t>(s), h)) {
        return true;
    }
    if (ASR::is_a<ASR::If_t>(*s)) {
        ASR::If_t* x = ASR::down_cast<ASR::If_t>(s);
        if (x->n_orelse != 0 || x->n_body == 0) {
            return false;
        }
        SourceCollector c;
        c.visit_expr(*x->m_test);
        if (c.calls) {
            return false;
        }
        std::vector<Hoistable> parts(x->n_body);
        for (size_t i = 0; i < x->n_body; i++) {
            if (!analyze_hoistable(x->m_body[i], parts[i])) {
                return false;
            }
            for (ASR::symbol_t* t : parts[i].targets) {
                if (!h.targets.insert(t).second) {
                    return false;
                }
            }
            h.whole_targets.insert(parts[i].whole_targets.begin(),
                parts[i].whole_targets.end());
        }
        // A statement of the body must not read what another one
        // reallocates.
        for (const Hoistable& part : parts) {
            if (intersects(part.inquired, h.targets) ||
                    intersects(part.valued, h.targets)) {
                return false;
            }
            h.inquired.insert(part.inquired.begin(), part.inquired.end());
            h.valued.insert(part.valued.begin(), part.valued.end());
        }
        if (intersects(c.inquired, h.targets) || intersects(c.valued, h.targets)) {
            return false;
        }
        h.inquired.insert(c.inquired.begin(), c.inquired.end());
        h.valued.insert(c.valued.begin(), c.valued.end());
        return true;
    }
    return false;
}

// Pointers that a statement of a loop body associates with an array section
// (`p => a(l:u, j)`), with no other statement of the body changing their
// association. Array expressions over sections refer to such temporaries,
// and the reallocation of the target of the assignment reads its shape from
// them: `size(p, 1)`.
using SectionPointers = std::map<ASR::symbol_t*, ASR::ArraySection_t*>;

// Replaces the inquiries of an expression about the extents and bounds of
// array sections, or of section pointers, by the extents of the sections,
// computed from their bounds and strides: the subscripts of a section, such
// as `j` above, do not take part in its shape. `failed` is set when anything
// else names such a pointer.
class SectionInquiryReplacer
        : public ASR::BaseExprReplacer<SectionInquiryReplacer> {
public:
    Allocator& al;
    const SectionPointers& sections;
    bool failed = false;
    bool changed = false;

    SectionInquiryReplacer(Allocator& al_, const SectionPointers& sections_)
        : al(al_), sections(sections_) {}

    ASR::ArraySection_t* section_of(ASR::expr_t* v) {
        if (ASR::is_a<ASR::ArraySection_t>(*v)) {
            return ASR::down_cast<ASR::ArraySection_t>(v);
        }
        if (!ASR::is_a<ASR::Var_t>(*v)) {
            return nullptr;
        }
        auto it = sections.find(past_external(ASR::down_cast<ASR::Var_t>(v)->m_v));
        return it == sections.end() ? nullptr : it->second;
    }

    // The range of the section `s` that is dimension `dim` of a pointer to
    // it, with a constant nonzero stride, or nullptr.
    const ASR::array_index_t* range(ASR::ArraySection_t* s, ASR::expr_t* dim,
            int64_t& stride) {
        int64_t d;
        if (dim == nullptr || !ASRUtils::is_value_constant(ASRUtils::expr_value(dim), d)) {
            return nullptr;
        }
        int64_t n = 0;
        for (size_t i = 0; i < s->n_args; i++) {
            const ASR::array_index_t& index = s->m_args[i];
            if (index.m_step == nullptr) {
                continue;
            }
            if (++n == d) {
                if (index.m_left == nullptr || index.m_right == nullptr ||
                        !ASRUtils::is_value_constant(
                            ASRUtils::expr_value(index.m_step), stride) ||
                        stride == 0) {
                    return nullptr;
                }
                return &index;
            }
        }
        return nullptr;
    }

    ASR::expr_t* as_type(ASRUtils::ASRBuilder& b, ASR::expr_t* e, ASR::ttype_t* type) {
        if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)) ==
                ASRUtils::extract_kind_from_ttype_t(type)) {
            return e;
        }
        return b.i2i_t(e, type);
    }

    // (u - l + s) / s, of type `type`
    ASR::expr_t* count(const ASR::array_index_t& index, int64_t stride,
            ASR::ttype_t* type, const Location& loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASRUtils::ExprStmtDuplicator d(al);
        ASR::expr_t* l = as_type(b, d.duplicate_expr(index.m_left), type);
        ASR::expr_t* u = as_type(b, d.duplicate_expr(index.m_right), type);
        ASR::expr_t* n = b.Add(b.Sub(u, l), b.i_t(stride, type));
        return stride == 1 ? n : b.Div(n, b.i_t(stride, type));
    }

    // max(0, (u - l + s) / s): the extent of the range
    ASR::expr_t* extent(const ASR::array_index_t& index, int64_t stride,
            ASR::ttype_t* type, const Location& loc) {
        ASRUtils::ASRBuilder b(al, loc);
        return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
            b.Gt(count(index, stride, type, loc), b.i_t(0, type)),
            count(index, stride, type, loc), b.i_t(0, type), type, nullptr));
    }

    void replace_ArraySize(ASR::ArraySize_t* x) {
        ASR::ArraySection_t* s = section_of(x->m_v);
        if (s == nullptr) {
            ASR::BaseExprReplacer<SectionInquiryReplacer>::replace_ArraySize(x);
            return;
        }
        int64_t stride;
        const ASR::array_index_t* index = range(s, x->m_dim, stride);
        if (index == nullptr) {
            failed = true;
            return;
        }
        *current_expr = extent(*index, stride, x->m_type, x->base.base.loc);
        changed = true;
    }

    void replace_ArrayBound(ASR::ArrayBound_t* x) {
        ASR::ArraySection_t* s = section_of(x->m_v);
        if (s == nullptr) {
            ASR::BaseExprReplacer<SectionInquiryReplacer>::replace_ArrayBound(x);
            return;
        }
        int64_t stride;
        const ASR::array_index_t* index = range(s, x->m_dim, stride);
        if (index == nullptr) {
            failed = true;
            return;
        }
        // A pointer associated without bounds has lower bounds 1.
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        if (x->m_bound == ASR::arrayboundType::LBound) {
            *current_expr = b.i_t(1, x->m_type);
        } else {
            *current_expr = extent(*index, stride, x->m_type, x->base.base.loc);
        }
        changed = true;
    }

    void replace_Var(ASR::Var_t* x) {
        if (section_of(&x->base) != nullptr) {
            failed = true;
        }
    }
};

// A copy of the ReAlloc `s` that computes the shapes of the array sections
// and of the section pointers `sections` it reads from the bounds of the
// sections, or nullptr when it reads none or cannot.
ASR::stmt_t* resolve_sections(Allocator& al, ASR::stmt_t* s,
        const SectionPointers& sections) {
    if (!ASR::is_a<ASR::ReAlloc_t>(*s)) {
        return nullptr;
    }
    ASRUtils::ExprStmtDuplicator d(al);
    ASR::ReAlloc_t* r = ASR::down_cast<ASR::ReAlloc_t>(d.duplicate_stmt(s));
    SectionInquiryReplacer replacer(al, sections);
    auto replace = [&](ASR::expr_t*& e) {
        if (e != nullptr) {
            replacer.current_expr = &e;
            replacer.replace_expr(e);
        }
    };
    for (size_t i = 0; i < r->n_args; i++) {
        for (size_t j = 0; j < r->m_args[i].n_dims; j++) {
            replace(r->m_args[i].m_dims[j].m_start);
            replace(r->m_args[i].m_dims[j].m_length);
        }
        replace(r->m_args[i].m_len_expr);
    }
    replace(r->m_shape_differs);
    return replacer.failed || !replacer.changed ? nullptr : &r->base;
}

// The variables of a procedure that something can reach by another name:
// the targets of pointer assignments, variables referenced by internal
// procedures and the selectors of ASSOCIATE constructs.
class AliasCollector : public ASR::BaseWalkVisitor<AliasCollector> {
public:
    SymbolSet& aliased;
    bool in_associate_block = false;

    explicit AliasCollector(SymbolSet& aliased_) : aliased(aliased_) {}

    // The variable a designator is part of; the subscripts of a section
    // are only read.
    void alias(ASR::expr_t* x) {
        ASR::symbol_t* root = designator_root(x);
        if (root != nullptr) {
            aliased.insert(root);
        } else {
            collect_vars(x, aliased);
        }
    }

    void visit_Associate(const ASR::Associate_t& x) {
        alias(x.m_value);
        ASR::BaseWalkVisitor<AliasCollector>::visit_Associate(x);
    }

    void visit_BindReference(const ASR::BindReference_t& x) {
        alias(x.m_variable);
        ASR::BaseWalkVisitor<AliasCollector>::visit_BindReference(x);
    }

    void visit_GetPointer(const ASR::GetPointer_t& x) {
        alias(x.m_arg);
        ASR::BaseWalkVisitor<AliasCollector>::visit_GetPointer(x);
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t& x) {
        bool saved = in_associate_block;
        in_associate_block = true;
        ASR::BaseWalkVisitor<AliasCollector>::visit_AssociateBlock(x);
        in_associate_block = saved;
    }

    void visit_Variable(const ASR::Variable_t& x) {
        if (in_associate_block) {
            collect_vars(x.m_symbolic_value, aliased);
            collect_vars(x.m_value, aliased);
        }
        ASR::BaseWalkVisitor<AliasCollector>::visit_Variable(x);
    }
};

// The procedure or main program whose body is being transformed.
struct Scope {
    SymbolTable* symtab = nullptr;
    bool is_program = false;
    bool has_internal_procedures = false;
    // The names of the variables that its internal procedures, or theirs,
    // reach by host association: nested_vars has made them their
    // HostAssociated variables
    std::set<std::string> reached;
    SymbolSet aliased;
};

static void collect_reached(SymbolTable* t, std::set<std::string>& reached) {
    for (auto& item : t->get_scope()) {
        if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
        ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(item.second);
        for (auto& v : f->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*v.second) &&
                    ASR::down_cast<ASR::Variable_t>(v.second)->m_storage ==
                        ASR::storage_typeType::HostAssociated) {
                reached.insert(v.first);
            }
        }
        collect_reached(f->m_symtab, reached);
    }
}

class HoistReAllocVisitor : public PassUtils::PassVisitor<HoistReAllocVisitor> {
public:
    Scope scope;

    explicit HoistReAllocVisitor(Allocator& al_)
        : PassUtils::PassVisitor<HoistReAllocVisitor>(al_, nullptr) {}

    template <typename T>
    void enter_scope(const T& x, bool is_program) {
        scope = Scope();
        scope.symtab = x.m_symtab;
        scope.is_program = is_program;
        for (auto& item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                scope.has_internal_procedures = true;
            }
        }
        if (scope.has_internal_procedures) {
            collect_reached(x.m_symtab, scope.reached);
        }
        AliasCollector c(scope.aliased);
        c.visit_symbol(x.base);
    }

    void visit_Program(const ASR::Program_t& x) {
        Scope saved = scope;
        enter_scope(x, true);
        PassUtils::PassVisitor<HoistReAllocVisitor>::visit_Program(x);
        scope = saved;
    }

    void visit_Function(const ASR::Function_t& x) {
        if (ASRUtils::runs_on_device(x)) {
            return;
        }
        Scope saved = scope;
        enter_scope(x, false);
        PassUtils::PassVisitor<HoistReAllocVisitor>::visit_Function(x);
        scope = saved;
    }

    // A parallel loop may run its iterations in any order or at once.
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t& /*x*/) {}
    void visit_OMPRegion(const ASR::OMPRegion_t& /*x*/) {}

    // A variable of the scope or of a BLOCK in it, whose allocation no
    // procedure can change unless it is an actual argument of the call, or
    // it is one an internal procedure of the scope names, which a call of
    // it, also through a procedure passed down, can change.
    // A procedure can change its value through a pointer only when it is
    // also is_aliased.
    bool is_call_safe(ASR::symbol_t* s) {
        if (!ASR::is_a<ASR::Variable_t>(*s) ||
                scope.reached.count(ASRUtils::symbol_name(s)) > 0) {
            return false;
        }
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(s);
        switch (v->m_storage) {
            case ASR::storage_typeType::Default:
            case ASR::storage_typeType::Parameter:
                break;
            case ASR::storage_typeType::Save:
                // A procedure can be called again by what it calls, and
                // change its saved variables; a main program cannot.
                if (!scope.is_program) {
                    return false;
                }
                break;
            default:
                return false;
        }
        SymbolTable* st = v->m_parent_symtab;
        while (st != scope.symtab) {
            if (st == nullptr || st->asr_owner == nullptr ||
                    !ASR::is_a<ASR::symbol_t>(*st->asr_owner) ||
                    !ASR::is_a<ASR::Block_t>(
                        *ASR::down_cast<ASR::symbol_t>(st->asr_owner))) {
                return false;
            }
            st = st->parent;
        }
        return true;
    }

    // Whether `s` can be reached by another name.
    bool is_aliased(ASR::symbol_t* s) {
        if (scope.aliased.count(s) > 0) {
            return true;
        }
        if (!ASR::is_a<ASR::Variable_t>(*s)) {
            return true;
        }
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(s);
        return v->m_target_attr || v->m_common_block != nullptr ||
            ASRUtils::is_pointer(v->m_type);
    }

    bool can_hoist(const Hoistable& h, const Effects& others,
            const Effects& before, const HeadReads& head) {
        // What runs before the reallocation in the first iteration must
        // reach it: if it could stop the program instead, as a check that an
        // array is allocated does, the reallocation, done first, could read
        // the shape of an array that is not allocated.
        if (others.unknown || before.escapes || before.terminates ||
                before.reads_through_call ||
                intersects(h.targets, head.parts) ||
                intersects(h.whole_targets, head.whole) ||
                intersects(h.targets, h.allocates_once
                    ? others.deallocated : others.alloc_modified) ||
                intersects(h.inquired, others.alloc_modified) ||
                intersects(h.valued, others.value_modified) ||
                intersects(h.targets, before.referenced)) {
            return false;
        }
        for (ASR::symbol_t* s : h.targets) {
            if (is_aliased(s)) {
                return false;
            }
            if (others.calls && !is_call_safe(s)) {
                return false;
            }
        }
        for (ASR::symbol_t* s : h.valued) {
            if (is_aliased(s) || (others.calls && !is_call_safe(s))) {
                return false;
            }
        }
        for (ASR::symbol_t* s : h.inquired) {
            if (others.calls && !is_call_safe(s)) {
                return false;
            }
        }
        return true;
    }

    // The statements of the body of the DO loop `x` that move before it,
    // removed from its body, or none.
    std::vector<ASR::stmt_t*> hoist_from(ASR::DoLoop_t& x) {
        const ASR::do_loop_head_t& head = x.m_head;
        if (x.n_orelse != 0 || head.m_v == nullptr || head.m_start == nullptr ||
                head.m_end == nullptr) {
            return {};
        }
        ASR::symbol_t* loop_var = designator_root(head.m_v);
        if (loop_var == nullptr) {
            return {};
        }
        SourceCollector head_sources;
        head_sources.visit_expr(*head.m_start);
        head_sources.visit_expr(*head.m_end);
        if (head.m_increment) {
            head_sources.visit_expr(*head.m_increment);
        }
        if (head_sources.calls) {
            return {};
        }
        return hoist_from_body(x.m_body, x.n_body, head_sources.head_reads(),
            loop_var);
    }

    // The statements of the loop body `m_body` that move before the loop,
    // removed from it, or none. `head` is what the head of the loop
    // reads, `loop_var` its loop variable or nullptr.
    std::vector<ASR::stmt_t*> hoist_from_body(ASR::stmt_t**& m_body,
            size_t& n_body, const HeadReads& head,
            ASR::symbol_t* loop_var) {
        std::vector<ASR::stmt_t*> hoisted;
        std::vector<Hoistable> candidates(n_body);
        std::vector<bool> is_candidate(n_body, false);
        bool any = false;
        for (size_t i = 0; i < n_body; i++) {
            is_candidate[i] = analyze_hoistable(m_body[i], candidates[i]);
            any = any || is_candidate[i];
        }
        if (!any) {
            return hoisted;
        }
        std::vector<Effects> effects(n_body);
        for (size_t i = 0; i < n_body; i++) {
            EffectsCollector c(effects[i]);
            c.visit_stmt(*m_body[i]);
        }
        // The section pointers of the body, with the statement that
        // associates each.
        std::map<ASR::symbol_t*, std::pair<size_t, ASR::ArraySection_t*>> sections;
        for (size_t i = 0; i < n_body; i++) {
            if (!ASR::is_a<ASR::Associate_t>(*m_body[i])) {
                continue;
            }
            ASR::Associate_t* a = ASR::down_cast<ASR::Associate_t>(m_body[i]);
            if (ASR::is_a<ASR::Var_t>(*a->m_target) &&
                    ASR::is_a<ASR::ArraySection_t>(*a->m_value) &&
                    a->m_form == ASR::pointer_bounds_formType::NoPointerBounds &&
                    ASRUtils::is_pointer(ASRUtils::expr_type(a->m_target))) {
                ASR::symbol_t* p = designator_root(a->m_target);
                size_t associations = 0;
                for (size_t j = 0; j < n_body; j++) {
                    associations += effects[j].alloc_modified.count(p);
                }
                if (associations == 1) {
                    sections[p] = {i, ASR::down_cast<ASR::ArraySection_t>(a->m_value)};
                }
            }
        }
        std::vector<ASR::stmt_t*> hoistable(m_body, m_body + n_body);
        std::vector<bool> hoist(n_body, false);
        for (size_t k = 0; k < n_body; k++) {
            if (!is_candidate[k]) {
                continue;
            }
            // An earlier statement that moves before the loop runs there
            // before this one, as it does in the first iteration, and is no
            // longer in the body: a reallocation to the shape of an array
            // that an earlier one reallocates, as in `t1 = a + b; t2 = 2*t1`,
            // can move too.
            Effects others, before;
            for (size_t j = 0; j < n_body; j++) {
                if (j == k || (j < k && hoist[j])) {
                    continue;
                }
                others.merge(effects[j]);
                if (j < k) {
                    before.merge(effects[j]);
                }
            }
            if (loop_var != nullptr) {
                others.value_modified.insert(loop_var);
            }
            hoist[k] = can_hoist(candidates[k], others, before, head);
            if (hoist[k]) {
                continue;
            }
            // The shape of a section does not depend on its subscripts, and
            // a section pointer associated before the reallocation in every
            // iteration has the shape of its section there.
            SectionPointers before_k;
            for (auto& section : sections) {
                if (section.second.first < k &&
                        candidates[k].inquired.count(section.first) > 0) {
                    before_k[section.first] = section.second.second;
                }
            }
            ASR::stmt_t* resolved = resolve_sections(al, m_body[k], before_k);
            Hoistable h;
            if (resolved != nullptr && analyze_hoistable(resolved, h) &&
                    can_hoist(h, others, before, head)) {
                hoist[k] = true;
                hoistable[k] = resolved;
            }
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            if (hoist[i]) {
                hoisted.push_back(hoistable[i]);
            } else {
                body.push_back(al, m_body[i]);
            }
        }
        if (!hoisted.empty()) {
            m_body = body.p;
            n_body = body.size();
        }
        return hoisted;
    }

    // How many times at least the loop `x` with the constant step `step`
    // runs, as far as its constant bounds tell: 0, 1, or -1 when its bounds
    // are not constant.
    int known_trips(const ASR::DoLoop_t& x, int64_t step) {
        int64_t start, end;
        if (ASRUtils::is_value_constant(ASRUtils::expr_value(x.m_head.m_start), start) &&
                ASRUtils::is_value_constant(ASRUtils::expr_value(x.m_head.m_end), end)) {
            return (step > 0 ? start <= end : start >= end) ? 1 : 0;
        }
        return -1;
    }

    // The test that the loop `x` with the constant step `step` runs at
    // least once.
    ASR::expr_t* runs_at_least_once(const ASR::DoLoop_t& x, int64_t step) {
        ASRUtils::ExprStmtDuplicator d(al);
        ASRUtils::ASRBuilder b(al, x.base.base.loc);
        ASR::expr_t* start = d.duplicate_expr(x.m_head.m_start);
        ASR::expr_t* end = d.duplicate_expr(x.m_head.m_end);
        return step > 0 ? b.LtE(start, end) : b.GtE(start, end);
    }

    // The variable `s` reallocates with the shape of a variable, `size(v,
    // 1), ..., size(v, n)` for the rank n of both, or nullptr.
    static ASR::symbol_t* copies_shape_of(ASR::ReAlloc_t* r, ASR::symbol_t*& target) {
        if (r->n_args != 1) {
            return nullptr;
        }
        const ASR::alloc_arg_t& arg = r->m_args[0];
        if (!ASR::is_a<ASR::Var_t>(*arg.m_a) || arg.m_type != nullptr ||
                arg.m_sym_subclass != nullptr || arg.n_codims != 0 ||
                arg.m_len_expr != nullptr) {
            return nullptr;
        }
        ASR::ttype_t* type = ASRUtils::expr_type(arg.m_a);
        if (!is_reallocatable_element_type(type) || ASRUtils::is_character(*type) ||
                arg.n_dims != static_cast<size_t>(ASRUtils::extract_n_dims_from_ttype(type))) {
            return nullptr;
        }
        ASR::symbol_t* source = nullptr;
        for (size_t d = 0; d < arg.n_dims; d++) {
            ASR::expr_t* length = arg.m_dims[d].m_length;
            if (length == nullptr || !ASR::is_a<ASR::ArraySize_t>(*length)) {
                return nullptr;
            }
            ASR::ArraySize_t* size = ASR::down_cast<ASR::ArraySize_t>(length);
            int64_t dim;
            if (!ASR::is_a<ASR::Var_t>(*size->m_v) ||
                    !ASRUtils::is_value_constant(size->m_dim, dim) ||
                    dim != static_cast<int64_t>(d) + 1) {
                return nullptr;
            }
            ASR::symbol_t* v = past_external(ASR::down_cast<ASR::Var_t>(size->m_v)->m_v);
            if (source != nullptr && v != source) {
                return nullptr;
            }
            source = v;
            if (static_cast<size_t>(ASRUtils::extract_n_dims_from_ttype(ASRUtils::symbol_type(v))) != arg.n_dims) {
                return nullptr;
            }
        }
        target = past_external(ASR::down_cast<ASR::Var_t>(arg.m_a)->m_v);
        return source;
    }

    // Removes from `body` each reallocation of `v` with the shape of `u`
    // that follows a reallocation of `u` with the shape of `v`, with nothing
    // that can change the allocation of either in between, as in
    //
    //     un = u
    //     un(2:n-1) = ...
    //     u = un
    //
    // The first one gives `u` and `v` the same shape, so the second finds
    // `v` allocated (its shape was read) with the shape of `u`, and does
    // nothing.
    // A ReAlloc whose extents read a scalar that the statement just before
    // it sets to an expression of inquiries and constants, as the extent of
    // a function result is kept, `m = size(a)`, reads that expression
    // instead: it has the same value there, and the ReAlloc then reads no
    // variable that the loop sets, so that it can leave the loop.
    void read_extents_in_place(ASR::stmt_t** body, size_t n_body) {
        class Inquiries : public ASR::BaseWalkVisitor<Inquiries> {
        public:
            bool ok = true;
            ASR::symbol_t* self = nullptr;
            void visit_FunctionCall(const ASR::FunctionCall_t&) { ok = false; }
            void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t&) { ok = false; }
            void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t&) { ok = false; }
            void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t&) { ok = false; }
            void visit_ArrayItem(const ASR::ArrayItem_t&) { ok = false; }
            void visit_Var(const ASR::Var_t& x) {
                if (past_external(x.m_v) == self) ok = false;
            }
        };
        class Substitute : public ASR::BaseExprReplacer<Substitute> {
        public:
            Allocator& al;
            ASR::symbol_t* var;
            ASR::expr_t* value;
            Substitute(Allocator& al_, ASR::symbol_t* var_, ASR::expr_t* value_)
                : al(al_), var(var_), value(value_) {}
            void replace_Var(ASR::Var_t* x) {
                if (past_external(x->m_v) == var) {
                    ASRUtils::ExprStmtDuplicator d(al);
                    *current_expr = d.duplicate_expr(value);
                }
            }
            void apply(ASR::expr_t*& e) {
                if (e == nullptr) return;
                current_expr = &e;
                replace_expr(e);
            }
        };
        for (size_t i = 1; i < n_body; i++) {
            if (!ASR::is_a<ASR::ReAlloc_t>(*body[i]) ||
                    !ASR::is_a<ASR::Assignment_t>(*body[i - 1])) {
                continue;
            }
            ASR::Assignment_t* a = ASR::down_cast<ASR::Assignment_t>(body[i - 1]);
            if (a->m_overloaded != nullptr || !ASR::is_a<ASR::Var_t>(*a->m_target) ||
                    !ASRUtils::is_integer(*ASRUtils::expr_type(a->m_target)) ||
                    ASRUtils::is_array(ASRUtils::expr_type(a->m_target))) {
                continue;
            }
            ASR::symbol_t* var = past_external(
                ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*var) ||
                    ASR::down_cast<ASR::Variable_t>(var)->m_storage !=
                        ASR::storage_typeType::Default ||
                    is_aliased(var)) {
                continue;
            }
            Inquiries inquiries;
            inquiries.self = var;
            inquiries.visit_expr(*a->m_value);
            if (!inquiries.ok) {
                continue;
            }
            ASR::ReAlloc_t* r = ASR::down_cast<ASR::ReAlloc_t>(body[i]);
            Substitute sub(al, var, a->m_value);
            for (size_t k = 0; k < r->n_args; k++) {
                for (size_t d = 0; d < r->m_args[k].n_dims; d++) {
                    sub.apply(r->m_args[k].m_dims[d].m_start);
                    sub.apply(r->m_args[k].m_dims[d].m_length);
                }
                sub.apply(r->m_args[k].m_len_expr);
            }
            sub.apply(r->m_shape_differs);
        }
    }

    void remove_redundant_reallocations(ASR::stmt_t**& body, size_t& n_body) {
        std::vector<bool> redundant(n_body, false);
        bool any = false;
        for (size_t j = 0; j < n_body; j++) {
            if (!ASR::is_a<ASR::ReAlloc_t>(*body[j])) {
                continue;
            }
            ASR::symbol_t* v = nullptr;
            ASR::symbol_t* u = copies_shape_of(
                ASR::down_cast<ASR::ReAlloc_t>(body[j]), v);
            if (u == nullptr) {
                continue;
            }
            Effects between;
            for (size_t i = j; i-- > 0;) {
                if (ASR::is_a<ASR::ReAlloc_t>(*body[i]) && !redundant[i]) {
                    ASR::symbol_t* target = nullptr;
                    ASR::symbol_t* source = copies_shape_of(
                        ASR::down_cast<ASR::ReAlloc_t>(body[i]), target);
                    if (target == u && source == v) {
                        redundant[j] = !between.unknown &&
                            between.alloc_modified.count(u) == 0 &&
                            between.alloc_modified.count(v) == 0 &&
                            (!between.calls || (is_call_safe(u) && is_call_safe(v)));
                        any = any || redundant[j];
                        break;
                    }
                }
                EffectsCollector c(between);
                c.visit_stmt(*body[i]);
            }
        }
        if (!any) {
            return;
        }
        Vec<ASR::stmt_t*> kept;
        kept.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            if (!redundant[i]) {
                kept.push_back(al, body[i]);
            }
        }
        body = kept.p;
        n_body = kept.size();
    }

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        ASR::DoLoop_t& xx = const_cast<ASR::DoLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
        if (scope.symtab == nullptr) {
            return;
        }
        remove_redundant_reallocations(xx.m_body, xx.n_body);
        read_extents_in_place(xx.m_body, xx.n_body);
        int64_t step = 1;
        if (x.m_head.m_increment &&
                (!ASRUtils::is_value_constant(
                    ASRUtils::expr_value(x.m_head.m_increment), step) || step == 0)) {
            return;
        }
        int trips = known_trips(x, step);
        if (trips == 0) {
            return;
        }
        std::vector<ASR::stmt_t*> hoisted = hoist_from(xx);
        std::vector<ASR::stmt_t*> versioned;
        if (x.n_orelse == 0 && x.m_head.m_v != nullptr &&
                designator_root(x.m_head.m_v) != nullptr) {
            versioned = version_loop(&xx.base, xx.m_body, xx.n_body,
                designator_root(x.m_head.m_v));
        }
        if (hoisted.empty() && versioned.empty()) {
            return;
        }
        pass_result.reserve(al, hoisted.size() + versioned.size() + 1);
        if (trips == 1) {
            for (ASR::stmt_t* s : hoisted) {
                pass_result.push_back(al, s);
            }
        } else if (!hoisted.empty()) {
            ASRUtils::ASRBuilder b(al, x.base.base.loc);
            pass_result.push_back(al, b.If(runs_at_least_once(x, step),
                hoisted, {}));
        }
        push_loop(&xx.base, versioned);
    }

    // Appends `versioned` to pass_result, or `loop` when it is empty.
    void push_loop(ASR::stmt_t* loop, const std::vector<ASR::stmt_t*>& versioned) {
        if (versioned.empty()) {
            pass_result.push_back(al, loop);
        }
        for (ASR::stmt_t* s : versioned) {
            pass_result.push_back(al, s);
        }
    }

    // Loop versioning. A reallocation that cannot move before its loop,
    // because the loop may leave or stop before it, because it is
    // conditional, or because its target has the TARGET attribute, still
    // does nothing in any iteration when its target has, before the loop,
    // the shape it gives it, and nothing else in the loop changes the
    // allocation of its target or what its shape is computed from. The loop
    // then runs without it, and as it was otherwise:
    //
    //     if (allocated(c) .and. size(c, 1) == size(a, 1)) then
    //         do i = 1, n  ! without the reallocations
    //     else
    //         do i = 1, n  ! as it was
    //     end if
    //
    // The test is evaluated even when the original loop would not reach the
    // reallocation, so it reads nothing that may not exist: each array it
    // inquires about is tested to be allocated first (and none is a pointer
    // or an optional dummy argument), and it reads the value of no
    // allocatable or pointer variable. A loop is versioned only when its
    // body has no versioned loop, so that the code grows at most twofold.
    std::set<const ASR::stmt_t*> versioned_loops;
    static constexpr size_t max_versioned_statements = 200;

    // Collects the reallocations anywhere in `body` into `out` and counts its
    // statements in `count`; false if it has a statement that may not be
    // duplicated or a versioned loop.
    bool collect_versionable(ASR::stmt_t** body, size_t n_body,
            std::vector<ASR::ReAlloc_t*>& out, size_t& count) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t* s = body[i];
            if (versioned_loops.count(s) > 0 ||
                    ++count > max_versioned_statements) {
                return false;
            }
            switch (s->type) {
                case ASR::stmtType::ReAlloc:
                    out.push_back(ASR::down_cast<ASR::ReAlloc_t>(s));
                    break;
                case ASR::stmtType::If: {
                    ASR::If_t* x = ASR::down_cast<ASR::If_t>(s);
                    if (!collect_versionable(x->m_body, x->n_body, out, count) ||
                            !collect_versionable(x->m_orelse, x->n_orelse, out, count)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t* x = ASR::down_cast<ASR::DoLoop_t>(s);
                    if (x->m_name != nullptr ||
                            !collect_versionable(x->m_body, x->n_body, out, count) ||
                            !collect_versionable(x->m_orelse, x->n_orelse, out, count)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t* x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    if (x->m_name != nullptr ||
                            !collect_versionable(x->m_body, x->n_body, out, count) ||
                            !collect_versionable(x->m_orelse, x->n_orelse, out, count)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::Assignment:
                case ASR::stmtType::SubroutineCall:
                case ASR::stmtType::Exit:
                case ASR::stmtType::Cycle:
                case ASR::stmtType::Return:
                case ASR::stmtType::Stop:
                case ASR::stmtType::ErrorStop:
                case ASR::stmtType::Print:
                case ASR::stmtType::FileWrite:
                case ASR::stmtType::Allocate:
                case ASR::stmtType::ExplicitDeallocate:
                case ASR::stmtType::ImplicitDeallocate:
                case ASR::stmtType::Associate:
                case ASR::stmtType::Nullify:
                case ASR::stmtType::DebugCheckArrayBounds:
                    break;
                default:
                    return false;
            }
        }
        return true;
    }

    // Whether the reallocation `r` reads an element of an array: the test
    // before a versioned loop would read it even when the loop would not,
    // with subscripts that may not be valid there.
    static bool reads_array_element(ASR::ReAlloc_t* r) {
        class ElementFinder : public ASR::BaseWalkVisitor<ElementFinder> {
        public:
            bool found = false;
            void visit_ArrayItem(const ASR::ArrayItem_t& /*x*/) {
                found = true;
            }
        };
        ElementFinder f;
        f.visit_stmt(r->base);
        return f.found;
    }

    // Whether `s` may be an optional dummy argument, which may be absent.
    // transform_optional_argument_functions makes an optional dummy
    // required and adds a scalar logical argument after it, which tells
    // whether it is present.
    bool may_be_optional(ASR::symbol_t* s) {
        if (!ASR::is_a<ASR::Variable_t>(*s)) {
            return true;
        }
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_presence == ASR::presenceType::Optional) {
            return true;
        }
        if (v->m_intent == ASR::intentType::Local ||
                v->m_intent == ASR::intentType::ReturnVar) {
            return false;
        }
        ASR::asr_t* owner = v->m_parent_symtab->asr_owner;
        if (owner == nullptr || !ASR::is_a<ASR::symbol_t>(*owner) ||
                !ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(owner))) {
            return true;
        }
        ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(
            ASR::down_cast<ASR::symbol_t>(owner));
        for (size_t i = 0; i < f->n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*f->m_args[i]) ||
                    ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v != s) {
                continue;
            }
            if (i + 1 == f->n_args) {
                return false;
            }
            ASR::expr_t* next = f->m_args[i + 1];
            return ASR::is_a<ASR::Var_t>(*next) &&
                ASR::is_a<ASR::Logical_t>(*ASRUtils::expr_type(next));
        }
        return true;
    }

    // The statement that sets `ok` to .false. unless the reallocation `r`
    // does nothing, when `ok` is .true. before it: it tests that the target
    // and every array whose shape `r` reads are allocated before it reads
    // their shapes. Or nullptr when it cannot be evaluated before the loop
    // safely.
    ASR::stmt_t* does_nothing_test(ASR::ReAlloc_t* r, ASR::expr_t* ok) {
        const ASR::alloc_arg_t& arg = r->m_args[0];
        ASR::ttype_t* test_type = ASRUtils::expr_type(r->m_shape_differs);
        if (!ASR::is_a<ASR::Logical_t>(*test_type) ||
                ASRUtils::extract_kind_from_ttype_t(test_type) !=
                    ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(ok))) {
            return nullptr;
        }
        InquiryCollector c;
        for (size_t i = 0; i < arg.n_dims; i++) {
            if (arg.m_dims[i].m_start) {
                c.visit_expr(*arg.m_dims[i].m_start);
            }
            if (arg.m_dims[i].m_length) {
                c.visit_expr(*arg.m_dims[i].m_length);
            }
        }
        c.visit_expr(*r->m_shape_differs);
        if (!c.ok) {
            return nullptr;
        }
        std::vector<ASR::expr_t*> guards;
        guards.push_back(arg.m_a);
        for (ASR::expr_t* e : c.designators) {
            bool known = false;
            for (ASR::expr_t* g : guards) {
                known = known || PassUtils::same_expression(g, e);
            }
            if (!known) {
                guards.push_back(e);
            }
        }
        const Location& loc = r->base.base.loc;
        ASRUtils::ExprStmtDuplicator d(al);
        ASRUtils::ASRBuilder b(al, loc);
        ASR::stmt_t* test = b.Assignment(ok,
            b.Not(d.duplicate_expr(r->m_shape_differs)));
        for (size_t i = guards.size(); i-- > 0;) {
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, d.duplicate_expr(guards[i]));
            ASR::expr_t* allocated = ASRUtils::EXPR(
                ASR::make_IntrinsicImpureFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicImpureFunctions::Allocated),
                    args.p, args.n, 0, ASRUtils::expr_type(ok), nullptr));
            test = b.If(allocated, {test}, {});
        }
        return b.If(ok, {b.Assignment(ok, b.bool_t(false,
            ASRUtils::expr_type(ok))), test}, {});
    }

    // Whether the reallocation `h` does nothing in every iteration of a
    // loop whose other statements have the effects `others`, once it does
    // nothing before the loop.
    bool can_version(const Hoistable& h, const Effects& others) {
        if (others.unknown ||
                intersects(h.targets, others.alloc_modified) ||
                intersects(h.inquired, others.alloc_modified) ||
                intersects(h.valued, others.value_modified)) {
            return false;
        }
        for (ASR::symbol_t* s : h.valued) {
            if (is_aliased(s) || may_be_optional(s) ||
                    ASRUtils::is_allocatable(ASRUtils::symbol_type(s)) ||
                    ASRUtils::is_pointer(ASRUtils::symbol_type(s)) ||
                    (others.calls && !is_call_safe(s))) {
                return false;
            }
        }
        for (const SymbolSet* set : {&h.inquired, &h.targets}) {
            for (ASR::symbol_t* s : *set) {
                if (may_be_optional(s) || (others.calls && !is_call_safe(s))) {
                    return false;
                }
            }
        }
        return true;
    }

    // Removes the statements `removed` from `body` and the bodies nested in
    // it.
    void strip(ASR::stmt_t**& body, size_t& n_body,
            const std::set<const ASR::stmt_t*>& removed) {
        Vec<ASR::stmt_t*> kept;
        kept.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t* s = body[i];
            if (removed.count(s) > 0) {
                continue;
            }
            if (ASR::is_a<ASR::If_t>(*s)) {
                ASR::If_t* x = ASR::down_cast<ASR::If_t>(s);
                strip(x->m_body, x->n_body, removed);
                strip(x->m_orelse, x->n_orelse, removed);
            } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                ASR::DoLoop_t* x = ASR::down_cast<ASR::DoLoop_t>(s);
                strip(x->m_body, x->n_body, removed);
                strip(x->m_orelse, x->n_orelse, removed);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*s)) {
                ASR::WhileLoop_t* x = ASR::down_cast<ASR::WhileLoop_t>(s);
                strip(x->m_body, x->n_body, removed);
                strip(x->m_orelse, x->n_orelse, removed);
            }
            kept.push_back(al, s);
        }
        body = kept.p;
        n_body = kept.size();
    }

    // The statements of the loop `loop` with the body `body`, versioned, or
    // none. `loop_var` is its loop variable or nullptr.
    std::vector<ASR::stmt_t*> version_loop(ASR::stmt_t* loop,
            ASR::stmt_t**& body, size_t& n_body, ASR::symbol_t* loop_var) {
        std::vector<ASR::ReAlloc_t*> reallocs;
        size_t count = 0;
        if (!collect_versionable(body, n_body, reallocs, count) ||
                reallocs.empty()) {
            return {};
        }
        const Location& loc = loop->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t* logical_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        std::string ok_name = scope.symtab->get_unique_name(
            "__libasr_reallocations_do_nothing");
        ASR::symbol_t* ok_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope.symtab,
                s2c(al, ok_name), nullptr, 0, ASR::intentType::Local,
                nullptr, nullptr, ASR::storage_typeType::Default,
                logical_type, nullptr, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required, false));
        ASR::expr_t* ok = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ok_sym));
        struct Candidate {
            ASR::ReAlloc_t* r;
            Hoistable h;
            ASR::stmt_t* test;
        };
        std::vector<Candidate> candidates;
        for (ASR::ReAlloc_t* r : reallocs) {
            Candidate c{r, Hoistable(), nullptr};
            if (!analyze_hoistable(&r->base, c.h) ||
                    ASRUtils::is_character(*ASRUtils::expr_type(r->m_args[0].m_a)) ||
                    reads_array_element(r)) {
                continue;
            }
            c.test = does_nothing_test(r, ok);
            if (c.test != nullptr) {
                candidates.push_back(c);
            }
        }
        // A reallocation that stays in the loop is among the effects that
        // the others must not depend on.
        while (!candidates.empty()) {
            std::set<const ASR::stmt_t*> removed;
            for (const Candidate& c : candidates) {
                removed.insert(&c.r->base);
            }
            Effects others;
            for (size_t i = 0; i < n_body; i++) {
                EffectsCollector e(others, &removed);
                e.visit_stmt(*body[i]);
            }
            if (loop_var != nullptr) {
                others.value_modified.insert(loop_var);
            }
            std::vector<Candidate> kept;
            for (const Candidate& c : candidates) {
                if (can_version(c.h, others)) {
                    kept.push_back(c);
                }
            }
            if (kept.size() == candidates.size()) {
                break;
            }
            candidates = kept;
        }
        if (candidates.empty()) {
            return {};
        }
        ASRUtils::ExprStmtDuplicator d(al);
        ASR::stmt_t* fallback = d.duplicate_stmt(loop);
        if (fallback == nullptr) {
            return {};
        }
        scope.symtab->add_symbol(ok_name, ok_sym);
        std::vector<ASR::stmt_t*> result;
        result.push_back(b.Assignment(ok, b.bool_t(true, logical_type)));
        std::set<const ASR::stmt_t*> removed;
        for (const Candidate& c : candidates) {
            result.push_back(c.test);
            removed.insert(&c.r->base);
        }
        strip(body, n_body, removed);
        ASR::stmt_t* versioned = b.If(ok, {loop}, {fallback});
        versioned_loops.insert(versioned);
        result.push_back(versioned);
        return result;
    }

    // A DO WHILE loop runs its body at least once when its condition holds
    // before it. The condition calls no procedure that could change
    // anything (as for the bounds of a DO loop) and does not read what the
    // hoisted statements reallocate, so it can be evaluated once more, in
    // front of them.
    void visit_WhileLoop(const ASR::WhileLoop_t& x) {
        ASR::WhileLoop_t& xx = const_cast<ASR::WhileLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
        if (scope.symtab == nullptr) {
            return;
        }
        remove_redundant_reallocations(xx.m_body, xx.n_body);
        read_extents_in_place(xx.m_body, xx.n_body);
        if (xx.n_orelse != 0) {
            return;
        }
        SourceCollector test_sources;
        test_sources.visit_expr(*x.m_test);
        if (test_sources.calls) {
            return;
        }
        std::vector<ASR::stmt_t*> hoisted = hoist_from_body(xx.m_body,
            xx.n_body, test_sources.head_reads(), nullptr);
        std::vector<ASR::stmt_t*> versioned = version_loop(&xx.base,
            xx.m_body, xx.n_body, nullptr);
        if (hoisted.empty() && versioned.empty()) {
            return;
        }
        ASRUtils::ExprStmtDuplicator d(al);
        ASRUtils::ASRBuilder b(al, x.base.base.loc);
        pass_result.reserve(al, versioned.size() + 2);
        if (!hoisted.empty()) {
            pass_result.push_back(al, b.If(d.duplicate_expr(x.m_test), hoisted, {}));
        }
        push_loop(&xx.base, versioned);
    }
};

} // namespace

void pass_hoist_realloc(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    HoistReAllocVisitor v(al);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
