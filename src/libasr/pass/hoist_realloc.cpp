#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/hoist_realloc.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <cstring>
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
* no statement of the body other than H can change the allocation, shape or
  bounds of an array H reallocates, or the allocation or shape of an array
  whose shape H reads, or any value H reads, and H does not read the loop
  variable: every later execution of H then finds its arrays allocated with
  the shape it would give them, and does nothing;
* a variable that something can reach by another name (TARGET, POINTER,
  EQUIVALENCE, COMMON, the target of a pointer assignment, a variable of an
  enclosing scope) is not one H reallocates, nor one whose value H reads,
  and when the body calls a procedure, every variable H reallocates or reads
  is local to this procedure (or a dummy argument of it), with no internal
  procedure that could reach it: such a variable can be changed by a
  procedure only when it is an actual argument of the call, which the body
  does name (F2018 15.5.2.13);
* the loop has a constant step, its bounds call no procedure and do not
  name an array H reallocates, so that they can be evaluated again for the
  test that the loop runs at all, with the same value.

Every other statement, and every statement this analysis does not know,
keeps H in the loop. Loops are processed innermost first, so that a ReAlloc
that leaves an inner loop can leave an enclosing loop as well.
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
        value_modified.insert(o.value_modified.begin(), o.value_modified.end());
        referenced.insert(o.referenced.begin(), o.referenced.end());
        calls = calls || o.calls;
        reads_through_call = reads_through_call || o.reads_through_call;
        escapes = escapes || o.escapes;
        terminates = terminates || o.terminates;
        unknown = unknown || o.unknown;
    }
};

// Collects the Effects of a statement of a loop body, with everything
// nested in it.
class EffectsCollector : public ASR::BaseWalkVisitor<EffectsCollector> {
public:
    Effects& e;
    // The names of the loops nested in the statement around the statement
    // being visited (nullptr for a loop without a name).
    std::vector<char*> loops;

    explicit EffectsCollector(Effects& e_) : e(e_) {}

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

    // Every variable in `x` may be changed in any way, as by a procedure
    // that takes `x` as an actual argument.
    void clobber(ASR::expr_t* x) {
        SymbolSet vars;
        collect_vars(x, vars);
        e.alloc_modified.insert(vars.begin(), vars.end());
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
        // variable or component may be reallocated.
        if (ASR::is_a<ASR::ArrayItem_t>(*x.m_target) ||
                ASR::is_a<ASR::ArraySection_t>(*x.m_target) ||
                ASR::is_a<ASR::StringItem_t>(*x.m_target) ||
                ASR::is_a<ASR::StringSection_t>(*x.m_target)) {
            modify_value(x.m_target);
        } else {
            modify_allocation(x.m_target);
        }
        if (x.m_move_allocation) {
            modify_allocation(x.m_value);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t& x) {
        modify_allocation(x.m_target);
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
        for (size_t i = 0; i < x.n_args; i++) {
            modify_allocation(x.m_args[i].m_a);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ReAlloc(x);
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            modify_allocation(x.m_vars[i]);
        }
        modify_value(x.m_stat);
        modify_value(x.m_errmsg);
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ExplicitDeallocate(x);
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            modify_allocation(x.m_vars[i]);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_ImplicitDeallocate(x);
    }

    void visit_Nullify(const ASR::Nullify_t& x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            modify_allocation(x.m_vars[i]);
        }
        ASR::BaseWalkVisitor<EffectsCollector>::visit_Nullify(x);
    }

    // The effects of a call of `name` with `args` on its actual arguments.
    // Through a dummy argument that is neither allocatable nor a pointer, a
    // procedure can change the value of the actual argument but not its
    // allocation; the variables of an actual argument that is not a
    // variable are only read.
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

// The variables an expression of a hoisted statement reads: those whose
// allocation, shape or bounds only it inquires about, and those whose value
// it reads.
class SourceCollector : public ASR::BaseWalkVisitor<SourceCollector> {
public:
    SymbolSet inquired;
    SymbolSet valued;
    bool calls = false;

    bool inquire(ASR::expr_t* v) {
        if (ASR::is_a<ASR::Var_t>(*v)) {
            inquired.insert(past_external(ASR::down_cast<ASR::Var_t>(v)->m_v));
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

    void visit_FunctionCall(const ASR::FunctionCall_t& /*x*/) {
        calls = true;
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

// A statement that can move before its loop: a ReAlloc, or an IF without
// ELSE whose body is such statements, with what it reallocates and reads.
struct Hoistable {
    SymbolSet targets;
    SymbolSet inquired;
    SymbolSet valued;
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

bool analyze_hoistable(ASR::stmt_t* s, Hoistable& h) {
    if (ASR::is_a<ASR::ReAlloc_t>(*s)) {
        ASR::ReAlloc_t* r = ASR::down_cast<ASR::ReAlloc_t>(s);
        if (r->n_args != 1) {
            return false;
        }
        const ASR::alloc_arg_t& arg = r->m_args[0];
        if (!ASR::is_a<ASR::Var_t>(*arg.m_a) || arg.m_type != nullptr ||
                arg.m_sym_subclass != nullptr || arg.n_codims != 0) {
            return false;
        }
        ASR::symbol_t* target = past_external(
            ASR::down_cast<ASR::Var_t>(arg.m_a)->m_v);
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
        c.visit_expr(*r->m_shape_differs);
        if (c.calls || c.valued.count(target) > 0) {
            return false;
        }
        c.inquired.erase(target);
        h.targets.insert(target);
        h.inquired = c.inquired;
        h.valued = c.valued;
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
    SymbolSet aliased;
};

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
    // procedure can change unless it is an actual argument of the call.
    // A procedure can change its value through a pointer only when it is
    // also is_aliased.
    bool is_call_safe(ASR::symbol_t* s) {
        if (!ASR::is_a<ASR::Variable_t>(*s) || scope.has_internal_procedures) {
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
            const Effects& before, const SymbolSet& head_vars) {
        // What runs before the reallocation in the first iteration must
        // reach it: if it could stop the program instead, as a check that an
        // array is allocated does, the reallocation, done first, could read
        // the shape of an array that is not allocated.
        if (others.unknown || before.escapes || before.terminates ||
                before.reads_through_call ||
                intersects(h.targets, head_vars) ||
                intersects(h.targets, others.alloc_modified) ||
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

    // The statements of the body of `x` that move before it, removed from
    // its body, or none.
    std::vector<ASR::stmt_t*> hoist_from(ASR::DoLoop_t& x) {
        std::vector<ASR::stmt_t*> hoisted;
        const ASR::do_loop_head_t& head = x.m_head;
        if (x.n_orelse != 0 || head.m_v == nullptr || head.m_start == nullptr ||
                head.m_end == nullptr) {
            return hoisted;
        }
        ASR::symbol_t* loop_var = designator_root(head.m_v);
        if (loop_var == nullptr) {
            return hoisted;
        }
        SourceCollector head_sources;
        head_sources.visit_expr(*head.m_start);
        head_sources.visit_expr(*head.m_end);
        if (head.m_increment) {
            head_sources.visit_expr(*head.m_increment);
        }
        if (head_sources.calls) {
            return hoisted;
        }
        SymbolSet head_vars = head_sources.inquired;
        head_vars.insert(head_sources.valued.begin(), head_sources.valued.end());

        std::vector<Hoistable> candidates(x.n_body);
        std::vector<bool> is_candidate(x.n_body, false);
        bool any = false;
        for (size_t i = 0; i < x.n_body; i++) {
            is_candidate[i] = analyze_hoistable(x.m_body[i], candidates[i]);
            any = any || is_candidate[i];
        }
        if (!any) {
            return hoisted;
        }
        std::vector<Effects> effects(x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            EffectsCollector c(effects[i]);
            c.visit_stmt(*x.m_body[i]);
        }
        std::vector<bool> hoist(x.n_body, false);
        for (size_t k = 0; k < x.n_body; k++) {
            if (!is_candidate[k]) {
                continue;
            }
            Effects others, before;
            for (size_t j = 0; j < x.n_body; j++) {
                if (j == k) {
                    continue;
                }
                others.merge(effects[j]);
                if (j < k) {
                    before.merge(effects[j]);
                }
            }
            others.value_modified.insert(loop_var);
            hoist[k] = can_hoist(candidates[k], others, before, head_vars);
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            if (hoist[i]) {
                hoisted.push_back(x.m_body[i]);
            } else {
                body.push_back(al, x.m_body[i]);
            }
        }
        if (!hoisted.empty()) {
            x.m_body = body.p;
            x.n_body = body.size();
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

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        ASR::DoLoop_t& xx = const_cast<ASR::DoLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
        if (scope.symtab == nullptr) {
            return;
        }
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
        if (hoisted.empty()) {
            return;
        }
        pass_result.reserve(al, hoisted.size() + 1);
        if (trips == 1) {
            for (ASR::stmt_t* s : hoisted) {
                pass_result.push_back(al, s);
            }
        } else {
            ASRUtils::ASRBuilder b(al, x.base.base.loc);
            pass_result.push_back(al, b.If(runs_at_least_once(x, step),
                hoisted, {}));
        }
        pass_result.push_back(al, &xx.base);
    }
};

} // namespace

void pass_hoist_realloc(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    HoistReAllocVisitor v(al);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
