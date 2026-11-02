#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/pass/pass_utils.h>
#include <deque>
#include <functional>
#include <map>
#include <set>

namespace LCompilers {

/*
Host frames
===========

Each instance of a host (a procedure or main program with internal
procedures that use its variables) gets a frame: a local object of a derived
type declared in the host, with one member per host variable that its internal
procedures use. A member holds the address of the variable (a member of
Reference type, bound by BindReference at the start of the host's body), so
nothing is copied, and every instance of a recursive host has its own frame.

    recursive subroutine h(n)                   ! after the pass
        integer, intent(in) :: n
        integer :: k
        type :: __lcompilers_frame_h
            integer, reference :: k
        end type
        type(__lcompilers_frame_h) :: __lcompilers_frame
        bind_reference(__lcompilers_frame%k, k)
        k = n
        call inner(__lcompilers_frame)
    contains
        subroutine inner(__lcompilers_frame)
            type(__lcompilers_frame_h), intent(in) :: __lcompilers_frame
            integer, host_associated :: k => __lcompilers_frame%k
            if (k > 1) call h(k - 1)
            k = k + 1
        end subroutine
    end subroutine

An internal procedure that needs the frame gets it as its last dummy
argument, and each call passes it: the host its frame object, an internal
procedure its own frame dummy. In an internal procedure, each host variable
is replaced by an alias: a variable of the same type with HostAssociated
storage, bound on entry to the variable its frame member refers to. A named
constant of the host is replaced by a copy.

A member refers to the variable itself, except for a variable whose
representation is fixed at run time by the host (a non-allocatable,
non-pointer array or character) or that is a procedure: the host associates
a pointer "view" with it on entry, and the member refers to the view. The
view of an assumed-size array only holds the address of its data, so its
aliases keep its declared bounds, which refer to the aliases of the host
variables they use. An assumed-size array of strings is represented by the
descriptor of its data and its length, which the member refers to like a
variable.

An internal procedure of an internal procedure (an extension) reaches a
variable of a more distant host through the aliases of the procedures in
between: the pass processes the hosts from the outside in, so that the
variables of a host are aliases of the procedures nested in it, which become
host variables of their own internal procedures in turn. A call to an
internal procedure of a more distant host passes the frame dummy of the
procedure nested in that host, which is reached the same way.

A procedure value of an internal procedure P that needs the frame has no
frame to pass when it is invoked: it is a raw code address, so that it keeps
the ABI of every other procedure value (dummy procedures, procedure pointers,
EXTERNAL and implicit interfaces, c_funloc). Its uses name thunks of the host
instead, procedures with P's own interface that call P with a frame that a
saved pointer of the host, a frame slot, points at.

A value that can be invoked after the statement that makes it (a Var naming
P: an actual argument, a pointer target, a component, c_funloc) is one of a
pool of K thunks `P__lcompilers_value_<i>` (K is
PassOptions::internal_procedure_thunks), thunk i reading frame slot i of a
saved array of the host. An instance of the host that makes a value takes a
free slot, once, points it at its frame and names the thunk of that slot; it
gives the slot back before it returns. The index of the slot it holds (0 for
none) is a variable of the host in the frame, so that taking the slot again
and returning without one cost a comparison. So every live instance has its own
thunks, and a value reaches the instance that made it (Fortran 2018
15.6.2.4), also while a newer instance of the host is active:

    subroutine h(n)                             ! after the pass
        ...
        type :: __lcompilers_frame_slot_h
            type(__lcompilers_frame_h), pointer :: frame
        end type
        type(__lcompilers_frame_slot_h), save :: __lcompilers_frame_slots(K)
        procedure(inner__lcompilers_value_1), pointer :: inner__lcompilers_value_pointer
        integer :: __lcompilers_held_frame_slot, __lcompilers_frame_slot
        bind_reference(__lcompilers_frame%n, n)
        bind_reference(__lcompilers_frame%__lcompilers_held_frame_slot, __lcompilers_held_frame_slot)
        __lcompilers_held_frame_slot = 0
        call __lcompilers_acquire_frame_slot(__lcompilers_frame, __lcompilers_frame_slot)
        call inner__lcompilers_value_select(__lcompilers_frame_slot, inner__lcompilers_value_pointer)
        call solve(inner__lcompilers_value_pointer)
        if (__lcompilers_held_frame_slot /= 0) call __lcompilers_release_frame_slot(__lcompilers_frame)
    contains
        subroutine inner__lcompilers_value_1(x)
            real, intent(inout) :: x
            call inner(x, __lcompilers_frame_slots(1)%frame)
        end subroutine
        ...                                     ! up to thunk K

If the instance holds no slot, the acquire helper takes a free one, or stops
naming the option when all K are taken; so the values one instance makes are
the same thunk. With OpenMP, both
helpers run under the lock of a named critical construct, so instances on
several threads take and give back slots safely. A value invoked after its
instance returned is undefined (Fortran 2018 19.5.2.5): its slot is null or
belongs to another instance. A main program is not entered again, so it
needs no pool. The other uses cannot outlive the
statement that makes them: the interface of a procedure variable, the
callback of a defined output procedure, and the calls in an interface body
or in the interface of a procedure variable to a procedure of the host that
reads one of its variables for a bound or a length (`import :: n`). They name
one thunk `P__lcompilers_value`, which reads a single slot,
`__lcompilers_current_frame`: such a statement first points it at the frame
of its instance, and every instance saves the slot when it starts and
restores it before it returns, so a newer instance active during the
statement has restored it before the statement goes on. If the interface of
P depends on host variables, a thunk reaches them, like an internal
procedure, through aliases bound from the frame in its slot.
*/

namespace HostFrames {

static ASR::symbol_t *scope_owner(SymbolTable *scope) {
    if (scope != nullptr && scope->asr_owner != nullptr &&
            ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        return ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    }
    return nullptr;
}

static bool is_procedure(ASR::symbol_t *sym) {
    return sym != nullptr && (ASR::is_a<ASR::Function_t>(*sym) ||
        ASR::is_a<ASR::Program_t>(*sym));
}

// An internal procedure: a procedure with a body, declared in a procedure or
// main program.
static bool is_internal_procedure(ASR::symbol_t *sym) {
    if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)) {
        return false;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
    return ASRUtils::get_FunctionType(f)->m_deftype ==
            ASR::deftypeType::Implementation &&
        is_procedure(scope_owner(f->m_symtab->parent));
}

static bool is_dummy_argument_of(ASR::symbol_t *sym, ASR::symbol_t *procedure) {
    if (!ASR::is_a<ASR::Function_t>(*procedure)) {
        return false;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(procedure);
    for (size_t i = 0; i < f->n_args; i++) {
        if (ASR::is_a<ASR::Var_t>(*f->m_args[i]) &&
                ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v == sym) {
            return true;
        }
    }
    return false;
}

// The entities of the host whose scope is `host` that an internal procedure
// of the host, or a procedure nested in it, refers to: its variables and
// dummy procedures, and the internal procedures of the host it calls or makes
// a procedure value of (through their thunks, see above).
class HostReferenceCollector: public ASR::BaseWalkVisitor<HostReferenceCollector> {
public:
    SymbolTable *host;
    ASR::symbol_t *host_sym;
    // thunk -> the internal procedure it calls
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &thunks;
    std::set<ASR::symbol_t*> entities;
    std::set<ASR::symbol_t*> called;
    // Inside an interface body, which does not access its host
    size_t in_interface = 0;

    HostReferenceCollector(SymbolTable *host_,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &thunks_):
        host(host_), host_sym(scope_owner(host_)), thunks(thunks_) {}

    // A thunk runs the internal procedure with the frame that the procedure
    // referring to it puts in the frame slot
    bool note_thunk(ASR::symbol_t *sym) {
        auto it = thunks.find(sym);
        if (it == thunks.end()) {
            return false;
        }
        called.insert(it->second);
        return true;
    }

    void note(ASR::symbol_t *sym) {
        if (sym == nullptr || note_thunk(sym) || in_interface > 0 ||
                ASRUtils::symbol_parent_symtab(sym) != host) {
            return;
        }
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            // The frame slots are static storage that is read directly
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            if (ASRUtils::is_reached_through_host_frame(*v) &&
                    !ASRUtils::is_frame_slot(*v)) {
                entities.insert(sym);
            }
        } else if (ASR::is_a<ASR::Function_t>(*sym) &&
                is_dummy_argument_of(sym, host_sym)) {
            entities.insert(sym);
        }
    }

    void note_call(ASR::symbol_t *name) {
        note(name);
        if (in_interface == 0 && is_internal_procedure(name) &&
                ASRUtils::symbol_parent_symtab(name) == host) {
            called.insert(name);
        }
    }

    void note_namelist(ASR::symbol_t *nml) {
        if (nml == nullptr) {
            return;
        }
        nml = ASRUtils::symbol_get_past_external(nml);
        if (!ASR::is_a<ASR::Namelist_t>(*nml)) {
            return;
        }
        ASR::Namelist_t *group = ASR::down_cast<ASR::Namelist_t>(nml);
        for (size_t i = 0; i < group->n_var_list; i++) {
            note(group->m_var_list[i]);
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        bool interface = ASRUtils::get_FunctionType(x)->m_deftype !=
            ASR::deftypeType::Implementation;
        in_interface += interface;
        ASR::BaseWalkVisitor<HostReferenceCollector>::visit_Function(x);
        in_interface -= interface;
    }

    void visit_Template(const ASR::Template_t &/*x*/) {}

    void visit_Var(const ASR::Var_t &x) {
        note(x.m_v);
    }

    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
        note(x.m_callback);
        ASR::BaseWalkVisitor<HostReferenceCollector>::visit_dtio_binding(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        note_call(x.m_name);
        ASR::BaseWalkVisitor<HostReferenceCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        note_call(x.m_name);
        ASR::BaseWalkVisitor<HostReferenceCollector>::visit_SubroutineCall(x);
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        note_namelist(x.m_nml);
        ASR::BaseWalkVisitor<HostReferenceCollector>::visit_FileRead(x);
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        note_namelist(x.m_nml);
        ASR::BaseWalkVisitor<HostReferenceCollector>::visit_FileWrite(x);
    }

    void visit_Namelist(const ASR::Namelist_t &x) {
        for (size_t i = 0; i < x.n_var_list; i++) {
            note(x.m_var_list[i]);
        }
    }
};

// Whether the array type `t` is that of an assumed-size dummy argument,
// represented by its data alone: an UnboundedPointerArray, or for a bind(c)
// procedure a PointerArray whose last dimension has no extent
static bool is_assumed_size(ASR::ttype_t *t) {
    ASR::array_physical_typeType physical_type = ASRUtils::extract_physical_type(t);
    if (physical_type == ASR::array_physical_typeType::UnboundedPointerArray) {
        return true;
    }
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(t, dims);
    return physical_type == ASR::array_physical_typeType::PointerArray &&
        n_dims > 0 && dims[n_dims - 1].m_length == nullptr;
}

// Whether a host variable is reached through a pointer view (see above)
static bool reached_through_view(ASR::symbol_t *sym) {
    if (ASR::is_a<ASR::Function_t>(*sym)) {
        return true;
    }
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
    if (ASRUtils::is_host_associated(*v)) {
        return false;
    }
    ASR::ttype_t *t = v->m_type;
    if (ASRUtils::is_allocatable(t) || ASRUtils::is_pointer(t)) {
        return false;
    }
    if (ASR::is_a<ASR::FunctionType_t>(*t)) {
        return true;
    }
    if (ASRUtils::is_array(t)) {
        return !ASRUtils::is_assumed_rank_array(t) &&
            !ASRUtils::is_assumed_size_string_array(t);
    }
    return ASRUtils::is_character(*t);
}

// Whether a host variable is an assumed-size array reached through a view.
// Its view only holds the address of its data: its aliases are declared with
// its own bounds, which refer to the aliases of the host variables they use.
static bool is_assumed_size_view(ASR::symbol_t *sym) {
    return reached_through_view(sym) && ASR::is_a<ASR::Variable_t>(*sym) &&
        ASRUtils::is_array(ASR::down_cast<ASR::Variable_t>(sym)->m_type) &&
        is_assumed_size(ASR::down_cast<ASR::Variable_t>(sym)->m_type);
}

// The type of the pointer view of a host variable
static ASR::ttype_t *view_type(Allocator &al, ASR::symbol_t *sym) {
    const Location &loc = sym->base.loc;
    if (ASR::is_a<ASR::Function_t>(*sym)) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
            duplicator.duplicate_ttype(ASR::down_cast<ASR::Function_t>(
                sym)->m_function_signature)));
    }
    ASR::ttype_t *t = ASR::down_cast<ASR::Variable_t>(sym)->m_type;
    if (ASR::is_a<ASR::FunctionType_t>(*t)) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
            duplicator.duplicate_ttype(t)));
    }
    if (ASRUtils::is_array(t)) {
        if (is_assumed_size(t)) {
            // An assumed-size array has no upper bound to describe
            return ASRUtils::duplicate_type_with_empty_dims(al, t,
                ASR::array_physical_typeType::UnboundedPointerArray, true);
        }
        ASR::ttype_t *view = ASRUtils::duplicate_type_with_empty_dims(al, t);
        ASR::Array_t *array = ASR::down_cast<ASR::Array_t>(view);
        if (ASRUtils::is_character(*array->m_type)) {
            ASR::String_t *str = ASRUtils::get_string_type(array->m_type);
            if (str->m_len_kind == ASR::AssumedLength || (str->m_len != nullptr &&
                    !ASRUtils::is_value_constant(str->m_len))) {
                array->m_type = ASRUtils::TYPE(ASR::make_String_t(al,
                    str->base.base.loc, str->m_kind, nullptr,
                    ASR::DeferredLength, ASR::DescriptorString));
            }
        }
        return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, view));
    }
    ASR::String_t *str = ASRUtils::get_string_type(t);
    return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
        ASRUtils::TYPE(ASR::make_String_t(al, str->base.base.loc, str->m_kind,
            nullptr, ASR::DeferredLength, ASR::DescriptorString))));
}

// The frame of a host
struct Frame {
    ASR::symbol_t *type = nullptr;
    ASR::symbol_t *object = nullptr;
    // host variable or dummy procedure -> its member
    std::map<ASR::symbol_t*, ASR::Variable_t*> members;
    // member -> the symbol of the host that names it, as for the members
    // of any derived type declared in a procedure
    std::map<ASR::Variable_t*, ASR::symbol_t*> member_names;
};

// The procedure whose host is being processed, and what replaces the host's
// entities in it and in the procedures nested in it.
struct Child {
    ASR::Function_t *proc = nullptr;
    ASR::symbol_t *frame_dummy = nullptr;
    // The variable that holds the frame: the frame dummy, or for a thunk the
    // frame slot of the host (for a thunk of a pool, the frame slots)
    ASR::symbol_t *frame = nullptr;
    // For a thunk of a pool, the designator of its frame slot, which is
    // copied for every use
    ASR::expr_t *frame_designator = nullptr;
    // host variable, named constant or dummy procedure -> its alias or copy
    std::map<ASR::symbol_t*, ASR::symbol_t*> replacements;
    // namelist group of the host -> its copy
    std::map<ASR::symbol_t*, ASR::symbol_t*> namelists;
};

// The frame slots of the thunk pools of a host (see above): a saved array of
// a derived type whose one member points at a frame
struct PoolSlots {
    ASR::symbol_t *slots = nullptr;
    // The index of the slot that the instance holds, 0 if none: a variable
    // of the host in its frame, so that its internal procedures reach it
    ASR::symbol_t *held = nullptr;
    // The member, as named in the host
    ASR::symbol_t *member = nullptr;
    ASR::ttype_t *member_type = nullptr;
};

// Replaces the entities of the host in an expression
class HostEntityReplacer: public ASR::BaseExprReplacer<HostEntityReplacer> {
public:
    Child *child = nullptr;

    void replace_Var(ASR::Var_t *x) {
        auto it = child->replacements.find(x->m_v);
        if (it != child->replacements.end()) {
            x->m_v = it->second;
        }
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t *x) {
        ASR::BaseExprReplacer<HostEntityReplacer>::replace_ArrayPhysicalCast(x);
        x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
    }
};

// Whether a node refers to an entity of the host that is replaced, or calls
// an internal procedure of the host that needs the frame (the call gets the
// frame as an actual argument)
class HostEntityFinder: public ASR::BaseWalkVisitor<HostEntityFinder> {
public:
    const Child &child;
    const std::set<ASR::symbol_t*> &needs_frame;
    bool found = false;

    HostEntityFinder(const Child &child_, const std::set<ASR::symbol_t*> &needs_frame_):
        child(child_), needs_frame(needs_frame_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (child.replacements.find(x.m_v) != child.replacements.end()) {
            found = true;
        }
    }

    void visit_call_name(ASR::symbol_t *name) {
        if (child.replacements.find(name) != child.replacements.end() ||
                needs_frame.find(name) != needs_frame.end()) {
            found = true;
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_call_name(x.m_name);
        ASR::BaseWalkVisitor<HostEntityFinder>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_call_name(x.m_name);
        ASR::BaseWalkVisitor<HostEntityFinder>::visit_SubroutineCall(x);
    }
};

// Appends the frame as the last actual argument of every call to an internal
// procedure of the host that needs it
class FrameActuals {
public:
    Allocator &al;
    const std::set<ASR::symbol_t*> &needs_frame;
    std::set<const ASR::asr_t*> done;

    FrameActuals(Allocator &al_, const std::set<ASR::symbol_t*> &needs_frame_):
        al(al_), needs_frame(needs_frame_) {}

    void append(const ASR::asr_t *call, ASR::symbol_t *name,
            ASR::call_arg_t *&args, size_t &n_args, ASR::symbol_t *frame,
            const Location &loc) {
        append(call, name, args, n_args, ASRUtils::EXPR(ASR::make_Var_t(al,
            loc, frame)), loc);
    }

    void append(const ASR::asr_t *call, ASR::symbol_t *name,
            ASR::call_arg_t *&args, size_t &n_args, ASR::expr_t *frame,
            const Location &loc) {
        if (needs_frame.find(name) == needs_frame.end() ||
                !done.insert(call).second) {
            return;
        }
        ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(name);
        size_t n_user = callee->n_args - 1;
        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, n_user + 1);
        for (size_t i = 0; i < n_args; i++) {
            new_args.push_back(al, args[i]);
        }
        // Absent optional arguments that the call leaves out
        while (new_args.size() < n_user) {
            ASR::call_arg_t absent;
            absent.loc = loc;
            absent.m_value = nullptr;
            new_args.push_back(al, absent);
        }
        ASR::call_arg_t actual;
        actual.loc = loc;
        actual.m_value = frame;
        new_args.push_back(al, actual);
        args = new_args.p;
        n_args = new_args.size();
    }
};

// Passes the frame object of the host at its calls, in its body and in its
// BLOCK and ASSOCIATE constructs, but not in its internal procedures
class HostCalls: public ASR::BaseWalkVisitor<HostCalls> {
public:
    FrameActuals &actuals;
    ASR::symbol_t *frame;

    HostCalls(FrameActuals &actuals_, ASR::symbol_t *frame_):
        actuals(actuals_), frame(frame_) {}

    void visit_Function(const ASR::Function_t &/*x*/) {}

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        actuals.append(&x.base.base, x.m_name, xx.m_args, xx.n_args, frame,
            x.base.base.loc);
        ASR::BaseWalkVisitor<HostCalls>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        actuals.append(&x.base.base, x.m_name, xx.m_args, xx.n_args, frame,
            x.base.base.loc);
        ASR::BaseWalkVisitor<HostCalls>::visit_SubroutineCall(x);
    }
};

// Rewrites an internal procedure of the host and the procedures nested in it:
// the entities of the host become aliases or copies, and calls to internal
// procedures of the host that need the frame pass the frame dummy.
class Rewriter: public ASR::CallReplacerOnExpressionsVisitor<Rewriter> {
public:
    Allocator &al;
    Child &child;
    FrameActuals &actuals;
    HostEntityReplacer replacer;
    bool in_own_signature = false;

    Rewriter(Allocator &al_, Child &child_, FrameActuals &actuals_):
            al(al_), child(child_), actuals(actuals_) {
        replacer.child = &child;
    }

    bool refers_to_host(ASR::expr_t *expr) {
        HostEntityFinder finder(child, actuals.needs_frame);
        finder.visit_expr(*expr);
        return finder.found;
    }

    bool refers_to_host(ASR::ttype_t *type) {
        HostEntityFinder finder(child, actuals.needs_frame);
        finder.visit_ttype(*type);
        return finder.found;
    }

    void call_replacer() {
        if (*current_expr == nullptr || !refers_to_host(*current_expr)) {
            return;
        }
        // The expression can share nodes, notably its type, with a
        // declaration of the host: replace in a copy.
        ASRUtils::ExprStmtDuplicator duplicator(al);
        *current_expr = duplicator.duplicate_expr(*current_expr);
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_Function(const ASR::Function_t &x) {
        if (ASRUtils::get_FunctionType(x)->m_deftype !=
                ASR::deftypeType::Implementation) {
            return;
        }
        ASR::Function_t &xx = const_cast<ASR::Function_t&>(x);
        if (refers_to_host(xx.m_function_signature)) {
            ASRUtils::ExprStmtDuplicator duplicator(al);
            xx.m_function_signature = duplicator.duplicate_ttype(
                xx.m_function_signature);
        }
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_Function(x);
    }

    void visit_Template(const ASR::Template_t &/*x*/) {}

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::Variable_t &xx = const_cast<ASR::Variable_t&>(x);
        bool changed = false;
        // A variable can share its type with a declaration of the host
        if (refers_to_host(xx.m_type)) {
            ASRUtils::ExprStmtDuplicator duplicator(al);
            xx.m_type = duplicator.duplicate_ttype(xx.m_type);
            changed = true;
        }
        changed = changed ||
            (xx.m_symbolic_value != nullptr && refers_to_host(xx.m_symbolic_value)) ||
            (xx.m_value != nullptr && refers_to_host(xx.m_value));
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_Variable(x);
        if (changed) {
            update_dependencies(xx);
        }
    }

    void update_dependencies(ASR::Variable_t &v) {
        SetChar deps;
        deps.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, deps, v.m_type,
            v.m_symbolic_value, v.m_value, v.m_name);
        v.m_dependencies = deps.p;
        v.n_dependencies = deps.size();
    }

    void visit_Namelist(const ASR::Namelist_t &x) {
        ASR::Namelist_t &xx = const_cast<ASR::Namelist_t&>(x);
        for (size_t i = 0; i < xx.n_var_list; i++) {
            auto it = child.replacements.find(xx.m_var_list[i]);
            if (it != child.replacements.end()) {
                xx.m_var_list[i] = it->second;
            }
        }
    }

    // A namelist group of the host becomes a group of the procedure that
    // lists the aliases
    void replace_namelist(ASR::symbol_t *&nml) {
        if (nml == nullptr || ASRUtils::symbol_parent_symtab(nml) !=
                child.proc->m_symtab->parent ||
                !ASR::is_a<ASR::Namelist_t>(*nml)) {
            return;
        }
        auto it = child.namelists.find(nml);
        if (it != child.namelists.end()) {
            nml = it->second;
            return;
        }
        ASR::Namelist_t *group = ASR::down_cast<ASR::Namelist_t>(nml);
        Vec<ASR::symbol_t*> var_list;
        var_list.reserve(al, group->n_var_list);
        for (size_t i = 0; i < group->n_var_list; i++) {
            auto r = child.replacements.find(group->m_var_list[i]);
            var_list.push_back(al, r != child.replacements.end()
                ? r->second : group->m_var_list[i]);
        }
        SymbolTable *scope = child.proc->m_symtab;
        std::string name = scope->get_unique_name(
            ASRUtils::symbol_name(nml), false);
        ASR::symbol_t *copy = ASR::down_cast<ASR::symbol_t>(ASR::make_Namelist_t(
            al, nml->base.loc, scope, group->m_group_name, var_list.p,
            var_list.size()));
        scope->add_symbol(name, copy);
        child.namelists[nml] = copy;
        nml = copy;
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_FileRead(x);
        replace_namelist(const_cast<ASR::FileRead_t&>(x).m_nml);
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_FileWrite(x);
        replace_namelist(const_cast<ASR::FileWrite_t&>(x).m_nml);
    }

    void replace_call_name(ASR::symbol_t *&name) {
        auto it = child.replacements.find(name);
        if (it != child.replacements.end()) {
            name = it->second;
        }
    }

    // The frame, as an actual argument. In the signature of the procedure, a
    // dummy argument is a FunctionParam.
    ASR::expr_t *frame_actual(const Location &loc) {
        if (in_own_signature && child.frame_dummy != nullptr) {
            return ASRUtils::EXPR(ASR::make_FunctionParam_t(al, loc,
                child.proc->n_args - 1, ASRUtils::symbol_type(child.frame_dummy),
                nullptr));
        }
        if (child.frame_designator != nullptr) {
            // Its types refer to the host variables that the frame type
            // refers to, which the thunk reaches through its aliases
            ASRUtils::ExprStmtDuplicator duplicator(al);
            ASR::expr_t *frame = duplicator.duplicate_expr(child.frame_designator);
            replacer.current_expr = &frame;
            replacer.replace_expr(frame);
            return frame;
        }
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, child.frame));
    }

    void visit_FunctionType(const ASR::FunctionType_t &x) {
        bool own = &x.base == child.proc->m_function_signature;
        bool in_own_signature_copy = in_own_signature;
        in_own_signature = in_own_signature || own;
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_FunctionType(x);
        in_own_signature = in_own_signature_copy;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        replace_call_name(xx.m_name);
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_FunctionCall(x);
        if (child.frame != nullptr) {
            actuals.append(&x.base.base, xx.m_name, xx.m_args, xx.n_args,
                frame_actual(x.base.base.loc), x.base.base.loc);
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        replace_call_name(xx.m_name);
        ASR::CallReplacerOnExpressionsVisitor<Rewriter>::visit_SubroutineCall(x);
        if (child.frame != nullptr) {
            actuals.append(&x.base.base, xx.m_name, xx.m_args, xx.n_args,
                frame_actual(x.base.base.loc), x.base.base.loc);
        }
    }
};

// The places that use the internal procedures `procs` of a host as procedure
// values (see above), in the host and the procedures nested in it: a Var
// naming one, the interface of a procedure variable, the callback of a
// defined output procedure, and a call in an interface, where no frame is
// available.
class ProcedureValueFinder: public ASR::BaseWalkVisitor<ProcedureValueFinder> {
public:
    const std::set<ASR::symbol_t*> &procs;
    // procedure -> the places that name it
    std::map<ASR::symbol_t*, std::vector<ASR::symbol_t**>> uses;
    // The procedures named by a Var outside interfaces: their values can be
    // invoked after the statement that makes them
    std::set<ASR::symbol_t*> valued;
    // Inside an interface body, or the interface of a procedure variable
    // or dummy procedure
    size_t in_interface = 0;
    const ASR::ttype_t *own_signature = nullptr;

    ProcedureValueFinder(const std::set<ASR::symbol_t*> &procs_): procs(procs_) {}

    void use(ASR::symbol_t *&sym) {
        if (sym != nullptr && procs.count(sym)) {
            uses[sym].push_back(&sym);
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        bool interface = ASRUtils::get_FunctionType(x)->m_deftype !=
            ASR::deftypeType::Implementation;
        const ASR::ttype_t *own_signature_copy = own_signature;
        own_signature = interface ? nullptr : x.m_function_signature;
        in_interface += interface;
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_Function(x);
        in_interface -= interface;
        own_signature = own_signature_copy;
    }

    void visit_FunctionType(const ASR::FunctionType_t &x) {
        bool interface = &x.base != own_signature;
        in_interface += interface;
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_FunctionType(x);
        in_interface -= interface;
    }

    void visit_Template(const ASR::Template_t &/*x*/) {}

    void visit_Var(const ASR::Var_t &x) {
        if (in_interface == 0 && procs.count(x.m_v)) {
            valued.insert(x.m_v);
        }
        use(const_cast<ASR::Var_t&>(x).m_v);
    }

    void visit_Variable(const ASR::Variable_t &x) {
        use(const_cast<ASR::Variable_t&>(x).m_type_declaration);
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_Variable(x);
    }

    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
        // The callback that the formatter calls, and the procedure that it
        // stands for, which must keep the interface of a defined output
        // procedure
        use(const_cast<ASR::dtio_binding_t&>(x).m_procedure);
        use(const_cast<ASR::dtio_binding_t&>(x).m_callback);
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_dtio_binding(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (in_interface > 0) {
            ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
            use(xx.m_name);
            use(xx.m_original_name);
        }
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        if (in_interface > 0) {
            ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
            use(xx.m_name);
            use(xx.m_original_name);
        }
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_SubroutineCall(x);
    }
};

// Whether a node calls one of the thunks of a host
class ThunkCallFinder: public ASR::BaseWalkVisitor<ThunkCallFinder> {
public:
    const std::set<ASR::symbol_t*> &thunks;
    bool found = false;

    ThunkCallFinder(const std::set<ASR::symbol_t*> &thunks_): thunks(thunks_) {}

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        found = found || thunks.count(x.m_name);
        ASR::BaseWalkVisitor<ThunkCallFinder>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        found = found || thunks.count(x.m_name);
        ASR::BaseWalkVisitor<ThunkCallFinder>::visit_SubroutineCall(x);
    }
};

// Whether a statement makes a procedure value of one of the thunks of a
// host, or calls through an interface that calls one of them. The statements
// nested in it (the body of an IF or a loop) are not part of it: they point
// the slot at the frame themselves, when they run.
class ProcedureValueSite: public ASR::BaseWalkVisitor<ProcedureValueSite> {
public:
    const std::set<ASR::symbol_t*> &thunks;
    bool found = false;
    bool in_statement = false;

    ProcedureValueSite(const std::set<ASR::symbol_t*> &thunks_): thunks(thunks_) {}

    void visit_stmt(const ASR::stmt_t &x) {
        if (in_statement) {
            return;
        }
        in_statement = true;
        ASR::BaseWalkVisitor<ProcedureValueSite>::visit_stmt(x);
        in_statement = false;
    }

    void visit_Function(const ASR::Function_t &/*x*/) {}

    void visit_Var(const ASR::Var_t &x) {
        found = found || thunks.count(x.m_v);
    }

    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
        found = found || (x.m_callback != nullptr && thunks.count(x.m_callback));
        ASR::BaseWalkVisitor<ProcedureValueSite>::visit_dtio_binding(x);
    }

    void visit_callee(ASR::symbol_t *name) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        ThunkCallFinder calls(thunks);
        if (ASR::is_a<ASR::Variable_t>(*callee)) {
            calls.visit_ttype(*ASR::down_cast<ASR::Variable_t>(callee)->m_type);
        } else if (ASR::is_a<ASR::Function_t>(*callee) && ASRUtils::get_FunctionType(
                ASR::down_cast<ASR::Function_t>(callee))->m_deftype !=
                    ASR::deftypeType::Implementation) {
            calls.visit_ttype(*ASR::down_cast<ASR::Function_t>(
                callee)->m_function_signature);
        }
        found = found || calls.found;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_callee(x.m_name);
        ASR::BaseWalkVisitor<ProcedureValueSite>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_callee(x.m_name);
        ASR::BaseWalkVisitor<ProcedureValueSite>::visit_SubroutineCall(x);
    }
};

// Points the frame slot of a host at the frame of the instance before every
// statement that makes a procedure value of one of its thunks (see above)
class ProcedureValueSites: public PassUtils::PassVisitor<ProcedureValueSites> {
public:
    const std::set<ASR::symbol_t*> &thunks;
    ASR::symbol_t *slot;
    ASR::symbol_t *frame;
    // Whether the procedures nested in the one being visited are visited
    bool nested = false;
    size_t sites = 0;

    ProcedureValueSites(Allocator &al_, const std::set<ASR::symbol_t*> &thunks_,
            ASR::symbol_t *slot_, ASR::symbol_t *frame_):
            PassVisitor(al_, nullptr), thunks(thunks_), slot(slot_), frame(frame_) {
        pass_result.reserve(al, 1);
    }

    void visit_stmt(const ASR::stmt_t &x) {
        PassUtils::PassVisitor<ProcedureValueSites>::visit_stmt(x);
        ProcedureValueSite site(thunks);
        site.visit_stmt(x);
        if (site.found) {
            const Location &loc = x.base.loc;
            pass_result.n = 0;
            pass_result.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(
                al, loc, ASRUtils::EXPR(ASR::make_Var_t(al, loc, slot)),
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, frame)))));
            retain_original_stmt = true;
            sites++;
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        if (nested && thunks.count((ASR::symbol_t*) &x) == 0) {
            PassUtils::PassVisitor<ProcedureValueSites>::visit_Function(x);
        }
    }

    // The statements of a host itself, and of its BLOCK and ASSOCIATE
    // constructs
    void visit_host(ASR::stmt_t **&body, size_t &n_body, SymbolTable *scope) {
        current_scope = scope;
        transform_stmts(body, n_body);
        visit_constructs(scope);
    }

    void visit_constructs(SymbolTable *scope) {
        for (auto &item: scope->get_scope()) {
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                visit_Block(*ASR::down_cast<ASR::Block_t>(item.second));
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                visit_AssociateBlock(*ASR::down_cast<ASR::AssociateBlock_t>(item.second));
            }
        }
    }
};

// The Vars of a statement that make a procedure value of a thunk with a pool
// (see above). The statements nested in it are not part of it.
class PoolValueVars: public ASR::BaseWalkVisitor<PoolValueVars> {
public:
    // thunk -> the helper that selects a thunk of its pool
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &pools;
    std::vector<ASR::Var_t*> vars;
    bool in_statement = false;

    PoolValueVars(const std::map<ASR::symbol_t*, ASR::symbol_t*> &pools_):
        pools(pools_) {}

    void visit_stmt(const ASR::stmt_t &x) {
        if (in_statement) {
            return;
        }
        in_statement = true;
        ASR::BaseWalkVisitor<PoolValueVars>::visit_stmt(x);
        in_statement = false;
    }

    void visit_Function(const ASR::Function_t &/*x*/) {}

    void visit_Var(const ASR::Var_t &x) {
        if (pools.count(x.m_v)) {
            vars.push_back(const_cast<ASR::Var_t*>(&x));
        }
    }
};

// Before every statement that makes a procedure value of a thunk with a
// pool, takes the frame slot of the instance (`acquire`) and points a
// procedure pointer at the thunk of the pool bound to it (the select helper
// of the pool), which the statement then names instead.
class PoolValueSites: public PassUtils::PassVisitor<PoolValueSites> {
public:
    // thunk -> the helper that selects a thunk of its pool
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &pools;
    const std::set<ASR::symbol_t*> &skipped;
    ASR::symbol_t *acquire;
    ASR::symbol_t *frame;
    // Where the slot index and the procedure pointers are declared
    SymbolTable *declarations;
    std::function<ASR::symbol_t*(SymbolTable*)> make_index;
    std::function<ASR::symbol_t*(SymbolTable*, ASR::symbol_t*)> make_pointer;
    ASR::symbol_t *index = nullptr;
    std::map<ASR::symbol_t*, ASR::symbol_t*> pointers;
    bool nested = false;

    PoolValueSites(Allocator &al_,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &pools_,
            const std::set<ASR::symbol_t*> &skipped_, ASR::symbol_t *acquire_,
            ASR::symbol_t *frame_, SymbolTable *declarations_,
            std::function<ASR::symbol_t*(SymbolTable*)> make_index_,
            std::function<ASR::symbol_t*(SymbolTable*, ASR::symbol_t*)> make_pointer_):
            PassVisitor(al_, nullptr), pools(pools_), skipped(skipped_),
            acquire(acquire_), frame(frame_), declarations(declarations_),
            make_index(make_index_), make_pointer(make_pointer_) {
        pass_result.reserve(al, 1);
    }

    void visit_stmt(const ASR::stmt_t &x) {
        PassUtils::PassVisitor<PoolValueSites>::visit_stmt(x);
        PoolValueVars site(pools);
        site.visit_stmt(x);
        if (site.vars.empty()) {
            return;
        }
        const Location &loc = x.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        if (index == nullptr) {
            index = make_index(declarations);
        }
        pass_result.n = 0;
        auto call = [&](ASR::symbol_t *name, ASR::symbol_t *a1, ASR::symbol_t *a2) {
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 2);
            for (ASR::symbol_t *arg: {a1, a2}) {
                ASR::call_arg_t a;
                a.loc = loc;
                a.m_value = b.Var(arg);
                args.push_back(al, a);
            }
            pass_result.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, loc, name, nullptr, args.p, args.size(), nullptr, nullptr, false)));
        };
        call(acquire, frame, index);
        std::set<ASR::symbol_t*> selected;
        for (ASR::Var_t *v: site.vars) {
            ASR::symbol_t *thunk = v->m_v;
            if (pointers.find(thunk) == pointers.end()) {
                pointers[thunk] = make_pointer(declarations, thunk);
            }
            ASR::symbol_t *pointer = pointers.at(thunk);
            if (selected.insert(thunk).second) {
                call(pools.at(thunk), index, pointer);
            }
            v->m_v = pointer;
        }
        retain_original_stmt = true;
    }

    void visit_Function(const ASR::Function_t &x) {
        if (nested && skipped.count((ASR::symbol_t*) &x) == 0) {
            PassUtils::PassVisitor<PoolValueSites>::visit_Function(x);
        }
    }

    void visit_host(ASR::stmt_t **&body, size_t &n_body, SymbolTable *scope) {
        current_scope = scope;
        transform_stmts(body, n_body);
        for (auto &item: scope->get_scope()) {
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                visit_Block(*ASR::down_cast<ASR::Block_t>(item.second));
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                visit_AssociateBlock(*ASR::down_cast<ASR::AssociateBlock_t>(item.second));
            }
        }
    }
};

// Puts the statements made by `make_restores` before every return of a
// procedure, including the returns in its BLOCK and ASSOCIATE constructs, but
// not those of the procedures nested in it.
class ReturnRestorer: public PassUtils::PassVisitor<ReturnRestorer> {
public:
    std::function<std::vector<ASR::stmt_t*>()> make_restores;

    ReturnRestorer(Allocator &al_, std::function<std::vector<ASR::stmt_t*>()> make_restores_):
            PassVisitor(al_, nullptr), make_restores(make_restores_) {
        pass_result.reserve(al, 1);
    }

    void visit_Return(const ASR::Return_t &/*x*/) {
        for (ASR::stmt_t *stmt: make_restores()) {
            pass_result.push_back(al, stmt);
        }
        retain_original_stmt = true;
    }

    void visit_Function(const ASR::Function_t &/*x*/) {}

    void restore_before_returns(ASR::Function_t &x) {
        current_scope = x.m_symtab;
        transform_stmts(x.m_body, x.n_body);
        for (auto &item: x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                visit_Block(*ASR::down_cast<ASR::Block_t>(item.second));
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                visit_AssociateBlock(*ASR::down_cast<ASR::AssociateBlock_t>(item.second));
            }
        }
    }
};

class HostFrameBuilder {
public:
    Allocator &al;
    // thunk -> the internal procedure it calls
    std::map<ASR::symbol_t*, ASR::symbol_t*> thunk_targets;

    diag::Diagnostics *diagnostics;
    // The number of thunks of each pool (see above)
    int64_t pool_size;
    // Whether the instances of a host can run on several threads, which
    // then take and give back frame slots under a lock
    bool openmp;
    bool failed = false;

    HostFrameBuilder(Allocator &al_, diag::Diagnostics *diagnostics_,
            int64_t pool_size_, bool openmp_):
        al(al_), diagnostics(diagnostics_), pool_size(pool_size_),
        openmp(openmp_) {}

    // A value dummy argument of a bind(c) procedure is held by the procedure
    // as a value, not as a variable with an address that a frame can refer
    // to (#14178).
    bool reject_unsupported(ASR::symbol_t *host_sym,
            const std::vector<ASR::symbol_t*> &captured) {
        if (!ASR::is_a<ASR::Function_t>(*host_sym) || ASRUtils::get_FunctionType(
                ASR::down_cast<ASR::Function_t>(host_sym))->m_abi !=
                    ASR::abiType::BindC) {
            return false;
        }
        for (ASR::symbol_t *sym: captured) {
            if (!ASR::is_a<ASR::Variable_t>(*sym) ||
                    !ASR::down_cast<ASR::Variable_t>(sym)->m_value_attr ||
                    !is_dummy_argument_of(sym, host_sym)) {
                continue;
            }
            std::string msg = "host association of a value dummy argument "
                "of a bind(c) procedure is not supported yet";
            if (diagnostics == nullptr) {
                throw LCompilersException(msg);
            }
            diagnostics->message_label(msg, {sym->base.loc},
                "used by an internal procedure of '" +
                std::string(ASRUtils::symbol_name(host_sym)) + "'",
                diag::Level::Error, diag::Stage::ASRPass);
            failed = true;
            return true;
        }
        return false;
    }

    static SymbolTable *procedure_scope(ASR::symbol_t *procedure) {
        return ASRUtils::symbol_symtab(procedure);
    }

    // The internal procedures of a host, in the order of its symbol table
    static std::vector<ASR::Function_t*> internal_procedures(SymbolTable *host) {
        std::vector<ASR::Function_t*> procs;
        for (auto &item: host->get_scope()) {
            if (is_internal_procedure(item.second) &&
                    ASRUtils::symbol_parent_symtab(item.second) == host) {
                procs.push_back(ASR::down_cast<ASR::Function_t>(item.second));
            }
        }
        return procs;
    }

    // The entities of the host that a type, or an expression, refers to
    void collect(SymbolTable *host, ASR::ttype_t *type, ASR::expr_t *expr,
            std::set<ASR::symbol_t*> &entities) {
        HostReferenceCollector c(host, thunk_targets);
        if (type != nullptr) {
            c.visit_ttype(*type);
        }
        if (expr != nullptr) {
            c.visit_expr(*expr);
        }
        entities.insert(c.entities.begin(), c.entities.end());
    }

    static bool is_constant(ASR::symbol_t *sym) {
        return ASR::is_a<ASR::Variable_t>(*sym) &&
            ASR::down_cast<ASR::Variable_t>(sym)->m_storage ==
                ASR::storage_typeType::Parameter;
    }

    ASR::Variable_t *add_variable(SymbolTable *scope, const std::string &hint,
            const Location &loc, ASR::intentType intent,
            ASR::expr_t *symbolic_value, ASR::expr_t *value,
            ASR::storage_typeType storage, ASR::ttype_t *type,
            ASR::symbol_t *type_declaration, bool target = false,
            bool is_volatile = false, bool is_asynchronous = false) {
        std::string name = scope->get_unique_name(hint, false);
        SetChar deps;
        deps.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, deps, type, symbolic_value,
            value, name);
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                deps.p, deps.size(), intent, symbolic_value, value, storage,
                type, type_declaration, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required, false,
                target, false, nullptr, is_volatile, is_asynchronous));
        scope->add_symbol(name, sym);
        return ASR::down_cast<ASR::Variable_t>(sym);
    }

    // The declaration of the variables of the type of a host entity
    static ASR::symbol_t *type_declaration_of(ASR::symbol_t *sym) {
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            return sym;
        }
        return ASR::down_cast<ASR::Variable_t>(sym)->m_type_declaration;
    }

    // Declares the frame type and object of the host, and binds the members
    // at the start of its body
    Frame make_frame(ASR::symbol_t *host_sym, const std::vector<ASR::symbol_t*> &captured,
            Vec<ASR::stmt_t*> &prologue) {
        SymbolTable *host = procedure_scope(host_sym);
        const Location &loc = host_sym->base.loc;
        Frame frame;
        SymbolTable *members_scope = al.make_new<SymbolTable>(host);
        Vec<char*> member_names;
        member_names.reserve(al, captured.size());
        std::set<std::string> dependency_names;
        std::map<ASR::symbol_t*, ASR::symbol_t*> views;
        std::string type_name = host->get_unique_name("__lcompilers_frame_" +
            std::string(ASRUtils::symbol_name(host_sym)), false);
        for (ASR::symbol_t *sym: captured) {
            std::string name = ASRUtils::symbol_name(sym);
            ASR::ttype_t *referenced;
            ASR::symbol_t *type_declaration = type_declaration_of(sym);
            if (reached_through_view(sym)) {
                ASR::ttype_t *type = view_type(al, sym);
                ASR::Variable_t *view = add_variable(host,
                    "__lcompilers_view_" + name, sym->base.loc,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default, type, type_declaration);
                views[sym] = (ASR::symbol_t*) view;
                referenced = type;
            } else {
                referenced = ASR::down_cast<ASR::Variable_t>(sym)->m_type;
            }
            ASRUtils::ExprStmtDuplicator duplicator(al);
            ASR::ttype_t *member_type = ASRUtils::TYPE(ASR::make_Reference_t(al,
                sym->base.loc, duplicator.duplicate_ttype(referenced)));
            ASR::Variable_t *member = add_variable(members_scope, name,
                sym->base.loc, ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, member_type, type_declaration);
            frame.members[sym] = member;
            member_names.push_back(al, member->m_name);
            std::string member_name = host->get_unique_name("1_" + type_name +
                "_" + std::string(member->m_name), false);
            ASR::symbol_t *member_sym = ASR::down_cast<ASR::symbol_t>(
                ASR::make_ExternalSymbol_t(al, sym->base.loc, host,
                    s2c(al, member_name), (ASR::symbol_t*) member,
                    s2c(al, type_name), nullptr, 0, member->m_name,
                    ASR::accessType::Public));
            host->add_symbol(member_name, member_sym);
            frame.member_names[member] = member_sym;
            if (type_declaration != nullptr && ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(referenced))) {
                dependency_names.insert(ASRUtils::symbol_name(type_declaration));
            }
        }
        SetChar dependencies;
        dependencies.reserve(al, dependency_names.size());
        for (const std::string &name: dependency_names) {
            dependencies.push_back(al, s2c(al, name));
        }
        frame.type = ASR::down_cast<ASR::symbol_t>(ASR::make_Struct_t(al, loc,
            members_scope, s2c(al, type_name), nullptr, dependencies.p,
            dependencies.size(), member_names.p, member_names.size(), nullptr, 0,
            ASR::abiType::Source, ASR::accessType::Public, false, false, false,
            nullptr, 0, nullptr, nullptr, nullptr, 0));
        ASR::down_cast<ASR::Struct_t>(frame.type)->m_struct_signature =
            ASRUtils::make_StructType_t_util(al, loc, frame.type, true);
        host->add_symbol(type_name, frame.type);
        frame.object = (ASR::symbol_t*) add_variable(host, "__lcompilers_frame",
            loc, ASR::intentType::Local, nullptr, nullptr,
            ASR::storage_typeType::Default, frame_type(loc, frame), frame.type);

        for (ASR::symbol_t *sym: captured) {
            const Location &sloc = sym->base.loc;
            ASR::expr_t *variable = ASRUtils::EXPR(ASR::make_Var_t(al, sloc, sym));
            if (ASR::is_a<ASR::Variable_t>(*sym) && ASRUtils::is_host_associated(
                    *ASR::down_cast<ASR::Variable_t>(sym))) {
                // A variable of a more distant host: pass on the reference
                // to it
                ASRUtils::ExprStmtDuplicator duplicator(al);
                variable = duplicator.duplicate_expr(
                    ASR::down_cast<ASR::Variable_t>(sym)->m_symbolic_value);
            }
            auto view = views.find(sym);
            if (view != views.end()) {
                ASR::expr_t *pointer = ASRUtils::EXPR(ASR::make_Var_t(al, sloc,
                    view->second));
                prologue.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(
                    al, sloc, pointer, variable)));
                variable = pointer;
            }
            prologue.push_back(al, ASRUtils::STMT(ASR::make_BindReference_t(al, sloc,
                member_designator(sloc, frame, frame.object, frame.members[sym]),
                variable)));
        }
        return frame;
    }

    ASR::ttype_t *frame_type(const Location &loc, const Frame &frame) {
        return ASRUtils::make_StructType_t_util(al, loc, frame.type, true);
    }

    // `object%member`, which designates the variable the member refers to
    ASR::expr_t *member_designator(const Location &loc, const Frame &frame,
            ASR::symbol_t *object, ASR::Variable_t *member) {
        return member_designator(loc, frame,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, object)), member);
    }

    ASR::expr_t *member_designator(const Location &loc, const Frame &frame,
            ASR::expr_t *object, ASR::Variable_t *member) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ttype_t *referenced = duplicator.duplicate_ttype(
            ASR::down_cast<ASR::Reference_t>(member->m_type)->m_type);
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
            duplicator.duplicate_expr(object), frame.member_names.at(member),
            referenced, nullptr));
    }

    // Appends the frame dummy to an internal procedure of the host
    ASR::symbol_t *add_frame_dummy(ASR::Function_t *proc, const Frame &frame) {
        const Location &loc = proc->base.base.loc;
        ASR::Variable_t *dummy = add_variable(proc->m_symtab, "__lcompilers_frame",
            loc, ASR::intentType::In, nullptr, nullptr,
            ASR::storage_typeType::Default, frame_type(loc, frame), frame.type);
        Vec<ASR::expr_t*> args;
        args.reserve(al, proc->n_args + 1);
        for (size_t i = 0; i < proc->n_args; i++) {
            args.push_back(al, proc->m_args[i]);
        }
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            (ASR::symbol_t*) dummy)));
        proc->m_args = args.p;
        proc->n_args = args.size();
        ASR::FunctionType_t *old = ASRUtils::get_FunctionType(proc);
        Vec<ASR::ttype_t*> arg_types;
        arg_types.reserve(al, old->n_arg_types + 1);
        for (size_t i = 0; i < old->n_arg_types; i++) {
            arg_types.push_back(al, old->m_arg_types[i]);
        }
        arg_types.push_back(al, frame_type(loc, frame));
        proc->m_function_signature = ASRUtils::TYPE(ASR::make_FunctionType_t(al,
            old->base.base.loc, arg_types.p, arg_types.size(),
            old->m_return_var_type, old->m_abi, old->m_deftype,
            old->m_bindc_name, old->m_elemental, old->m_pure, old->m_module,
            old->m_inline, old->m_static, old->m_restrictions,
            old->n_restrictions, old->m_is_restriction, old->m_exec_space));
        return (ASR::symbol_t*) dummy;
    }

    // The alias in `child` of the host entity `sym`, which `member` of the
    // frame refers to
    ASR::symbol_t *make_alias(Child &child, const Frame &frame,
            ASR::symbol_t *sym, ASR::Variable_t *member) {
        const Location &loc = sym->base.loc;
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ttype_t *type = duplicator.duplicate_ttype(is_assumed_size_view(sym)
            ? ASR::down_cast<ASR::Variable_t>(sym)->m_type
            : ASR::down_cast<ASR::Reference_t>(member->m_type)->m_type);
        if (is_assumed_size_view(sym)) {
            // The alias has the representation of the view (the data alone),
            // also for the PointerArray of a bind(c) host
            ASR::down_cast<ASR::Array_t>(type)->m_physical_type =
                ASR::array_physical_typeType::UnboundedPointerArray;
        }
        bool target = false, is_volatile = false, is_asynchronous = false;
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            target = v->m_target_attr;
            is_volatile = v->m_is_volatile;
            is_asynchronous = v->m_is_asynchronous;
        }
        return (ASR::symbol_t*) add_variable(child.proc->m_symtab,
            ASRUtils::symbol_name(sym), loc, ASR::intentType::Local,
            child.frame_designator != nullptr
                ? member_designator(loc, frame, child.frame_designator, member)
                : member_designator(loc, frame, child.frame, member), nullptr,
            ASR::storage_typeType::HostAssociated, type,
            member->m_type_declaration, target, is_volatile, is_asynchronous);
    }

    // A copy in `child` of the named constant `v` of the host
    ASR::symbol_t *copy_constant(Child &child, ASR::Variable_t *v) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::expr_t *value = duplicator.duplicate_expr(v->m_value);
        ASR::expr_t *symbolic_value = duplicator.duplicate_expr(
            v->m_value != nullptr ? v->m_value : v->m_symbolic_value);
        ASR::Variable_t *copy = add_variable(child.proc->m_symtab, v->m_name,
            v->base.base.loc, ASR::intentType::Local, symbolic_value, value,
            ASR::storage_typeType::Parameter, duplicator.duplicate_ttype(v->m_type),
            v->m_type_declaration);
        copy->m_abi = v->m_abi;
        return (ASR::symbol_t*) copy;
    }

    // A thunk of the internal procedure `p` of the host `host` (see above): a
    // procedure of the host with the interface of `p`, which calls `p`. Its
    // dummy arguments are copies of those of `p`, with the declarations they
    // need: entry-evaluated bounds, named constants, interfaces and
    // use-associated entities. An intent(out) dummy is intent(inout) in the
    // thunk, so that only `p` applies the effects of intent(out) on entry
    // (deallocation, finalization, default initialization), and a pointer
    // result is associated with the result of `p`, not assigned its target.
    // Any other result is the new target of its assignment: the thunk hands
    // the result of `p` to its caller, which finalizes it, and finalizes
    // neither it nor its own result variable (F2018 7.5.6.3 p5).
    ASR::symbol_t *make_thunk(SymbolTable *host, ASR::Function_t *p,
            const std::string &suffix = "__lcompilers_value") {
        const Location &loc = p->base.base.loc;
        SymbolTable *scope = al.make_new<SymbolTable>(host);
        std::set<ASR::symbol_t*> dummies;
        for (size_t i = 0; i < p->n_args; i++) {
            dummies.insert(ASR::down_cast<ASR::Var_t>(p->m_args[i])->m_v);
        }
        if (p->m_return_var != nullptr) {
            dummies.insert(ASR::down_cast<ASR::Var_t>(p->m_return_var)->m_v);
        }
        std::set<ASR::symbol_t*> declarations = dummies;
        for (auto &item: p->m_symtab->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*sym) ||
                (ASR::is_a<ASR::Variable_t>(*sym) &&
                    ASR::down_cast<ASR::Variable_t>(sym)->m_storage ==
                        ASR::storage_typeType::Parameter) ||
                (ASR::is_a<ASR::Function_t>(*sym) && ASRUtils::get_FunctionType(
                    ASR::down_cast<ASR::Function_t>(sym))->m_deftype !=
                        ASR::deftypeType::Implementation)) {
                declarations.insert(sym);
            }
        }
        std::vector<ASR::symbol_t*> pending(declarations.begin(), declarations.end());
        while (!pending.empty()) {
            ASR::symbol_t *sym = pending.back();
            pending.pop_back();
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                continue;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            std::set<ASR::symbol_t*> deps;
            collect(p->m_symtab, v->m_type, v->m_symbolic_value, deps);
            for (ASR::symbol_t *dep: deps) {
                if (ASR::is_a<ASR::Variable_t>(*dep) &&
                        ASRUtils::is_entry_initialized_local(
                            *ASR::down_cast<ASR::Variable_t>(dep)) &&
                        declarations.insert(dep).second) {
                    pending.push_back(dep);
                }
            }
        }
        ASRUtils::SymbolDuplicator duplicator(al);
        std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> variables;
        for (auto &item: p->m_symtab->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (!declarations.count(sym)) {
                continue;
            }
            duplicator.duplicate_symbol(sym, scope);
            ASR::symbol_t *copy = scope->get_symbol(item.first);
            if (ASR::is_a<ASR::Variable_t>(*sym) && copy != nullptr &&
                    ASR::is_a<ASR::Variable_t>(*copy)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(copy);
                if (v->m_intent == ASR::intentType::Out) {
                    v->m_intent = ASR::intentType::InOut;
                }
                variables.push_back({ASR::down_cast<ASR::Variable_t>(sym), v});
            }
        }
        duplicator.remap_local_references(variables, scope);
        duplicator.fixup_local_type_declarations(scope, p->m_symtab);

        std::string name = host->get_unique_name(std::string(p->m_name) +
            suffix, false);
        if (p->m_return_var != nullptr) {
            // The result is named after the thunk, so that the name of `p`
            // does not denote it in the thunk
            ASR::Variable_t *result = ASR::down_cast<ASR::Variable_t>(
                scope->get_symbol(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(
                    p->m_return_var)->m_v)));
            scope->erase_symbol(result->m_name);
            result->m_name = s2c(al, name);
            scope->add_symbol(name, (ASR::symbol_t*) result);
        }

        Vec<ASR::expr_t*> args;
        args.reserve(al, p->n_args);
        Vec<ASR::call_arg_t> actuals;
        actuals.reserve(al, p->n_args);
        for (size_t i = 0; i < p->n_args; i++) {
            ASR::symbol_t *dummy = scope->get_symbol(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(p->m_args[i])->m_v));
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, dummy)));
            ASR::call_arg_t actual;
            actual.loc = loc;
            actual.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, dummy));
            actuals.push_back(al, actual);
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        ASR::expr_t *return_var = nullptr;
        if (p->m_return_var != nullptr) {
            return_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                scope->get_symbol(name)));
            ASRUtils::ExprStmtDuplicator type_duplicator(al);
            ASR::ttype_t *type = ASRUtils::expr_type(return_var);
            ASR::expr_t *call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al,
                loc, (ASR::symbol_t*) p, (ASR::symbol_t*) p, actuals.p, actuals.size(),
                type_duplicator.duplicate_ttype(type), nullptr, nullptr));
            if (ASRUtils::is_pointer(type)) {
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Associate_t_util(al,
                    loc, return_var, call)));
            } else {
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
                    loc, return_var, call, nullptr, ASRUtils::is_allocatable(type),
                    false, true)));
            }
        } else {
            body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al,
                loc, (ASR::symbol_t*) p, (ASR::symbol_t*) p, actuals.p, actuals.size(),
                nullptr, nullptr, false)));
        }
        ASR::FunctionType_t *type = ASRUtils::get_FunctionType(p);
        ASR::symbol_t *thunk = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, scope, s2c(al, name), nullptr, 0,
                args.p, args.size(), body.p, body.size(), return_var, type->m_abi,
                ASR::accessType::Private, ASR::deftypeType::Implementation,
                type->m_abi == ASR::abiType::BindC ? s2c(al, name) : nullptr,
                type->m_elemental, type->m_pure, false, false, false, nullptr, 0,
                false, false, false));
        host->add_symbol(name, thunk);
        thunk_targets[thunk] = (ASR::symbol_t*) p;
        return thunk;
    }

    // Processes the host `host_sym`, then its internal procedures as hosts
    void process(ASR::symbol_t *host_sym) {
        SymbolTable *host = procedure_scope(host_sym);
        std::vector<ASR::Function_t*> procs = internal_procedures(host);
        if (procs.empty()) {
            return;
        }
        const Location &loc = host_sym->base.loc;

        // Every procedure value of an internal procedure names its thunk
        std::set<ASR::symbol_t*> proc_set;
        for (ASR::Function_t *proc: procs) {
            proc_set.insert((ASR::symbol_t*) proc);
        }
        ProcedureValueFinder values(proc_set);
        values.visit_symbol(*host_sym);
        std::vector<ASR::Function_t*> thunk_procs;
        for (ASR::Function_t *proc: procs) {
            auto it = values.uses.find((ASR::symbol_t*) proc);
            if (it == values.uses.end()) {
                continue;
            }
            ASR::symbol_t *thunk = make_thunk(host, proc);
            thunk_procs.push_back(ASR::down_cast<ASR::Function_t>(thunk));
            for (ASR::symbol_t **use: it->second) {
                *use = thunk;
            }
        }

        std::map<ASR::Function_t*, HostReferenceCollector*> uses;
        std::deque<HostReferenceCollector> collectors;
        for (ASR::Function_t *proc: procs) {
            collectors.emplace_back(host, thunk_targets);
            collectors.back().visit_Function(*proc);
            uses[proc] = &collectors.back();
        }
        for (ASR::Function_t *thunk: thunk_procs) {
            collectors.emplace_back(host, thunk_targets);
            collectors.back().visit_Function(*thunk);
            uses[thunk] = &collectors.back();
        }

        // The internal procedures that need the frame: those that refer to a
        // host variable, and those that call one that needs it or make a
        // procedure value of it
        std::set<ASR::symbol_t*> needs_frame;
        for (ASR::Function_t *proc: procs) {
            for (ASR::symbol_t *e: uses[proc]->entities) {
                if (!is_constant(e)) {
                    needs_frame.insert((ASR::symbol_t*) proc);
                    break;
                }
            }
        }
        bool changed = true;
        while (changed) {
            changed = false;
            for (ASR::Function_t *proc: procs) {
                if (needs_frame.count((ASR::symbol_t*) proc)) {
                    continue;
                }
                for (ASR::symbol_t *s: uses[proc]->called) {
                    if (needs_frame.count(s)) {
                        needs_frame.insert((ASR::symbol_t*) proc);
                        changed = true;
                        break;
                    }
                }
            }
        }

        // A procedure value of an internal procedure that does not need the
        // frame is the procedure itself
        std::set<ASR::symbol_t*> thunks;
        std::vector<ASR::Function_t*> kept_thunks;
        for (ASR::Function_t *thunk: thunk_procs) {
            ASR::symbol_t *target = thunk_targets.at((ASR::symbol_t*) thunk);
            if (needs_frame.count(target)) {
                thunks.insert((ASR::symbol_t*) thunk);
                kept_thunks.push_back(thunk);
                continue;
            }
            for (ASR::symbol_t **use: values.uses.at(target)) {
                *use = target;
            }
            host->erase_symbol(thunk->m_name);
            thunk_targets.erase((ASR::symbol_t*) thunk);
        }

        // A procedure value that can be invoked after the statement that
        // makes it is a thunk of a pool, bound to the frame slot that the
        // instance holds (see above). A main program is not entered again,
        // so the one frame slot of its thunk is enough.
        // thunk -> its pool, pool thunk -> the index of its frame slot
        std::map<ASR::symbol_t*, std::vector<ASR::symbol_t*>> pools;
        std::map<ASR::symbol_t*, size_t> pool_index;
        std::vector<ASR::Function_t*> pool_procs;
        if (ASR::is_a<ASR::Function_t>(*host_sym)) {
            for (ASR::Function_t *thunk: kept_thunks) {
                ASR::symbol_t *target = thunk_targets.at((ASR::symbol_t*) thunk);
                if (!values.valued.count(target)) {
                    continue;
                }
                std::vector<ASR::symbol_t*> &pool = pools[(ASR::symbol_t*) thunk];
                for (int64_t i = 0; i < pool_size; i++) {
                    ASR::symbol_t *t = make_thunk(host,
                        ASR::down_cast<ASR::Function_t>(target),
                        "__lcompilers_value_" + std::to_string(i + 1));
                    pool.push_back(t);
                    pool_index[t] = i;
                    pool_procs.push_back(ASR::down_cast<ASR::Function_t>(t));
                    collectors.emplace_back(host, thunk_targets);
                    collectors.back().visit_Function(*ASR::down_cast<ASR::Function_t>(t));
                    uses[ASR::down_cast<ASR::Function_t>(t)] = &collectors.back();
                }
            }
        }

        // The host entities the frame refers to, including those that their
        // types refer to, in the order of the host's symbol table
        std::set<ASR::symbol_t*> captured_set;
        std::vector<ASR::symbol_t*> worklist;
        std::vector<ASR::Function_t*> children_procs(procs);
        children_procs.insert(children_procs.end(), kept_thunks.begin(),
            kept_thunks.end());
        children_procs.insert(children_procs.end(), pool_procs.begin(),
            pool_procs.end());
        for (ASR::Function_t *proc: children_procs) {
            for (ASR::symbol_t *e: uses[proc]->entities) {
                if (!is_constant(e) && captured_set.insert(e).second) {
                    worklist.push_back(e);
                }
            }
        }
        while (!worklist.empty()) {
            ASR::symbol_t *e = worklist.back();
            worklist.pop_back();
            if (!ASR::is_a<ASR::Variable_t>(*e) ||
                    (reached_through_view(e) && !is_assumed_size_view(e))) {
                continue;
            }
            std::set<ASR::symbol_t*> deps;
            collect(host, ASR::down_cast<ASR::Variable_t>(e)->m_type, nullptr, deps);
            for (ASR::symbol_t *d: deps) {
                if (!is_constant(d) && captured_set.insert(d).second) {
                    worklist.push_back(d);
                }
            }
        }
        // The index of the frame slot that the instance holds
        ASR::symbol_t *held = nullptr;
        if (!pools.empty()) {
            held = (ASR::symbol_t*) add_variable(host, "__lcompilers_held_frame_slot",
                loc, ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4)), nullptr);
            captured_set.insert(held);
        }
        std::vector<ASR::symbol_t*> captured;
        for (auto &item: host->get_scope()) {
            if (captured_set.count(item.second)) {
                captured.push_back(item.second);
            }
        }

        if (reject_unsupported(host_sym, captured)) {
            return;
        }

        Frame frame;
        Vec<ASR::stmt_t*> prologue;
        prologue.reserve(al, 2*captured.size() + 1);
        if (!captured.empty()) {
            frame = make_frame(host_sym, captured, prologue);
        }
        // The frame slot that the thunks read
        ASR::symbol_t *slot = nullptr;
        if (!thunks.empty()) {
            slot = (ASR::symbol_t*) add_variable(host, "__lcompilers_current_frame",
                loc, ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Save, ASRUtils::TYPE(ASR::make_Pointer_t(al,
                    loc, frame_type(loc, frame))), frame.type);
        }
        PoolSlots pool_slots;
        // The helpers that take and give back the frame slot of an instance
        // take its frame like an internal procedure, through which they
        // reach the host variables that the frame type refers to
        std::map<ASR::Function_t*, ASR::symbol_t*> helper_frames;
        ASR::symbol_t *acquire = nullptr, *release = nullptr;
        if (!pools.empty()) {
            pool_slots = make_pool_slots(host_sym, frame);
            pool_slots.held = held;
            ASRUtils::ASRBuilder b(al, loc);
            prologue.push_back(al, b.Assignment(b.Var(held), b.i32(0)));
            make_pool_frame_helpers(host_sym, frame, pool_slots, acquire, release);
            for (ASR::symbol_t *h: {acquire, release}) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(h);
                helper_frames[f] = ASR::down_cast<ASR::Var_t>(f->m_args[0])->m_v;
                children_procs.push_back(f);
                collectors.emplace_back(host, thunk_targets);
                collectors.back().visit_Function(*f);
                uses[f] = &collectors.back();
            }
        }

        // Frame dummies first, so that every call can pass the frame
        std::map<ASR::Function_t*, Child> children;
        for (ASR::Function_t *proc: children_procs) {
            Child &child = children[proc];
            child.proc = proc;
            if (needs_frame.count((ASR::symbol_t*) proc)) {
                child.frame_dummy = add_frame_dummy(proc, frame);
                child.frame = child.frame_dummy;
            } else if (thunks.count((ASR::symbol_t*) proc)) {
                child.frame = slot;
            } else if (helper_frames.count(proc)) {
                child.frame_dummy = helper_frames.at(proc);
                child.frame = child.frame_dummy;
            } else if (pool_index.count((ASR::symbol_t*) proc)) {
                child.frame = pool_slots.slots;
                child.frame_designator = slot_designator(loc, pool_slots,
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                        pool_index.at((ASR::symbol_t*) proc) + 1,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))));
            }
        }
        FrameActuals actuals(al, needs_frame);
        for (ASR::Function_t *proc: children_procs) {
            Child &child = children[proc];
            // The aliases and copies, including those of the entities that
            // their types and values refer to
            std::vector<ASR::symbol_t*> pending(uses[proc]->entities.begin(),
                uses[proc]->entities.end());
            if (child.frame_designator != nullptr) {
                // The host variables that the types of the designator of
                // the frame slot refer to
                std::set<ASR::symbol_t*> deps;
                collect(host, nullptr, child.frame_designator, deps);
                pending.insert(pending.end(), deps.begin(), deps.end());
            }
            std::vector<ASR::symbol_t*> created;
            while (!pending.empty()) {
                ASR::symbol_t *e = pending.back();
                pending.pop_back();
                if (child.replacements.count(e)) {
                    continue;
                }
                ASR::symbol_t *r;
                if (is_constant(e)) {
                    r = copy_constant(child, ASR::down_cast<ASR::Variable_t>(e));
                } else {
                    LCOMPILERS_ASSERT(child.frame != nullptr);
                    r = make_alias(child, frame, e, frame.members[e]);
                }
                child.replacements[e] = r;
                created.push_back(r);
                ASR::Variable_t *rv = ASR::down_cast<ASR::Variable_t>(r);
                std::set<ASR::symbol_t*> deps;
                collect(host, rv->m_type, rv->m_storage ==
                    ASR::storage_typeType::Parameter ? rv->m_symbolic_value : nullptr,
                    deps);
                pending.insert(pending.end(), deps.begin(), deps.end());
            }
            Rewriter rewriter(al, child, actuals);
            for (ASR::symbol_t *r: created) {
                rewriter.visit_Variable(*ASR::down_cast<ASR::Variable_t>(r));
            }
            rewriter.visit_Function(*proc);
        }

        if (!captured.empty()) {
            ASR::stmt_t **&body = ASR::is_a<ASR::Function_t>(*host_sym)
                ? ASR::down_cast<ASR::Function_t>(host_sym)->m_body
                : ASR::down_cast<ASR::Program_t>(host_sym)->m_body;
            size_t &n_body = ASR::is_a<ASR::Function_t>(*host_sym)
                ? ASR::down_cast<ASR::Function_t>(host_sym)->n_body
                : ASR::down_cast<ASR::Program_t>(host_sym)->n_body;
            HostCalls calls(actuals, frame.object);
            for (size_t i = 0; i < n_body; i++) {
                calls.visit_stmt(*body[i]);
            }
            for (auto &item: host->get_scope()) {
                if (ASR::is_a<ASR::Block_t>(*item.second) ||
                        ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                    calls.visit_symbol(*item.second);
                }
            }
            if (!pools.empty()) {
                use_pools(host_sym, body, n_body, frame, pools, acquire, release,
                    pool_slots.held, procs, children);
            }
            if (!thunks.empty()) {
                mark_procedure_value_sites(host_sym, body, n_body, frame, slot,
                    thunks, procs, children, prologue);
            }
            for (size_t i = 0; i < n_body; i++) {
                prologue.push_back(al, body[i]);
            }
            body = prologue.p;
            n_body = prologue.size();
        }

        for (ASR::Function_t *proc: procs) {
            process((ASR::symbol_t*) proc);
        }
    }

    // The frame slots of the thunk pools of a host
    PoolSlots make_pool_slots(ASR::symbol_t *host_sym, const Frame &frame) {
        SymbolTable *host = procedure_scope(host_sym);
        const Location &loc = host_sym->base.loc;
        PoolSlots ps;
        SymbolTable *members_scope = al.make_new<SymbolTable>(host);
        std::string type_name = host->get_unique_name("__lcompilers_frame_slot_" +
            std::string(ASRUtils::symbol_name(host_sym)), false);
        ps.member_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
            frame_type(loc, frame)));
        ASR::Variable_t *member = add_variable(members_scope, "frame", loc,
            ASR::intentType::Local, nullptr, nullptr, ASR::storage_typeType::Default,
            ps.member_type, frame.type);
        std::string member_name = host->get_unique_name("1_" + type_name + "_frame",
            false);
        ps.member = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al, loc,
            host, s2c(al, member_name), (ASR::symbol_t*) member, s2c(al, type_name),
            nullptr, 0, member->m_name, ASR::accessType::Public));
        host->add_symbol(member_name, ps.member);
        Vec<char*> member_names;
        member_names.reserve(al, 1);
        member_names.push_back(al, member->m_name);
        SetChar dependencies;
        dependencies.reserve(al, 1);
        dependencies.push_back(al, s2c(al, ASRUtils::symbol_name(frame.type)));
        ASR::symbol_t *type = ASR::down_cast<ASR::symbol_t>(ASR::make_Struct_t(al,
            loc, members_scope, s2c(al, type_name), nullptr, dependencies.p,
            dependencies.size(), member_names.p, member_names.size(), nullptr, 0,
            ASR::abiType::Source, ASR::accessType::Public, false, false, false,
            nullptr, 0, nullptr, nullptr, nullptr, 0));
        ASR::down_cast<ASR::Struct_t>(type)->m_struct_signature =
            ASRUtils::make_StructType_t_util(al, loc, type, true);
        host->add_symbol(type_name, type);
        ASR::ttype_t *integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
            integer_type));
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            pool_size, integer_type));
        dims.push_back(al, dim);
        ps.slots = (ASR::symbol_t*) add_variable(host, "__lcompilers_frame_slots",
            loc, ASR::intentType::Local, nullptr, nullptr,
            ASR::storage_typeType::Save, ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                ASRUtils::make_StructType_t_util(al, loc, type, true), dims.p,
                dims.size(), ASR::array_physical_typeType::FixedSizeArray,
                ASR::memory_spaceType::Global)), type);
        return ps;
    }

    // `slots(index)%frame`: the frame slot `index` of the thunk pools
    ASR::expr_t *slot_designator(const Location &loc, const PoolSlots &ps,
            ASR::expr_t *index) {
        Vec<ASR::array_index_t> indices;
        indices.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = index;
        ai.m_step = nullptr;
        indices.push_back(al, ai);
        ASR::Variable_t *slots = ASR::down_cast<ASR::Variable_t>(ps.slots);
        ASR::expr_t *element = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, ps.slots)), indices.p,
            indices.size(), ASRUtils::make_StructType_t_util(al, loc,
                slots->m_type_declaration, true),
            ASR::arraystorageType::ColMajor, nullptr, false));
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc, element,
            ps.member, duplicator.duplicate_ttype(ps.member_type), nullptr));
    }

    // The statements that take (`start`) or give back the lock of the frame
    // slots, the OpenMP runtime lock of the named critical construct
    // `lcompilers_frame_slots`, which the openmp pass, run before this one,
    // would have made of one
    ASR::stmt_t *slot_lock(SymbolTable *scope, const Location &loc, bool start) {
        ASR::symbol_t *lock = PassUtils::omp_critical_name_lock(al, loc, scope,
            "lcompilers_frame_slots");
        std::string name = start ? "GOMP_critical_name_start" : "GOMP_critical_name_end";
        ASR::symbol_t *fn = scope->get_symbol(name);
        if (fn == nullptr) {
            SymbolTable *fn_scope = al.make_new<SymbolTable>(scope);
            ASR::symbol_t *arg = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, fn_scope, s2c(al, "pptr"),
                    nullptr, 0, ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, ASRUtils::TYPE(ASR::make_CPtr_t(
                        al, loc, ASR::cptr_kindType::CPointer)), nullptr,
                    ASR::abiType::BindC, ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            fn_scope->add_symbol("pptr", arg);
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg)));
            fn = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(al,
                loc, fn_scope, s2c(al, name), nullptr, 0, args.p, args.size(),
                nullptr, 0, nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                ASR::deftypeType::Interface, s2c(al, name), false, false, false,
                false, false, nullptr, 0, false, false, false));
            scope->add_symbol(name, fn);
        }
        Vec<ASR::call_arg_t> call_args;
        call_args.reserve(al, 1);
        ASR::call_arg_t a;
        a.loc = loc;
        a.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, lock));
        call_args.push_back(al, a);
        return ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc, fn,
            nullptr, call_args.p, call_args.size(), nullptr, nullptr, false));
    }

    // The helpers acquire(frame, index), which gives the index of the slot
    // that the instance holds, taking a free one if it holds none, and
    // release(frame), which gives back the slot that the instance holds
    void make_pool_frame_helpers(ASR::symbol_t *host_sym, const Frame &frame,
            const PoolSlots &ps, ASR::symbol_t *&acquire, ASR::symbol_t *&release) {
        SymbolTable *host = procedure_scope(host_sym);
        const Location &loc = host_sym->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        ASR::ttype_t *integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        auto error_stop = [&](const std::string &msg) {
            return ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc, b.StringConstant(msg,
                b.String(b.i32(msg.size()), ASR::ExpressionLength))));
        };
        auto frame_dummy = [&](SymbolTable *scope) {
            return (ASR::symbol_t*) add_variable(scope, "__lcompilers_frame", loc,
                ASR::intentType::In, nullptr, nullptr,
                ASR::storage_typeType::Default, frame_type(loc, frame), frame.type,
                true);
        };
        auto locked = [&](SymbolTable *scope, std::vector<ASR::stmt_t*> stmts) {
            if (openmp) {
                stmts.insert(stmts.begin(), slot_lock(scope, loc, true));
                stmts.push_back(slot_lock(scope, loc, false));
            }
            return stmts;
        };
        // A new expression for every use
        auto held = [&]() { return b.Var(ps.held); };
        std::string host_name = ASRUtils::symbol_name(host_sym);

        // acquire(frame, index): the index of the slot that the instance
        // holds, taking a free one if it holds none
        acquire = make_pool_helper(host, loc,
            "__lcompilers_acquire_frame_slot",
            [&](SymbolTable *scope, Vec<ASR::expr_t*> &args) {
                args.push_back(al, b.Var(frame_dummy(scope)));
                args.push_back(al, b.Var((ASR::symbol_t*) add_variable(scope, "index",
                    loc, ASR::intentType::Out, nullptr, nullptr,
                    ASR::storage_typeType::Default, integer_type, nullptr)));
            },
            [&](SymbolTable *scope, Vec<ASR::stmt_t*> &stmts) {
                ASR::symbol_t *f = scope->get_symbol("__lcompilers_frame");
                ASR::symbol_t *i = (ASR::symbol_t*) add_variable(scope, "i", loc,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default, integer_type, nullptr);
                ASR::expr_t *slot = slot_designator(loc, ps, b.Var(i));
                // i = 1; while (i <= K .and. held == 0) { take slot i if it
                // is free; i = i + 1 }
                ASR::stmt_t *search = b.While(ASRUtils::EXPR(ASR::make_LogicalBinOp_t(
                    al, loc, b.LtE(b.Var(i), b.i32(pool_size)),
                    ASR::logicalbinopType::And, b.Eq(held(), b.i32(0)), logical_type,
                    nullptr)), {
                    b.If(ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc,
                        ASRUtils::EXPR(ASR::make_PointerAssociated_t(al, loc, slot,
                            nullptr, logical_type, nullptr)), logical_type, nullptr)),
                        {ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, loc,
                            slot, b.Var(f))),
                         b.Assignment(held(), b.Var(i))}, {}),
                    b.Assignment(b.Var(i), ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                        al, loc, b.Var(i), ASR::binopType::Add, b.i32(1),
                        integer_type, nullptr)))});
                std::vector<ASR::stmt_t*> take = locked(scope,
                    {b.Assignment(b.Var(i), b.i32(1)), search});
                take.push_back(b.If(b.Eq(held(), b.i32(0)), {error_stop(
                    "more than " + std::to_string(pool_size) + " instances of '" +
                    host_name + "' that made procedure values of its internal "
                    "procedures are active at the same time; recompile with "
                    "--internal-procedure-thunks=<larger value>")}, {}));
                stmts.push_back(al, b.If(b.Eq(held(), b.i32(0)), take, {}));
                stmts.push_back(al, b.Assignment(b.Var(scope->get_symbol("index")),
                    held()));
            });
        // release(frame): gives back the slot that the instance holds
        release = make_pool_helper(host, loc,
            "__lcompilers_release_frame_slot",
            [&](SymbolTable *scope, Vec<ASR::expr_t*> &args) {
                args.push_back(al, b.Var(frame_dummy(scope)));
            },
            [&](SymbolTable *scope, Vec<ASR::stmt_t*> &stmts) {
                Vec<ASR::expr_t*> vars;
                vars.reserve(al, 1);
                vars.push_back(al, slot_designator(loc, ps, held()));
                for (ASR::stmt_t *stmt: locked(scope, {ASRUtils::STMT(
                        ASR::make_Nullify_t(al, loc, vars.p, vars.size()))})) {
                    stmts.push_back(al, stmt);
                }
                stmts.push_back(al, b.Assignment(held(), b.i32(0)));
            });
    }

    // A subroutine of the host with the dummies that `make_dummies` adds
    // and the body that `make_body` makes
    ASR::symbol_t *make_pool_helper(SymbolTable *host, const Location &loc,
            const std::string &hint,
            std::function<void(SymbolTable*, Vec<ASR::expr_t*>&)> make_dummies,
            std::function<void(SymbolTable*, Vec<ASR::stmt_t*>&)> make_body) {
        SymbolTable *scope = al.make_new<SymbolTable>(host);
        Vec<ASR::expr_t*> args;
        args.reserve(al, 2);
        make_dummies(scope, args);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        make_body(scope, body);
        std::string name = host->get_unique_name(hint, false);
        ASR::symbol_t *helper = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, scope, s2c(al, name), nullptr, 0,
                args.p, args.size(), body.p, body.size(), nullptr,
                ASR::abiType::Source, ASR::accessType::Private,
                ASR::deftypeType::Implementation, nullptr, false, false, false,
                false, false, nullptr, 0, false, false, false));
        host->add_symbol(name, helper);
        return helper;
    }

    // The procedure pointer type of the values of the thunks of `pool`: the
    // bounds of the dummies of a thunk are its own declarations, which can
    // call internal procedures of the host, so the pointer keeps only their
    // kinds, ranks and representations
    ASR::ttype_t *pool_pointer_type(const Location &loc,
            const std::vector<ASR::symbol_t*> &pool) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ttype_t *signature = duplicator.duplicate_ttype(
            ASR::down_cast<ASR::Function_t>(pool[0])->m_function_signature);
        ASR::FunctionType_t *ft = ASR::down_cast<ASR::FunctionType_t>(signature);
        ft->m_deftype = ASR::deftypeType::Interface;
        for (size_t i = 0; i < ft->n_arg_types; i++) {
            ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
                ft->m_arg_types[i]);
            if (ASR::is_a<ASR::Array_t>(*t) && ASR::down_cast<ASR::Array_t>(
                    t)->m_physical_type != ASR::array_physical_typeType::FixedSizeArray) {
                ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(t);
                for (size_t j = 0; j < a->n_dims; j++) {
                    a->m_dims[j].m_start = nullptr;
                    a->m_dims[j].m_length = nullptr;
                }
            }
        }
        return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, signature));
    }

    // Makes the procedure values of the thunks with a pool (see above): the
    // helpers that select the thunk of a slot, the statements that make the
    // values, and the return of the slot before every return of the host.
    void use_pools(ASR::symbol_t *host_sym, ASR::stmt_t **&body, size_t &n_body,
            const Frame &frame,
            const std::map<ASR::symbol_t*, std::vector<ASR::symbol_t*>> &pools,
            ASR::symbol_t *acquire, ASR::symbol_t *release, ASR::symbol_t *held,
            const std::vector<ASR::Function_t*> &procs,
            std::map<ASR::Function_t*, Child> &children) {
        SymbolTable *host = procedure_scope(host_sym);
        const Location &loc = host_sym->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        auto local = [&](SymbolTable *scope, const std::string &name,
                ASR::intentType intent) {
            return (ASR::symbol_t*) add_variable(scope, name, loc, intent,
                nullptr, nullptr, ASR::storage_typeType::Default, integer_type,
                nullptr);
        };

        // select(index, pointer) for each pool: points `pointer` at the
        // thunk of slot `index`
        std::map<ASR::symbol_t*, ASR::symbol_t*> selects;
        for (auto &item: pools) {
            const std::vector<ASR::symbol_t*> &pool = item.second;
            selects[item.first] = make_pool_helper(host, loc,
                std::string(ASRUtils::symbol_name(item.first)) + "_select",
                [&](SymbolTable *scope, Vec<ASR::expr_t*> &args) {
                    args.push_back(al, b.Var(local(scope, "index", ASR::intentType::In)));
                    args.push_back(al, b.Var((ASR::symbol_t*) add_variable(scope,
                        "pointer", loc, ASR::intentType::Out, nullptr, nullptr,
                        ASR::storage_typeType::Default, pool_pointer_type(loc, pool),
                        pool[0])));
                },
                [&](SymbolTable *scope, Vec<ASR::stmt_t*> &stmts) {
                    ASR::symbol_t *index = scope->get_symbol("index");
                    ASR::symbol_t *pointer = scope->get_symbol("pointer");
                    for (size_t i = 0; i < pool.size(); i++) {
                        stmts.push_back(al, b.If(b.Eq(b.Var(index), b.i32(i + 1)),
                            {ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, loc,
                                b.Var(pointer), b.Var(pool[i])))}, {}));
                    }
                });
        }

        std::set<ASR::symbol_t*> skipped{acquire, release};
        for (auto &item: pools) {
            skipped.insert(item.first);
            skipped.insert(item.second.begin(), item.second.end());
            skipped.insert(selects.at(item.first));
        }
        auto make_index = [&](SymbolTable *scope) {
            return local(scope, "__lcompilers_frame_slot", ASR::intentType::Local);
        };
        auto make_pointer = [&](SymbolTable *scope, ASR::symbol_t *thunk) {
            const std::vector<ASR::symbol_t*> &pool = pools.at(thunk);
            return (ASR::symbol_t*) add_variable(scope,
                std::string(ASRUtils::symbol_name(thunk)) + "_pointer", loc,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, pool_pointer_type(loc, pool),
                pool[0]);
        };
        PoolValueSites host_sites(al, selects, skipped, acquire, frame.object, host,
            make_index, make_pointer);
        host_sites.visit_host(body, n_body, host);
        for (ASR::Function_t *proc: procs) {
            ASR::symbol_t *frame_dummy = children[proc].frame_dummy;
            if (frame_dummy == nullptr) {
                continue;
            }
            PoolValueSites proc_sites(al, selects, skipped, acquire, frame_dummy,
                proc->m_symtab, make_index, make_pointer);
            proc_sites.nested = true;
            proc_sites.visit_Function(*proc);
        }

        // The host gives back its slot, if it holds one, before it returns
        auto release_call = [&]() {
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 1);
            ASR::call_arg_t a;
            a.loc = loc;
            a.m_value = b.Var(frame.object);
            args.push_back(al, a);
            return b.If(b.NotEq(b.Var(held), b.i32(0)),
                {ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc,
                    release, nullptr, args.p, args.size(), nullptr, nullptr,
                    false))}, {});
        };
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(host_sym);
        ReturnRestorer restorer(al, [&]() {
            return std::vector<ASR::stmt_t*>{release_call()};
        });
        restorer.restore_before_returns(*f);
        if (f->n_body == 0 || !ASR::is_a<ASR::Return_t>(*f->m_body[f->n_body - 1])) {
            Vec<ASR::stmt_t*> released;
            released.reserve(al, f->n_body + 1);
            for (size_t i = 0; i < f->n_body; i++) {
                released.push_back(al, f->m_body[i]);
            }
            released.push_back(al, release_call());
            f->m_body = released.p;
            f->n_body = released.size();
        }
    }

    // Points the frame slot at the frame of the instance before every
    // statement that makes a procedure value (see above), and if there is
    // one, saves the slot when the host starts and restores it before it
    // returns. A main program is not entered again, so it does not restore.
    void mark_procedure_value_sites(ASR::symbol_t *host_sym, ASR::stmt_t **&body,
            size_t &n_body, const Frame &frame, ASR::symbol_t *slot,
            const std::set<ASR::symbol_t*> &thunks,
            const std::vector<ASR::Function_t*> &procs,
            std::map<ASR::Function_t*, Child> &children,
            Vec<ASR::stmt_t*> &prologue) {
        SymbolTable *host = procedure_scope(host_sym);
        const Location &loc = host_sym->base.loc;
        ProcedureValueSites host_sites(al, thunks, slot, frame.object);
        host_sites.visit_host(body, n_body, host);
        size_t sites = host_sites.sites;
        for (ASR::Function_t *proc: procs) {
            ASR::symbol_t *frame_dummy = children[proc].frame_dummy;
            if (frame_dummy == nullptr) {
                continue;
            }
            ProcedureValueSites proc_sites(al, thunks, slot, frame_dummy);
            proc_sites.nested = true;
            proc_sites.visit_Function(*proc);
            sites += proc_sites.sites;
        }
        if (sites == 0 || !ASR::is_a<ASR::Function_t>(*host_sym)) {
            return;
        }
        ASR::symbol_t *previous = (ASR::symbol_t*) add_variable(host,
            "__lcompilers_previous_frame", loc, ASR::intentType::Local, nullptr,
            nullptr, ASR::storage_typeType::Default, ASRUtils::TYPE(
                ASR::make_Pointer_t(al, loc, frame_type(loc, frame))), frame.type);
        auto associate = [&](ASR::symbol_t *target, ASR::symbol_t *value) {
            return ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, loc,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, target)),
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, value))));
        };
        prologue.push_back(al, associate(previous, slot));
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(host_sym);
        ReturnRestorer restorer(al, [&]() {
            return std::vector<ASR::stmt_t*>{associate(slot, previous)};
        });
        restorer.restore_before_returns(*f);
        if (f->n_body == 0 || !ASR::is_a<ASR::Return_t>(*f->m_body[f->n_body - 1])) {
            Vec<ASR::stmt_t*> restored;
            restored.reserve(al, f->n_body + 1);
            for (size_t i = 0; i < f->n_body; i++) {
                restored.push_back(al, f->m_body[i]);
            }
            restored.push_back(al, associate(slot, previous));
            f->m_body = restored.p;
            f->n_body = restored.size();
        }
    }
};

// The procedures and main programs that are not nested in a procedure
static void collect_program_units(SymbolTable *scope,
        std::vector<ASR::symbol_t*> &units) {
    for (auto &item: scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Program_t>(*sym) || ASR::is_a<ASR::Function_t>(*sym)) {
            units.push_back(sym);
        } else if (ASR::is_a<ASR::Module_t>(*sym)) {
            collect_program_units(ASR::down_cast<ASR::Module_t>(sym)->m_symtab, units);
        }
    }
}

} // namespace HostFrames

void pass_nested_vars(Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& pass_options) {
    std::vector<ASR::symbol_t*> units;
    HostFrames::collect_program_units(unit.m_symtab, units);
    HostFrames::HostFrameBuilder builder(al, pass_options.diagnostics,
        pass_options.internal_procedure_thunks, pass_options.openmp);
    for (ASR::symbol_t *u: units) {
        // A procedure defined in another translation unit (loaded from a
        // module file) is lowered there.
        if (ASR::is_a<ASR::Function_t>(*u) && ASRUtils::get_FunctionType(
                ASR::down_cast<ASR::Function_t>(u))->m_abi ==
                    ASR::abiType::ExternalUndefined) {
            continue;
        }
        builder.process(u);
        if (builder.failed) {
            return;
        }
    }
    PassUtils::UpdateDependenciesVisitor x(al);
    x.visit_TranslationUnit(unit);
    ASRVerifyOptions verify_options;
    verify_options.nested_vars = true;
    PassUtils::verify_pass_invariant(unit, pass_options, verify_options,
        "nested_vars");
}

} // namespace LCompilers
