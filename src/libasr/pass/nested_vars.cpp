#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/pass/pass_utils.h>
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

A procedure value that requires a frame becomes a ProcedureClosure: the
procedure, its logical interface, the target's frame dummy, and this
activation's frame designator. No mutable shared state participates in
binding or invoking it. closure_lowering builds the canonical entry after
the passes that change procedure signatures have finished.
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

    // A logical value interface denotes the original internal procedure.
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
            if (ASRUtils::is_reached_through_host_frame(
                    *ASR::down_cast<ASR::Variable_t>(sym))) {
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
    // The variable that holds the activation's frame.
    ASR::symbol_t *frame = nullptr;
    // host variable, named constant or dummy procedure -> its alias or copy
    std::map<ASR::symbol_t*, ASR::symbol_t*> replacements;
    // namelist group of the host -> its copy
    std::map<ASR::symbol_t*, ASR::symbol_t*> namelists;
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
        use(const_cast<ASR::Var_t&>(x).m_v);
    }

    void visit_Variable(const ASR::Variable_t &x) {
        use(const_cast<ASR::Variable_t&>(x).m_type_declaration);
        ASR::BaseWalkVisitor<ProcedureValueFinder>::visit_Variable(x);
    }

    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
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

class MakeProcedureClosures:
    public ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures> {
    Allocator &al;
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &targets;
    const std::map<ASR::Function_t*, Child> &children;
    ASR::symbol_t *frame;
    bool interface_scope = false;

public:
    MakeProcedureClosures(Allocator &al_,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &targets_,
            const std::map<ASR::Function_t*, Child> &children_,
            ASR::symbol_t *frame_):
        al(al_), targets(targets_), children(children_), frame(frame_) {}

    bool contains_capture(ASR::expr_t *expression) {
        class Finder: public ASR::BaseWalkVisitor<Finder> {
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &targets;
        public:
            bool found = false;
            Finder(const std::map<ASR::symbol_t*, ASR::symbol_t*> &targets_):
                targets(targets_) {}
            void visit_expr(const ASR::expr_t &x) {
                if (!found) ASR::BaseWalkVisitor<Finder>::visit_expr(x);
            }
            void visit_Var(const ASR::Var_t &x) {
                found = targets.count(x.m_v) != 0;
            }
            void visit_ProcedureClosure(const ASR::ProcedureClosure_t &x) {
                found = x.m_environment != nullptr;
            }
        } finder(targets);
        if (expression) finder.visit_expr(*expression);
        return finder.found;
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::symbol_t *saved = frame;
        bool saved_interface = interface_scope;
        interface_scope = ASRUtils::get_FunctionType(x)->m_deftype !=
            ASR::deftypeType::Implementation;
        auto it = children.find(const_cast<ASR::Function_t*>(&x));
        if (it != children.end() && it->second.frame != nullptr) {
            frame = it->second.frame;
        }
        ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_Function(x);
        frame = saved;
        interface_scope = saved_interface;
    }

    void visit_FunctionType(const ASR::FunctionType_t &x) {
        bool saved = interface_scope;
        interface_scope = true;
        ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_FunctionType(x);
        interface_scope = saved;
    }

    template <typename T> void bind_specification_call(T &x) {
        auto found = targets.find(x.m_name);
        if (found != targets.end()) x.m_name = found->second;
        auto *symbol = ASRUtils::symbol_get_past_external(x.m_name);
        if (!ASR::is_a<ASR::Function_t>(*symbol)) return;
        auto *function = ASR::down_cast<ASR::Function_t>(symbol);
        auto child = children.find(function);
        if (child == children.end() || child->second.frame_dummy == nullptr ||
                x.n_args == function->n_args) return;
        LCOMPILERS_ASSERT(frame != nullptr);
        const Location &loc = x.base.base.loc;
        Vec<ASR::call_arg_t> args;
        args.reserve(al, x.n_args + 1);
        for (size_t i = 0; i < x.n_args; i++) args.push_back(al, x.m_args[i]);
        ASR::expr_t *environment = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, frame));
        if (interface_scope) {
            environment = ASRUtils::EXPR(ASR::make_LexicalEnvironment_t(
                al, loc, environment));
        }
        args.push_back(al, {loc, environment});
        x.m_args = args.p;
        x.n_args = args.size();
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        bind_specification_call(const_cast<ASR::FunctionCall_t&>(x));
        ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        bind_specification_call(const_cast<ASR::SubroutineCall_t&>(x));
        ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_SubroutineCall(x);
    }

    void call_replacer() {
        if (*current_expr == nullptr) return;
        if (ASR::is_a<ASR::StructConstant_t>(**current_expr)) {
            auto *constant = ASR::down_cast<ASR::StructConstant_t>(*current_expr);
            if (!contains_capture(*current_expr)) return;
            ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_StructConstant(*constant);
            bool is_constant = true;
            for (size_t i = 0; i < constant->n_args; i++) {
                if (constant->m_args[i].m_value && !ASRUtils::is_value_constant(
                        constant->m_args[i].m_value)) is_constant = false;
            }
            if (!is_constant) {
                *current_expr = ASRUtils::EXPR(ASR::make_StructConstructor_t(al,
                    constant->base.base.loc, constant->m_dt_sym,
                    constant->m_args, constant->n_args, constant->m_type, nullptr));
            }
            return;
        }
        if (ASR::is_a<ASR::StructConstructor_t>(**current_expr)) {
            auto *constructor = ASR::down_cast<ASR::StructConstructor_t>(*current_expr);
            if (!constructor->m_value || !contains_capture(*current_expr)) return;
            ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_StructConstructor(*constructor);
            if (constructor->m_value && !ASRUtils::is_value_constant(
                    constructor->m_value)) constructor->m_value = nullptr;
            return;
        }
        if (ASR::is_a<ASR::ArrayConstructor_t>(**current_expr)) {
            auto *constructor = ASR::down_cast<ASR::ArrayConstructor_t>(*current_expr);
            if (!constructor->m_value || !contains_capture(*current_expr)) return;
            ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_ArrayConstructor(*constructor);
            for (size_t i = 0; i < constructor->n_args; i++) {
                if (!ASRUtils::is_value_constant(constructor->m_args[i])) {
                    constructor->m_value = nullptr;
                    break;
                }
            }
            return;
        }
        if (ASR::is_a<ASR::FunctionPointerCast_t>(**current_expr)) {
            auto *cast = ASR::down_cast<ASR::FunctionPointerCast_t>(*current_expr);
            if (!cast->m_value || !contains_capture(cast->m_value)) return;
            ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_FunctionPointerCast(*cast);
            if (cast->m_value && !ASRUtils::is_value_constant(
                    cast->m_value)) cast->m_value = nullptr;
            return;
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(**current_expr)) {
            auto *member = ASR::down_cast<ASR::StructInstanceMember_t>(*current_expr);
            if (!member->m_value || !contains_capture(member->m_value)) return;
            ASR::CallReplacerOnExpressionsVisitor<MakeProcedureClosures>::visit_StructInstanceMember(*member);
            if (member->m_value && !ASRUtils::is_value_constant(
                    member->m_value)) member->m_value = nullptr;
            return;
        }
        if (!ASR::is_a<ASR::Var_t>(**current_expr)) return;
        ASR::symbol_t *interface = ASR::down_cast<ASR::Var_t>(*current_expr)->m_v;
        auto it = targets.find(interface);
        if (it == targets.end()) return;
        ASR::Function_t *target = ASR::down_cast<ASR::Function_t>(it->second);
        auto child = children.find(target);
        if (child == children.end() || child->second.frame_dummy == nullptr) return;
        const Location &loc = (*current_expr)->base.loc;
        LCOMPILERS_ASSERT(frame != nullptr);
        *current_expr = ASRUtils::EXPR(ASR::make_ProcedureClosure_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, it->second)),
            interface, child->second.frame_dummy,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, frame)),
            ASR::down_cast<ASR::Function_t>(interface)->m_function_signature));
    }
};

class HostFrameBuilder {
public:
    Allocator &al;
    // thunk -> the internal procedure it calls
    std::map<ASR::symbol_t*, ASR::symbol_t*> thunk_targets;

    diag::Diagnostics *diagnostics;
    bool failed = false;

    HostFrameBuilder(Allocator &al_, diag::Diagnostics *diagnostics_):
        al(al_), diagnostics(diagnostics_) {}

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
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ttype_t *referenced = duplicator.duplicate_ttype(
            ASR::down_cast<ASR::Reference_t>(member->m_type)->m_type);
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, object)),
            frame.member_names.at(member), referenced, nullptr));
    }

    // Appends the frame dummy to an internal procedure of the host
    ASR::symbol_t *add_frame_dummy(ASR::Function_t *proc, const Frame &frame) {
        const Location &loc = proc->base.base.loc;
        ASR::Variable_t *dummy = add_variable(proc->m_symtab, "__lcompilers_frame",
            loc, ASR::intentType::In, nullptr, nullptr,
            ASR::storage_typeType::HostFrameArgument, frame_type(loc, frame), frame.type);
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
            member_designator(loc, frame, child.frame, member), nullptr,
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

    // The logical interface of the internal procedure `p`, without its frame.
    // Its
    // dummy arguments are copies of those of `p`, with the declarations they
    // need: entry-evaluated bounds, named constants, interfaces and
    // use-associated entities.
    ASR::symbol_t *make_value_interface(SymbolTable *host, ASR::Function_t *p) {
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
                variables.push_back({ASR::down_cast<ASR::Variable_t>(sym),
                    ASR::down_cast<ASR::Variable_t>(copy)});
            }
        }
        duplicator.remap_local_references(variables, scope);
        duplicator.fixup_local_type_declarations(scope, p->m_symtab);

        std::string name = host->get_unique_name(std::string(p->m_name) +
            "_closure_interface", false);
        if (p->m_return_var != nullptr) {
            // Keep the result distinct from the original procedure's name.
            ASR::Variable_t *result = ASR::down_cast<ASR::Variable_t>(
                scope->get_symbol(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(
                    p->m_return_var)->m_v)));
            scope->erase_symbol(result->m_name);
            result->m_name = s2c(al, name);
            scope->add_symbol(name, (ASR::symbol_t*) result);
        }

        Vec<ASR::expr_t*> args;
        args.reserve(al, p->n_args);
        for (size_t i = 0; i < p->n_args; i++) {
            ASR::symbol_t *dummy = scope->get_symbol(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(p->m_args[i])->m_v));
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, dummy)));
        }
        ASR::expr_t *return_var = nullptr;
        if (p->m_return_var != nullptr) {
            return_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                scope->get_symbol(name)));
        }
        ASR::FunctionType_t *type = ASRUtils::get_FunctionType(p);
        ASR::symbol_t *thunk = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, scope, s2c(al, name), nullptr, 0,
                args.p, args.size(), nullptr, 0, return_var, type->m_abi,
                ASR::accessType::Private, ASR::deftypeType::Interface,
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

        // Keep the logical interface separate from the framed implementation.
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
            ASR::symbol_t *thunk = make_value_interface(host, proc);
            ASR::Function_t *logical_interface = ASR::down_cast<ASR::Function_t>(thunk);
            thunk_procs.push_back(logical_interface);
            for (ASR::symbol_t **use: it->second) {
                *use = thunk;
            }
        }

        std::map<ASR::Function_t*, HostReferenceCollector*> uses;
        std::vector<HostReferenceCollector> collectors;
        collectors.reserve(procs.size() + thunk_procs.size());
        for (ASR::Function_t *proc: procs) {
            collectors.emplace_back(host, thunk_targets);
            collectors.back().visit_Function(*proc);
            uses[proc] = &collectors.back();
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

        // The host entities the frame refers to, including those that their
        // types refer to, in the order of the host's symbol table
        std::set<ASR::symbol_t*> captured_set;
        std::vector<ASR::symbol_t*> worklist;
        std::vector<ASR::Function_t*> children_procs(procs);
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
        // Frame dummies first, so that every call can pass the frame
        std::map<ASR::Function_t*, Child> children;
        for (ASR::Function_t *proc: children_procs) {
            Child &child = children[proc];
            child.proc = proc;
            if (needs_frame.count((ASR::symbol_t*) proc)) {
                child.frame_dummy = add_frame_dummy(proc, frame);
                child.frame = child.frame_dummy;
            }
        }
        FrameActuals actuals(al, needs_frame);
        for (ASR::Function_t *proc: children_procs) {
            Child &child = children[proc];
            // The aliases and copies, including those of the entities that
            // their types and values refer to
            std::vector<ASR::symbol_t*> pending(uses[proc]->entities.begin(),
                uses[proc]->entities.end());
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
            for (size_t i = 0; i < n_body; i++) {
                prologue.push_back(al, body[i]);
            }
            body = prologue.p;
            n_body = prologue.size();
        }

        if (!thunks.empty()) {
            MakeProcedureClosures closures(al, thunk_targets, children, frame.object);
            closures.visit_symbol(*host_sym);
        }

        for (ASR::Function_t *proc: procs) {
            process((ASR::symbol_t*) proc);
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
    HostFrames::HostFrameBuilder builder(al, pass_options.diagnostics);
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
