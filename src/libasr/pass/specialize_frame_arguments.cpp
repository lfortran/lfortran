#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/specialize_frame_arguments.h>
#include <libasr/pass/procedure_argument_utils.h>
#include <libasr/pass/pass_utils.h>
#include <map>
#include <set>
#include <vector>

namespace LCompilers {

/*
Dummy procedures passed with the frames of their hosts
=======================================================

A procedure value is a single raw code address (see nested_vars.cpp), so a
value of an internal procedure P that needs the frame of its host is one of
a pool of thunks, each bound to a frame slot that an instance of the host
holds until it returns. A recursive host that passes P on before it recurses
holds one slot per active instance, so its depth is limited by
--internal-procedure-thunks (Knuth's man or boy test, #16665).

With --fast, a procedure G of this translation unit gets a copy G' next to
it (`G__lcompilers_frames`), in which each eligible dummy procedure f is a
pair of dummies: a code pointer `f`, whose interface has an extra last dummy
argument, a type(c_ptr) value, and the address of a frame,
`f__lcompilers_frame`. A call of `f` passes the address as that last
argument, and passing `f` on to an eligible dummy of a procedure passes both
halves (`frame` stands for `__lcompilers_frame` below):

    recursive integer function a'(k, x1, x1_frame, ...) result(res)
        interface
            integer function x1(frame_address)
                type(c_ptr), value :: frame_address
            end function
        end interface
        type(c_ptr), value :: x1_frame
        ...
    contains
        recursive integer function b() result(res)
            m = m - 1
            res = a'(m, b_frame_entry, frame_address(b_frame_entry), &
                x1, x1_frame, ...)
        end function
        integer function b_frame_entry(frame_address)
            type(c_ptr), value :: frame_address
            b_frame_entry = b()
        end function
    end function

An actual procedure P of a call of G' is passed as a frame entry and its
FrameAddress (see ASR.asdl): an internal procedure `P__lcompilers_frame_entry`
of the host of P (for an internal procedure P), or otherwise of the
procedure or main program that makes the call, with the interface of the
dummy and the extra last dummy argument, which calls P. nested_vars gives a
frame entry the frame through that argument instead of a frame dummy, and
makes its FrameAddress the address of the frame of the instance that passes
it, or a null pointer when P needs no frame (a module or external procedure,
or an internal procedure that uses nothing of its host). A frame entry is
passed by its code address, so no thunk or frame slot is needed: a
recursive host of P is only limited by the stack.

G itself keeps its interface, for callers in other translation units, C,
c_funloc and procedure pointers. A call of G is a call of G' if each
eligible dummy of G gets an actual procedure of the right interface, or a
pair of the procedure that makes the call (a copy itself), and the call
needs G': it passes a pair or an internal procedure, or G' makes such calls
itself. The other calls, and every procedure value that is not passed to an
eligible dummy, are as before: nested_vars makes a thunk where an internal
procedure needs one. A procedure nested in G' that is a copy of one with
eligible dummies (such as a copy that specialize_procedure_arguments made in
G) has the same eligible dummies, and its own copy is put next to it.

A dummy procedure f of G is eligible if:

* G can be copied next to itself with its internal procedures: it has a
  body in this translation unit and the default ABI, it is not elemental,
  it has no saved variables, common blocks, derived types, generic
  interfaces or namelists (nor have its internal procedures), and it is not
  too large.
* f is a non-optional, non-pointer dummy procedure with an explicit
  interface that declares only its dummy arguments and result, all scalars
  of numeric or logical type (arrays and strings are not paired yet).
* In G and its internal procedures, f is only called, or passed on as an
  actual argument to an eligible dummy of a procedure with the same
  interface in a call whose other eligible dummies can be paired. Any other
  use (a procedure pointer, c_funloc, a component, a procedure without a
  body or with an implicit interface) is an escape: f is not eligible, and
  an internal procedure passed to it gets a thunk as before.

This is decided for all dummies at once, as the largest set that is closed
under passing on. Each G with eligible dummies is copied once; a call in G'
of a procedure with eligible dummies is processed like any other call.
*/

namespace {

using namespace ProcedureArguments;

// A call, by the fields that the pass reads and rewrites, and the scope it
// is in
struct Call {
    ASR::symbol_t **name;
    ASR::symbol_t **original_name;
    ASR::call_arg_t **args;
    size_t *n_args;
    ASR::expr_t *dt;
    Location loc;
    SymbolTable *scope;
};

// The calls in a procedure, main program or translation unit and the
// procedures nested in it: those in statements, and those in declarations
// (bounds, lengths, initializers and interfaces)
class CallCollector: public ASR::BaseWalkVisitor<CallCollector> {
public:
    SymbolTable *current_scope = nullptr;
    size_t in_declaration = 0;
    std::vector<Call> calls, declaration_calls;

    void visit_Program(const ASR::Program_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallCollector>::visit_Program(x);
        current_scope = parent;
    }

    void visit_Function(const ASR::Function_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        bool interface = ASRUtils::get_FunctionType(x)->m_deftype !=
            ASR::deftypeType::Implementation;
        in_declaration += interface;
        ASR::BaseWalkVisitor<CallCollector>::visit_Function(x);
        in_declaration -= interface;
        current_scope = parent;
    }

    void visit_Block(const ASR::Block_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallCollector>::visit_Block(x);
        current_scope = parent;
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallCollector>::visit_AssociateBlock(x);
        current_scope = parent;
    }

    void visit_Variable(const ASR::Variable_t &x) {
        in_declaration++;
        ASR::BaseWalkVisitor<CallCollector>::visit_Variable(x);
        in_declaration--;
    }

    void visit_FunctionType(const ASR::FunctionType_t &x) {
        in_declaration++;
        ASR::BaseWalkVisitor<CallCollector>::visit_FunctionType(x);
        in_declaration--;
    }

    template <typename T>
    void collect(const T &x) {
        T &xx = const_cast<T&>(x);
        Call call = {&xx.m_name, &xx.m_original_name, &xx.m_args, &xx.n_args, xx.m_dt,
            x.base.base.loc, current_scope};
        (in_declaration > 0 ? declaration_calls : calls).push_back(call);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        collect(x);
        ASR::BaseWalkVisitor<CallCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        collect(x);
        ASR::BaseWalkVisitor<CallCollector>::visit_SubroutineCall(x);
    }
};

// Whether a declaration of an interface refers only to the interface's own
// entities, without calls
class OwnEntities: public ASR::BaseWalkVisitor<OwnEntities> {
public:
    SymbolTable *scope;
    bool ok = true;

    OwnEntities(SymbolTable *scope_): scope(scope_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::symbol_parent_symtab(x.m_v) != scope) {
            ok = false;
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) {
        ok = false;
    }
};

// Whether the interface `iface` declares only its dummy arguments and
// result, of intrinsic types whose bounds and lengths refer only to them, so
// that a copy of it is the same interface in any scope
bool self_contained(ASR::Function_t *iface) {
    size_t n = iface->n_args + (iface->m_return_var != nullptr ? 1 : 0);
    if (iface->m_symtab->get_scope().size() != n) {
        return false;
    }
    for (auto &item: iface->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
            return false;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
        if (v->m_type_declaration != nullptr || ASR::is_a<ASR::StructType_t>(
                *ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable_pointer(v->m_type)))) {
            return false;
        }
        OwnEntities own(iface->m_symtab);
        own.visit_ttype(*v->m_type);
        if (v->m_symbolic_value != nullptr) {
            own.visit_expr(*v->m_symbolic_value);
        }
        if (!own.ok) {
            return false;
        }
    }
    return true;
}

// Whether the dummy arguments and result of `iface` are scalars of numeric
// or logical type, the dummy procedures that are passed as pairs for now
// (arrays and strings get extra arguments and copies in later passes)
bool scalar_interface(ASR::Function_t *iface) {
    std::vector<ASR::expr_t*> vars(iface->m_args, iface->m_args + iface->n_args);
    if (iface->m_return_var != nullptr) {
        vars.push_back(iface->m_return_var);
    }
    for (ASR::expr_t *v: vars) {
        if (v == nullptr || !ASR::is_a<ASR::Var_t>(*v) ||
                !ASR::is_a<ASR::Variable_t>(*ASR::down_cast<ASR::Var_t>(v)->m_v)) {
            return false;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(v);
        switch (type->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical:
                break;
            default:
                return false;
        }
    }
    return true;
}

// The interface of dummy argument `i` of `f` if it is a dummy procedure that
// can be passed as a pair, or nullptr. The interface of its actual
// procedures must be the same (see same_interface).
ASR::Function_t *pair_interface(ASR::Function_t *f, size_t i) {
    ASR::Function_t *iface = removable_dummy_interface(f, i);
    if (iface == nullptr || !scalar_interface(iface) ||
            !self_contained(iface)) {
        return nullptr;
    }
    return iface;
}

// Whether `sym` is a dummy argument of its procedure
bool is_dummy(ASR::symbol_t *sym) {
    ASR::symbol_t *owner = scope_owner(ASRUtils::symbol_parent_symtab(sym));
    if (owner == nullptr || !ASR::is_a<ASR::Function_t>(*owner)) {
        return false;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(owner);
    for (size_t i = 0; i < f->n_args; i++) {
        if (dummy_symbol(f, i) == sym) {
            return true;
        }
    }
    return false;
}

// The scope of the procedure or main program whose scope `scope` is, or is
// nested in through BLOCK and ASSOCIATE constructs
SymbolTable *procedure_scope(SymbolTable *scope) {
    for (; scope != nullptr; scope = scope->parent) {
        ASR::symbol_t *owner = scope_owner(scope);
        if (owner != nullptr && (ASR::is_a<ASR::Function_t>(*owner) ||
                ASR::is_a<ASR::Program_t>(*owner))) {
            return scope;
        }
        if (owner == nullptr || !(ASR::is_a<ASR::Block_t>(*owner) ||
                ASR::is_a<ASR::AssociateBlock_t>(*owner))) {
            return nullptr;
        }
    }
    return nullptr;
}

// Replaces symbols: those of a procedure in its copy by the copies, and a
// dummy procedure of a copy by its new interface
class SymbolRetargeter: public ASR::BaseWalkVisitor<SymbolRetargeter> {
public:
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &targets;

    SymbolRetargeter(const std::map<ASR::symbol_t*, ASR::symbol_t*> &targets_):
        targets(targets_) {}

    void retarget(ASR::symbol_t *&sym) {
        if (sym != nullptr) {
            auto it = targets.find(sym);
            if (it != targets.end()) {
                sym = it->second;
            }
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        retarget(const_cast<ASR::Var_t&>(x).m_v);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        retarget(xx.m_name);
        retarget(xx.m_original_name);
        ASR::BaseWalkVisitor<SymbolRetargeter>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        retarget(xx.m_name);
        retarget(xx.m_original_name);
        ASR::BaseWalkVisitor<SymbolRetargeter>::visit_SubroutineCall(x);
    }

    void visit_Variable(const ASR::Variable_t &x) {
        retarget(const_cast<ASR::Variable_t&>(x).m_type_declaration);
        ASR::BaseWalkVisitor<SymbolRetargeter>::visit_Variable(x);
    }

    void visit_ExternalSymbol(const ASR::ExternalSymbol_t &x) {
        retarget(const_cast<ASR::ExternalSymbol_t&>(x).m_external);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        retarget(const_cast<ASR::BlockCall_t&>(x).m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        retarget(const_cast<ASR::AssociateBlockCall_t&>(x).m_m);
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        retarget(const_cast<ASR::StructInstanceMember_t&>(x).m_m);
        ASR::BaseWalkVisitor<SymbolRetargeter>::visit_StructInstanceMember(x);
    }

    void visit_FunctionPointerCast(const ASR::FunctionPointerCast_t &x) {
        retarget(const_cast<ASR::FunctionPointerCast_t&>(x).m_to);
        ASR::BaseWalkVisitor<SymbolRetargeter>::visit_FunctionPointerCast(x);
    }

    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
        ASR::dtio_binding_t &xx = const_cast<ASR::dtio_binding_t&>(x);
        retarget(xx.m_procedure);
        retarget(xx.m_callback);
        ASR::BaseWalkVisitor<SymbolRetargeter>::visit_dtio_binding(x);
    }

    void visit_ScopedVariableAttribute(const ASR::ScopedVariableAttribute_t &x) {
        retarget(const_cast<ASR::ScopedVariableAttribute_t&>(x).m_variable);
    }

    void visit_FrameAddress(const ASR::FrameAddress_t &x) {
        retarget(const_cast<ASR::FrameAddress_t&>(x).m_procedure);
    }
};

// symbol of `original` or a scope nested in it -> the symbol of the same
// name in the corresponding scope of `copy`
void map_copies(SymbolTable *original, SymbolTable *copy,
        std::map<ASR::symbol_t*, ASR::symbol_t*> &copies) {
    for (auto &item: original->get_scope()) {
        ASR::symbol_t *c = copy->get_symbol(item.first);
        if (c == nullptr || c == item.second || c->type != item.second->type) {
            continue;
        }
        copies[item.second] = c;
        SymbolTable *o = ASRUtils::symbol_symtab(item.second);
        SymbolTable *s = ASRUtils::symbol_symtab(c);
        if (o != nullptr && s != nullptr && o != s) {
            map_copies(o, s, copies);
        }
    }
}

ASR::ttype_t *cptr_type(Allocator &al, const Location &loc) {
    return ASRUtils::TYPE(ASR::make_CPtr_t(al, loc,
        ASR::cptr_kindType::CPointer));
}

// A new type(c_ptr) value dummy argument of the procedure whose scope is
// `scope`
ASR::symbol_t *add_address_dummy(Allocator &al, SymbolTable *scope,
        const std::string &hint, const Location &loc) {
    std::string name = scope->get_unique_name(hint, false);
    ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name), nullptr,
            0, ASR::intentType::In, nullptr, nullptr,
            ASR::storage_typeType::Default, cptr_type(al, loc), nullptr,
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::presenceType::Required, true));
    scope->add_symbol(name, sym);
    return sym;
}

class FrameArgumentSpecializer {
public:
    Allocator &al;
    // eligible dummy -> its interface
    std::map<Dummy, ASR::Function_t*> eligible;
    // procedure with eligible dummies -> their indices
    std::map<ASR::Function_t*, std::vector<size_t>> eligible_of;
    // procedure with eligible dummies -> the calls in it and the procedures
    // nested in it
    std::map<ASR::Function_t*, std::vector<Call>> calls_of;
    // The procedures with eligible dummies whose copies make calls that
    // need copies
    std::set<ASR::Function_t*> useful;
    // procedure with eligible dummies -> its copy, which is put next to it
    // when it is first called
    std::map<ASR::Function_t*, ASR::Function_t*> copies;
    std::set<ASR::Function_t*> placed;
    std::vector<ASR::Function_t*> pending;
    // code dummy of a copy -> its frame dummy, and the interface of the
    // dummy it was
    std::map<ASR::symbol_t*, ASR::symbol_t*> frames;
    std::map<ASR::symbol_t*, ASR::Function_t*> code_interfaces;
    // (procedure as named by the call, scope of the entry) -> frame entry:
    // one per procedure and scope, since the interfaces of the dummies it is
    // passed to are all the same (see same_interface)
    std::map<std::pair<ASR::symbol_t*, SymbolTable*>, ASR::symbol_t*> entries;
    // ExternalSymbol of a procedure -> the ExternalSymbol of its copy
    std::map<ASR::symbol_t*, ASR::symbol_t*> copy_names;

    FrameArgumentSpecializer(Allocator &al_): al(al_) {}

    // The scope of the frame entry of the actual procedure `sym` of a call
    // in `scope`: the host of an internal procedure, otherwise the procedure
    // or main program that makes the call
    SymbolTable *entry_scope(ASR::symbol_t *sym, SymbolTable *scope) {
        if (is_internal_procedure(sym)) {
            return ASRUtils::symbol_parent_symtab(sym);
        }
        return procedure_scope(scope);
    }

    // Whether the actual procedure `sym` of a call in `scope` can be passed
    // to a dummy with the interface `iface` as a frame entry
    bool can_enter(ASR::symbol_t *sym, ASR::Function_t *iface,
            SymbolTable *scope) {
        ASR::symbol_t *p = ASRUtils::symbol_get_past_external(sym);
        if (p == nullptr || !ASR::is_a<ASR::Function_t>(*p) || is_dummy(p) ||
                !same_interface(iface, ASR::down_cast<ASR::Function_t>(p))) {
            return false;
        }
        SymbolTable *host = entry_scope(sym, scope);
        if (host == nullptr ||
                !in_scope_chain(host, ASRUtils::symbol_parent_symtab(sym))) {
            return false;
        }
        // The entry calls `sym` by its name, which none of its dummy
        // arguments may hide
        for (size_t i = 0; i < iface->n_args; i++) {
            if (std::string(ASRUtils::symbol_name(dummy_symbol(iface, i))) ==
                    ASRUtils::symbol_name(sym)) {
                return false;
            }
        }
        return true;
    }

    // Whether a call that names `g` by `name` can name the copy of `g`: by
    // itself, or through an ExternalSymbol next to `name`
    bool copy_nameable(ASR::symbol_t *name, ASR::Function_t *g) {
        return name == (ASR::symbol_t*) g || (ASR::is_a<ASR::ExternalSymbol_t>(
            *name) && ASR::down_cast<ASR::ExternalSymbol_t>(name)->m_external ==
                (ASR::symbol_t*) g);
    }

    // Whether every eligible dummy of `g` in the call `call` in `f` (nullptr
    // outside procedures with eligible dummies) can be paired: with an
    // eligible dummy of `f` or with a frame entry
    bool pairable(const Call &call, ASR::Function_t *g, ASR::Function_t *f) {
        if (*call.n_args != g->n_args || !copy_nameable(*call.name, g)) {
            return false;
        }
        for (size_t k: eligible_of.at(g)) {
            ASR::Function_t *iface = eligible.at({g, k});
            ASR::expr_t *a = (*call.args)[k].m_value;
            if (a == nullptr || !ASR::is_a<ASR::Var_t>(*a)) {
                return false;
            }
            ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(a)->m_v;
            bool dummy = false;
            for (size_t i = 0; f != nullptr && i < f->n_args; i++) {
                if (dummy_symbol(f, i) == s) {
                    auto e = eligible.find({f, i});
                    if (e == eligible.end() || !same_interface(iface, e->second)) {
                        return false;
                    }
                    dummy = true;
                    break;
                }
            }
            if (!dummy && !can_enter(s, iface, call.scope)) {
                return false;
            }
        }
        return true;
    }

    // Whether the call `call` passes an internal procedure to an eligible
    // dummy of `g`
    bool passes_internal_procedure(const Call &call, ASR::Function_t *g) {
        for (size_t k: eligible_of.at(g)) {
            ASR::expr_t *a = (*call.args)[k].m_value;
            if (a != nullptr && ASR::is_a<ASR::Var_t>(*a) && is_internal_procedure(
                    ASR::down_cast<ASR::Var_t>(a)->m_v)) {
                return true;
            }
        }
        return false;
    }

    void update_eligible_of() {
        eligible_of.clear();
        for (auto &e: eligible) {
            eligible_of[e.first.first].push_back(e.first.second);
        }
    }

    // Whether the eligible dummy `d` is only passed on soundly: to an
    // eligible dummy of the same interface, in a call whose eligible dummies
    // can all be paired
    bool passed_on_soundly(const Dummy &d) {
        ASR::Function_t *f = d.first;
        ASR::symbol_t *sym = dummy_symbol(f, d.second);
        for (const Call &call: calls_of.at(f)) {
            for (size_t j = 0; j < *call.n_args; j++) {
                ASR::expr_t *a = (*call.args)[j].m_value;
                if (a == nullptr || !ASR::is_a<ASR::Var_t>(*a) ||
                        ASR::down_cast<ASR::Var_t>(a)->m_v != sym) {
                    continue;
                }
                ASR::Function_t *g = called_procedure(*call.name, call.dt);
                auto e = g != nullptr ? eligible.find({g, j}) : eligible.end();
                if (e == eligible.end() ||
                        !same_interface(e->second, eligible.at(d)) ||
                        !pairable(call, g, f)) {
                    return false;
                }
            }
        }
        return true;
    }

    // The procedures nested in a copy that are copies of procedures with
    // eligible dummies (such as the copies that specialize_procedure_arguments
    // made in a host), and those procedures: once the analysis is done, the
    // copies have the same eligible dummies
    std::vector<std::pair<ASR::Function_t*, ASR::Function_t*>> nested_copies;
    bool analyzed = false;

    void inherit_eligible() {
        for (size_t n = 0; n < nested_copies.size(); n++) {
            ASR::Function_t *copy = nested_copies[n].first;
            ASR::Function_t *original = nested_copies[n].second;
            auto e = eligible_of.find(original);
            if (e == eligible_of.end()) {
                continue;
            }
            for (size_t i: e->second) {
                eligible[{copy, i}] = eligible.at({original, i});
            }
            eligible_of[copy] = e->second;
            if (useful.count(original)) {
                useful.insert(copy);
            }
        }
        nested_copies.clear();
    }

    // A copy of `f` next to it, not yet in its scope, with the symbols of
    // `f` and the scopes nested in it replaced by those of the copy
    ASR::Function_t *make_copy(ASR::Function_t *f) {
        ASRUtils::SymbolDuplicator duplicator(al);
        ASR::symbol_t *copy_sym = duplicator.duplicate_Function(f,
            f->m_symtab->parent);
        if (copy_sym == nullptr) {
            return nullptr;
        }
        ASR::Function_t *copy = ASR::down_cast<ASR::Function_t>(copy_sym);
        std::map<ASR::symbol_t*, ASR::symbol_t*> targets;
        map_copies(f->m_symtab, copy->m_symtab, targets);
        SymbolRetargeter retargeter(targets);
        retargeter.visit_Function(*copy);
        for (auto &t: targets) {
            if (ASR::is_a<ASR::Function_t>(*t.first)) {
                nested_copies.push_back({ASR::down_cast<ASR::Function_t>(t.second),
                    ASR::down_cast<ASR::Function_t>(t.first)});
            }
        }
        if (analyzed) {
            inherit_eligible();
        }
        return copy;
    }

    void analyze(SymbolTable *global_scope) {
        std::vector<ASR::Function_t*> procedures;
        collect_procedures(global_scope, procedures);
        for (ASR::Function_t *f: procedures) {
            DummyUseCollector uses;
            std::map<size_t, ASR::Function_t*> interfaces;
            for (size_t i = 0; i < f->n_args; i++) {
                ASR::Function_t *iface = pair_interface(f, i);
                if (iface != nullptr) {
                    uses.dummies[dummy_symbol(f, i)] = i;
                    interfaces[i] = iface;
                }
            }
            if (uses.dummies.empty() || !copyable_procedure(f, true)) {
                continue;
            }
            for (auto &item: f->m_symtab->get_scope()) {
                uses.visit_symbol(*item.second);
            }
            for (size_t i = 0; i < f->n_body; i++) {
                uses.visit_stmt(*f->m_body[i]);
            }
            if (f->m_return_var != nullptr) {
                uses.visit_expr(*f->m_return_var);
            }
            CallCollector calls;
            calls.visit_Function(*f);
            // A dummy called or passed in a declaration (a bound, a length,
            // an initializer) is not paired: its calls are evaluated with the
            // declarations, before the statements that the pass rewrites
            std::set<ASR::symbol_t*> in_declarations;
            for (const Call &call: calls.declaration_calls) {
                in_declarations.insert(*call.name);
                for (size_t j = 0; j < *call.n_args; j++) {
                    ASR::expr_t *a = (*call.args)[j].m_value;
                    if (a != nullptr && ASR::is_a<ASR::Var_t>(*a)) {
                        in_declarations.insert(ASR::down_cast<ASR::Var_t>(a)->m_v);
                    }
                }
            }
            bool any = false;
            for (auto &d: uses.dummies) {
                if (uses.unremovable.count(d.first) > 0 ||
                        uses.references[d.first] != uses.allowed[d.first] ||
                        in_declarations.count(d.first) > 0) {
                    continue;
                }
                eligible[{f, d.second}] = interfaces.at(d.second);
                any = true;
            }
            if (any) {
                calls_of[f] = calls.calls;
            }
        }
        bool again = true;
        while (again) {
            again = false;
            bool changed = true;
            while (changed) {
                changed = false;
                update_eligible_of();
                for (auto it = eligible.begin(); it != eligible.end(); ) {
                    if (passed_on_soundly(it->first)) {
                        ++it;
                    } else {
                        it = eligible.erase(it);
                        changed = true;
                    }
                }
            }
            update_eligible_of();
            for (auto &e: eligible_of) {
                if (copies.count(e.first)) {
                    continue;
                }
                ASR::Function_t *copy = make_copy(e.first);
                if (copy != nullptr) {
                    copies[e.first] = copy;
                    continue;
                }
                for (size_t i: e.second) {
                    eligible.erase({e.first, i});
                }
                again = true;
            }
        }
        update_eligible_of();
        bool changed = true;
        while (changed) {
            changed = false;
            for (auto &e: eligible_of) {
                ASR::Function_t *f = e.first;
                if (useful.count(f)) {
                    continue;
                }
                for (const Call &call: calls_of.at(f)) {
                    ASR::Function_t *g = called_procedure(*call.name, call.dt);
                    if (g != nullptr && eligible_of.count(g) &&
                            pairable(call, g, f) &&
                            (useful.count(g) || passes_internal_procedure(call, g))) {
                        useful.insert(f);
                        changed = true;
                        break;
                    }
                }
            }
        }
        analyzed = true;
        inherit_eligible();
    }

    // Copies the procedures nested in a copy (in `scope`) that have
    // eligible dummies, before the calls in the copy are rewritten, so that
    // their copies are made from what they were copied from
    void copy_nested(SymbolTable *scope) {
        for (auto &item: scope->get_scope()) {
            SymbolTable *nested = ASRUtils::symbol_symtab(item.second);
            if (nested == nullptr || !(ASR::is_a<ASR::Function_t>(*item.second) ||
                    ASR::is_a<ASR::Block_t>(*item.second) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*item.second))) {
                continue;
            }
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(item.second);
                if (eligible_of.count(f) && !copies.count(f)) {
                    ASR::Function_t *copy = make_copy(f);
                    if (copy == nullptr) {
                        // It is copied like what it is a copy of, which the
                        // analysis copied
                        throw LCompilersException("specialize_frame_arguments: "
                            "cannot copy '" + std::string(f->m_name) + "'");
                    }
                    copies[f] = copy;
                }
            }
            copy_nested(nested);
        }
    }

    // The interface `code` of a dummy procedure of a copy takes the address
    // of a frame as its last dummy argument
    void add_frame_argument(ASR::Function_t *code) {
        const Location &loc = code->base.base.loc;
        ASR::symbol_t *address = add_address_dummy(al, code->m_symtab,
            "__lcompilers_frame_address", loc);
        Vec<ASR::expr_t*> args;
        args.reserve(al, code->n_args + 1);
        for (size_t i = 0; i < code->n_args; i++) {
            args.push_back(al, code->m_args[i]);
        }
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, address)));
        code->m_args = args.p;
        code->n_args = args.size();
        code->m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, loc, code->m_args,
                code->n_args, code->m_return_var,
                ASRUtils::get_FunctionType(code), code->m_symtab));
    }

    // Puts the copy of `g` next to it, with each eligible dummy procedure
    // replaced by a code pointer and the address of a frame
    ASR::Function_t *place(ASR::Function_t *g) {
        ASR::Function_t *copy = copies.at(g);
        if (placed.count(g)) {
            return copy;
        }
        placed.insert(g);
        copy_nested(copy->m_symtab);
        SymbolTable *parent = g->m_symtab->parent;
        std::string name = parent->get_unique_name(std::string(g->m_name) +
            "__lcompilers_frames", false);
        copy->m_name = s2c(al, name);
        copy->m_access = ASR::accessType::Private;
        parent->add_symbol(name, (ASR::symbol_t*) copy);

        std::map<ASR::symbol_t*, ASR::symbol_t*> interfaces;
        Vec<ASR::expr_t*> args;
        args.reserve(al, copy->n_args + eligible_of.at(g).size());
        for (size_t i = 0; i < copy->n_args; i++) {
            args.push_back(al, copy->m_args[i]);
            auto e = eligible.find({g, i});
            if (e == eligible.end()) {
                continue;
            }
            const Location &loc = copy->m_args[i]->base.loc;
            ASR::symbol_t *dummy = dummy_symbol(copy, i);
            std::string dummy_name = ASRUtils::symbol_name(dummy);
            ASR::Function_t *code;
            if (ASR::is_a<ASR::Function_t>(*dummy)) {
                code = ASR::down_cast<ASR::Function_t>(dummy);
                add_frame_argument(code);
            } else {
                // A procedure statement, whose interface can be that of a
                // procedure with a body: the dummy becomes an interface body
                copy->m_symtab->erase_symbol(dummy_name);
                code = frame_procedure(e->second, copy->m_symtab, dummy_name,
                    nullptr);
                interfaces[dummy] = (ASR::symbol_t*) code;
                args.p[args.size() - 1] = ASRUtils::EXPR(ASR::make_Var_t(al,
                    loc, (ASR::symbol_t*) code));
            }
            ASR::symbol_t *frame = add_address_dummy(al, copy->m_symtab,
                dummy_name + "__lcompilers_frame", loc);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, frame)));
            frames[(ASR::symbol_t*) code] = frame;
            code_interfaces[(ASR::symbol_t*) code] = e->second;
        }
        copy->m_args = args.p;
        copy->n_args = args.size();
        copy->m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, copy->base.base.loc,
                copy->m_args, copy->n_args, copy->m_return_var,
                ASRUtils::get_FunctionType(g), copy->m_symtab));
        if (!interfaces.empty()) {
            SymbolRetargeter retargeter(interfaces);
            retargeter.visit_Function(*copy);
        }
        pending.push_back(copy);
        return copy;
    }

    // A procedure of `scope` named `name` with the interface `iface` and an
    // extra last dummy argument, a type(c_ptr) value: an interface body, or
    // with `callee` a frame entry that calls `callee` with its other dummy
    // arguments. Its result is named after it, so that it does not hide
    // `callee`.
    ASR::Function_t *frame_procedure(ASR::Function_t *iface, SymbolTable *scope,
            const std::string &name, ASR::symbol_t *callee) {
        const Location &loc = iface->base.base.loc;
        ASRUtils::SymbolDuplicator duplicator(al);
        ASR::Function_t *copy = ASR::down_cast<ASR::Function_t>(
            duplicator.duplicate_Function(iface, scope));
        SymbolTable *symtab = copy->m_symtab;
        ASR::expr_t *return_var = copy->m_return_var;
        if (return_var != nullptr) {
            ASR::Variable_t *result = ASRUtils::EXPR2VAR(return_var);
            symtab->erase_symbol(result->m_name);
            result->m_name = s2c(al, name);
            symtab->add_symbol(name, (ASR::symbol_t*) result);
        }
        ASR::symbol_t *address = add_address_dummy(al, symtab,
            "__lcompilers_frame_address", loc);
        Vec<ASR::expr_t*> args;
        args.reserve(al, copy->n_args + 1);
        Vec<ASR::call_arg_t> actuals;
        actuals.reserve(al, copy->n_args);
        for (size_t i = 0; i < copy->n_args; i++) {
            args.push_back(al, copy->m_args[i]);
            ASR::call_arg_t actual;
            actual.loc = loc;
            actual.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                ASR::down_cast<ASR::Var_t>(copy->m_args[i])->m_v));
            actuals.push_back(al, actual);
        }
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, address)));
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        if (callee != nullptr && return_var != nullptr) {
            ASRUtils::ExprStmtDuplicator type_duplicator(al);
            ASR::ttype_t *type = type_duplicator.duplicate_ttype(
                ASRUtils::expr_type(return_var));
            ASR::expr_t *call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(
                al, loc, callee, nullptr, actuals.p, actuals.size(), type,
                nullptr, nullptr));
            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, return_var, call, nullptr, false, false, true)));
        } else if (callee != nullptr) {
            body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, loc, callee, nullptr, actuals.p, actuals.size(), nullptr,
                nullptr, false)));
        }
        ASR::FunctionType_t *type = ASRUtils::get_FunctionType(iface);
        ASR::symbol_t *f = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, symtab, s2c(al, name),
                nullptr, 0, args.p, args.size(), body.p, body.size(),
                return_var, ASR::abiType::Source,
                callee != nullptr ? ASR::accessType::Private
                    : ASR::accessType::Public,
                callee != nullptr ? ASR::deftypeType::Implementation
                    : ASR::deftypeType::Interface,
                nullptr, false, type->m_pure, false, false, false, nullptr, 0,
                false, false, false));
        scope->add_symbol(name, f);
        return ASR::down_cast<ASR::Function_t>(f);
    }

    // The frame entry of the actual procedure `sym` of a call in `scope`
    // for a dummy with the interface `iface` (see above). The interfaces of
    // the dummies that `sym` is passed to are all the same (see
    // same_interface), so one entry serves them all.
    ASR::symbol_t *entry(ASR::symbol_t *sym, ASR::Function_t *iface,
            SymbolTable *scope) {
        SymbolTable *host = entry_scope(sym, scope);
        auto it = entries.find({sym, host});
        if (it != entries.end()) {
            return it->second;
        }
        std::string name = host->get_unique_name(std::string(
            ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(sym))) +
            "__lcompilers_frame_entry", false);
        ASR::symbol_t *e = (ASR::symbol_t*) frame_procedure(iface, host, name,
            sym);
        LCOMPILERS_ASSERT(ASRUtils::is_frame_entry(e));
        entries[{sym, host}] = e;
        return e;
    }

    // The name that a call in the scope of `name`, which names `g`, gives
    // the copy of `g`
    ASR::symbol_t *copy_name(ASR::symbol_t *name, ASR::Function_t *copy) {
        if (!ASR::is_a<ASR::ExternalSymbol_t>(*name)) {
            return (ASR::symbol_t*) copy;
        }
        auto it = copy_names.find(name);
        if (it != copy_names.end()) {
            return it->second;
        }
        ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(name);
        SymbolTable *scope = ext->m_parent_symtab;
        std::string n = scope->get_unique_name(copy->m_name, false);
        ASR::symbol_t *copy_ext = ASR::down_cast<ASR::symbol_t>(
            ASR::make_ExternalSymbol_t(al, ext->base.base.loc, scope,
                s2c(al, n), (ASR::symbol_t*) copy, ext->m_module_name,
                ext->m_scope_names, ext->n_scope_names, copy->m_name,
                ASR::accessType::Private));
        scope->add_symbol(n, copy_ext);
        copy_names[name] = copy_ext;
        return copy_ext;
    }

    // A call of a code dummy passes its frame
    void pass_frame(const Call &call, ASR::Function_t *code,
            ASR::symbol_t *frame) {
        Vec<ASR::call_arg_t> args;
        args.reserve(al, code->n_args);
        for (size_t i = 0; i < *call.n_args; i++) {
            args.push_back(al, (*call.args)[i]);
        }
        while (args.size() + 1 < code->n_args) {
            ASR::call_arg_t absent;
            absent.loc = call.loc;
            absent.m_value = nullptr;
            args.push_back(al, absent);
        }
        ASR::call_arg_t actual;
        actual.loc = call.loc;
        actual.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, call.loc, frame));
        args.push_back(al, actual);
        *call.args = args.p;
        *call.n_args = args.size();
    }

    void process(const Call &call) {
        auto fr = frames.find(*call.name);
        if (fr != frames.end()) {
            pass_frame(call, ASR::down_cast<ASR::Function_t>(*call.name),
                fr->second);
            return;
        }
        // A code dummy is only passed to a call that becomes a call of a
        // copy: the analysis pairs a dummy only where it is passed on so
        bool pair = false;
        for (size_t i = 0; i < *call.n_args; i++) {
            ASR::expr_t *a = (*call.args)[i].m_value;
            pair = pair || (a != nullptr && ASR::is_a<ASR::Var_t>(*a) &&
                frames.count(ASR::down_cast<ASR::Var_t>(a)->m_v));
        }
        auto unpaired = [&]() {
            if (pair) {
                throw LCompilersException("specialize_frame_arguments: a "
                    "dummy procedure passed with its frame reaches a call of '" +
                    std::string(ASRUtils::symbol_name(*call.name)) +
                    "' that does not take it");
            }
        };
        ASR::Function_t *g = called_procedure(*call.name, call.dt);
        if (g == nullptr || !eligible_of.count(g) || *call.n_args != g->n_args ||
                !copy_nameable(*call.name, g)) {
            unpaired();
            return;
        }
        bool forwarded = false, internal = false;
        for (size_t k: eligible_of.at(g)) {
            ASR::Function_t *iface = eligible.at({g, k});
            ASR::expr_t *a = (*call.args)[k].m_value;
            bool ok = a != nullptr && ASR::is_a<ASR::Var_t>(*a);
            if (ok) {
                ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(a)->m_v;
                if (frames.count(s)) {
                    forwarded = true;
                    ok = same_interface(iface, code_interfaces.at(s));
                } else {
                    ok = can_enter(s, iface, call.scope);
                    internal = internal || is_internal_procedure(s);
                }
            }
            if (!ok) {
                unpaired();
                return;
            }
        }
        if (!forwarded && !internal && !useful.count(g)) {
            unpaired();
            return;
        }
        ASR::Function_t *copy = place(g);
        Vec<ASR::call_arg_t> args;
        args.reserve(al, copy->n_args);
        for (size_t i = 0; i < *call.n_args; i++) {
            ASR::call_arg_t arg = (*call.args)[i];
            auto e = eligible.find({g, i});
            if (e == eligible.end()) {
                args.push_back(al, arg);
                continue;
            }
            const Location &loc = arg.loc;
            ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(arg.m_value)->m_v;
            ASR::call_arg_t frame;
            frame.loc = loc;
            auto f = frames.find(s);
            if (f != frames.end()) {
                frame.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, f->second));
            } else {
                ASR::symbol_t *e_sym = entry(s, e->second, call.scope);
                arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, e_sym));
                frame.m_value = ASRUtils::EXPR(ASR::make_FrameAddress_t(al, loc,
                    e_sym, cptr_type(al, loc), nullptr));
            }
            args.push_back(al, arg);
            args.push_back(al, frame);
        }
        *call.name = copy_name(*call.name, copy);
        *call.original_name = nullptr;
        *call.args = args.p;
        *call.n_args = args.size();
    }

    // Processes the calls of the translation unit, then those of each copy
    // that is put in place
    bool run(ASR::TranslationUnit_t &unit) {
        CallCollector collector;
        collector.visit_TranslationUnit(unit);
        for (const Call &call: collector.calls) {
            process(call);
        }
        while (!pending.empty()) {
            ASR::Function_t *copy = pending.back();
            pending.pop_back();
            CallCollector copy_collector;
            copy_collector.visit_Function(*copy);
            for (const Call &call: copy_collector.calls) {
                process(call);
            }
        }
        return !placed.empty();
    }
};

} // namespace

void pass_specialize_frame_arguments(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options) {
    if (!pass_options.fast) {
        return;
    }
    FrameArgumentSpecializer specializer(al);
    specializer.analyze(unit.m_symtab);
    if (specializer.eligible.empty()) {
        return;
    }
    if (specializer.run(unit)) {
        PassUtils::UpdateDependenciesVisitor dependencies(al);
        dependencies.visit_TranslationUnit(unit);
    }
}

} // namespace LCompilers
