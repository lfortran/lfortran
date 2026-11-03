#ifndef LIBASR_PASS_PROCEDURE_ARGUMENT_UTILS_H
#define LIBASR_PASS_PROCEDURE_ARGUMENT_UTILS_H

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <map>
#include <set>
#include <string>
#include <vector>

// The analysis of dummy procedures and the copying of procedures that the
// --fast passes specialize_procedure_arguments and specialize_frame_arguments
// share.

namespace LCompilers {

namespace ProcedureArguments {

// The largest number of ASR statements and expressions of a procedure that
// is copied.
constexpr size_t max_copied_nodes = 4000;

ASR::symbol_t *scope_owner(SymbolTable *scope);

bool in_scope_chain(SymbolTable *scope, SymbolTable *s);

// `sym` is visible from `scope` the way asr_verify requires it: it is the
// symbol of its name in a scope that contains `scope`.
bool visible_from(SymbolTable *scope, ASR::symbol_t *sym);

// An internal procedure: a procedure with a body, declared in a procedure or
// main program.
bool is_internal_procedure(ASR::symbol_t *sym);

// The module or program at the top of the scopes that contain `scope`, or
// nullptr in the global scope.
ASR::symbol_t *top_level_unit(SymbolTable *scope);

// The symbol of a dummy argument, or nullptr.
ASR::symbol_t *dummy_symbol(ASR::Function_t *f, size_t i);

// The explicit interface of dummy argument `i` of `f` if it is a dummy
// procedure that can be removed: neither optional nor a pointer. It is either
// declared by an interface body (a Function symbol) or with a procedure
// statement (a Variable of procedure type declared by its interface).
ASR::Function_t *removable_dummy_interface(ASR::Function_t *f, size_t i);

// A dummy argument `a` of the interface of a dummy procedure and the
// corresponding dummy argument `b` of an actual procedure are passed the
// same way.
bool same_dummy(ASR::expr_t *a, ASR::expr_t *b);

// The actual procedure `actual` can be called wherever a call of the dummy
// procedure with the interface `iface` was.
bool same_interface(ASR::Function_t *iface, ASR::Function_t *actual);

// Counts the statements and expressions of a procedure.
class NodeCounter: public ASR::BaseWalkVisitor<NodeCounter> {
public:
    size_t count = 0;

    void visit_stmt(const ASR::stmt_t &x) {
        count++;
        ASR::BaseWalkVisitor<NodeCounter>::visit_stmt(x);
    }

    void visit_expr(const ASR::expr_t &x) {
        count++;
        ASR::BaseWalkVisitor<NodeCounter>::visit_expr(x);
    }
};

// Only variables without saved storage, interfaces, entities of modules and
// BLOCK/ASSOCIATE constructs made of those, and with `internal_procedures`
// also internal procedures made of those.
bool copyable_scope(SymbolTable *scope, bool internal_procedures = false);

// A procedure that can be copied, apart from its dummies: into a host, or
// with `internal_procedures` next to itself, with its internal procedures.
bool copyable_procedure(ASR::Function_t *f, bool internal_procedures = false);

// The procedure with a body that a call by its name runs, or nullptr.
ASR::Function_t *called_procedure(ASR::symbol_t *name, ASR::expr_t *dt);

// A dummy procedure: argument `index` of `procedure`.
typedef std::pair<ASR::Function_t*, size_t> Dummy;

// The uses of the removable dummy procedures of one procedure: every
// reference that is not a call or an actual argument of a call of a
// procedure with a body makes a dummy unremovable.
class DummyUseCollector: public ASR::BaseWalkVisitor<DummyUseCollector> {
public:
    // dummy symbol -> its index
    std::map<ASR::symbol_t*, size_t> dummies;
    // references (Var) and the ones that are allowed
    std::map<ASR::symbol_t*, size_t> references, allowed;
    std::set<ASR::symbol_t*> unremovable;
    std::map<ASR::symbol_t*, std::vector<Dummy>> passed_to;

    void visit_Variable(const ASR::Variable_t &x) {
        if (x.m_type_declaration != nullptr &&
                dummies.count(x.m_type_declaration) > 0) {
            unremovable.insert(x.m_type_declaration);
        }
        ASR::BaseWalkVisitor<DummyUseCollector>::visit_Variable(x);
    }

    void visit_Var(const ASR::Var_t &x) {
        if (dummies.count(x.m_v) > 0) {
            references[x.m_v]++;
        }
    }

    void visit_call(ASR::symbol_t *name, ASR::symbol_t *original_name,
            ASR::call_arg_t *args, size_t n_args, ASR::expr_t *dt) {
        if (dummies.count(name) > 0 && dt != nullptr) {
            unremovable.insert(name);
        }
        if (original_name != nullptr && original_name != name &&
                dummies.count(original_name) > 0) {
            unremovable.insert(original_name);
        }
        ASR::Function_t *callee = called_procedure(name, dt);
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t *arg = args[i].m_value;
            if (arg == nullptr || !ASR::is_a<ASR::Var_t>(*arg)) {
                continue;
            }
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(arg)->m_v;
            if (dummies.count(sym) > 0 && callee != nullptr &&
                    i < callee->n_args) {
                allowed[sym]++;
                passed_to[sym].push_back({callee, i});
            }
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_call(x.m_name, x.m_original_name, x.m_args, x.n_args, x.m_dt);
        ASR::BaseWalkVisitor<DummyUseCollector>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_call(x.m_name, x.m_original_name, x.m_args, x.n_args, x.m_dt);
        ASR::BaseWalkVisitor<DummyUseCollector>::visit_FunctionCall(x);
    }
};

// Every procedure with a body, in the order of the scopes.
void collect_procedures(SymbolTable *scope,
        std::vector<ASR::Function_t*> &procedures);

// Makes every symbol that a copy in a host refers to visible from it, the
// way asr_verify requires: an entity of another module whose name does not
// resolve to it from the copy (it is not visible, or an entity of the host
// with the same name hides it) is referred to through an ExternalSymbol of
// the copy's scope. `ok` is false if some entity cannot be made visible.
class ScopeImporter: public ASR::BaseWalkVisitor<ScopeImporter> {
public:
    Allocator &al;
    SymbolTable *copy_scope;
    SymbolTable *current_scope;
    // the module or program of the host
    ASR::symbol_t *host_unit;
    bool ok = true;
    // entity -> its ExternalSymbol in copy_scope
    std::map<ASR::symbol_t*, ASR::symbol_t*> imports;
    // the modules of the imported entities
    std::set<std::string> modules;

    ScopeImporter(Allocator &al_, SymbolTable *copy_scope_,
            ASR::symbol_t *host_unit_):
        al(al_), copy_scope(copy_scope_), current_scope(copy_scope_),
        host_unit(host_unit_) {}

    ASR::symbol_t *import(ASR::symbol_t *sym) {
        if (sym == nullptr || current_scope->resolve_symbol(
                ASRUtils::symbol_name(sym)) == sym) {
            return sym;
        }
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(sym);
        if (target == nullptr) {
            ok = false;
            return sym;
        }
        auto it = imports.find(target);
        if (it != imports.end()) {
            return it->second;
        }
        ASR::symbol_t *owner = scope_owner(ASRUtils::symbol_parent_symtab(target));
        if (owner == nullptr || !ASR::is_a<ASR::Module_t>(*owner) ||
                owner == host_unit) {
            // An entity of the host's own program unit (such as the
            // internal procedure that replaces a dummy) that a local of
            // the copy hides is still referred to by the copy's nodes.
            if (!visible_from(current_scope, sym)) {
                ok = false;
            }
            return sym;
        }
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(owner);
        std::string name = copy_scope->get_unique_name(
            ASRUtils::symbol_name(target), false);
        ASR::symbol_t *ext = ASR::down_cast<ASR::symbol_t>(
            ASR::make_ExternalSymbol_t(al, target->base.loc, copy_scope,
                s2c(al, name), target, m->m_name, nullptr, 0,
                ASRUtils::symbol_name(target), ASR::accessType::Private));
        copy_scope->add_symbol(name, ext);
        imports[target] = ext;
        modules.insert(m->m_name);
        return ext;
    }

    void require_visible(ASR::symbol_t *sym) {
        if (sym != nullptr && !visible_from(current_scope, sym)) {
            ok = false;
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<ScopeImporter>::visit_Function(x);
        current_scope = parent;
    }

    void visit_Block(const ASR::Block_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<ScopeImporter>::visit_Block(x);
        current_scope = parent;
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<ScopeImporter>::visit_AssociateBlock(x);
        current_scope = parent;
    }

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::Variable_t &xx = const_cast<ASR::Variable_t&>(x);
        xx.m_type_declaration = import(xx.m_type_declaration);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_Variable(x);
    }

    void visit_Var(const ASR::Var_t &x) {
        const_cast<ASR::Var_t&>(x).m_v = import(x.m_v);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        xx.m_name = import(xx.m_name);
        xx.m_original_name = import(xx.m_original_name);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        xx.m_name = import(xx.m_name);
        xx.m_original_name = import(xx.m_original_name);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_FunctionCall(x);
    }

    void visit_FunctionPointerCast(const ASR::FunctionPointerCast_t &x) {
        ASR::FunctionPointerCast_t &xx =
            const_cast<ASR::FunctionPointerCast_t&>(x);
        xx.m_to = import(xx.m_to);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_FunctionPointerCast(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        require_visible(x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        require_visible(x.m_m);
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        for (size_t i = 0; i < x.n_dtio_bindings; i++) {
            require_visible(x.m_dtio_bindings[i].m_procedure);
            require_visible(x.m_dtio_bindings[i].m_callback);
        }
        ASR::BaseWalkVisitor<ScopeImporter>::visit_StringFormat(x);
    }
};

// Makes the module or program that contains `scope` depend on the module
// `module_name`.
void add_module_dependency(Allocator &al, SymbolTable *scope,
        const std::string &module_name);

} // namespace ProcedureArguments

} // namespace LCompilers

#endif // LIBASR_PASS_PROCEDURE_ARGUMENT_UTILS_H
