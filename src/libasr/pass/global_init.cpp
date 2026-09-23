#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/pass/global_init.h>
#include <libasr/pass/pass_utils.h>

#include <map>
#include <string>
#include <utility>
#include <vector>

namespace LCompilers {

namespace ASRUtils {

namespace {

    const std::string global_init_prefix = "__lfortran_global_init_";
    const std::string global_init_guard_name = "__lfortran_global_init_done";

    // The scope the initializer of `owner` lives in, and the name it gets.
    // A module keeps its initializer among its own symbols so that separate
    // compilation exports it with the module, a program keeps its own, and an
    // initializer that belongs to no program unit lives in the global scope.
    SymbolTable* owner_symtab(ASR::TranslationUnit_t &unit, ASR::asr_t *owner) {
        if (owner == (ASR::asr_t*)&unit) {
            return unit.m_symtab;
        }
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
        return ASRUtils::symbol_symtab(sym);
    }

    // A module and a program are named once in a whole program, so naming
    // their initializers after them is enough to keep them apart at link
    // time. The translation unit is not: an initializer owned by one needs a
    // name no other object file can produce, which is why the coarray pass
    // builds its own out of the names of the coarrays it allocates instead of
    // asking for one here.
    std::string owner_name([[maybe_unused]] ASR::TranslationUnit_t &unit,
            ASR::asr_t *owner) {
        LCOMPILERS_ASSERT(owner != (ASR::asr_t*)&unit);
        return ASRUtils::symbol_name(ASR::down_cast<ASR::symbol_t>(owner));
    }

    char** owner_global_init(ASR::TranslationUnit_t &unit, ASR::asr_t *owner) {
        if (owner == (ASR::asr_t*)&unit) {
            return &unit.m_global_init;
        }
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
        switch (sym->type) {
            case ASR::symbolType::Module:
                return &ASR::down_cast<ASR::Module_t>(sym)->m_global_init;
            case ASR::symbolType::Program:
                return &ASR::down_cast<ASR::Program_t>(sym)->m_global_init;
            default:
                throw LCompilersException(
                    "Only a module, a program or the translation unit can own "
                    "a global initializer");
        }
    }

    // A saved logical of `scope`, false until the initialization it guards
    // has run. It is created together with what it guards, so it can never
    // clash with a user symbol of that scope.
    ASR::expr_t* make_run_once_guard(Allocator &al, SymbolTable *scope,
            const Location &loc, const std::string &name) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        ASR::symbol_t *guard_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                nullptr, 0, ASR::intentType::Local, b.bool_t(false, logical_type),
                b.bool_t(false, logical_type), ASR::storage_typeType::Save,
                logical_type, nullptr, ASR::abiType::Source, ASR::accessType::Private,
                ASR::presenceType::Required, false));
        scope->add_symbol(name, guard_sym);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, guard_sym));
    }

    // Mark the guard as taken, so whatever follows it inside the guarded
    // block runs exactly once.
    ASR::stmt_t* mark_run_stmt(Allocator &al, ASR::expr_t *guard,
            const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        return ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, guard,
            b.bool_t(true, ASRUtils::expr_type(guard)), nullptr, false, false));
    }

    // The guard `if` is the only statement of the body, so it is where every
    // later statement is added.
    ASR::If_t* guard_of(ASR::Function_t *fn) {
        LCOMPILERS_ASSERT(fn->n_body == 1 && ASR::is_a<ASR::If_t>(*fn->m_body[0]));
        return ASR::down_cast<ASR::If_t>(fn->m_body[0]);
    }

} // anonymous namespace

ASR::Function_t* get_or_create_global_init(Allocator &al,
        ASR::TranslationUnit_t &unit, ASR::asr_t *owner,
        bool defined_elsewhere) {
    SymbolTable *scope = owner_symtab(unit, owner);
    char **global_init = owner_global_init(unit, owner);
    if (*global_init != nullptr) {
        ASR::symbol_t *sym = scope->get_symbol(*global_init);
        LCOMPILERS_ASSERT(sym && ASR::is_a<ASR::Function_t>(*sym));
        return ASR::down_cast<ASR::Function_t>(sym);
    }

    const Location &loc = unit.base.base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    std::string fn_name = scope->get_unique_name(
        global_init_prefix + owner_name(unit, owner), false);
    SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);

    // The guard is a saved local of the initializer itself, so it can never
    // clash with a user symbol of the owning scope.
    ASR::expr_t *guard = make_run_once_guard(al, fn_symtab, loc,
        global_init_guard_name);

    Vec<ASR::stmt_t*> body; body.reserve(al, 1);
    body.push_back(al, b.If(b.Not(guard), {mark_run_stmt(al, guard, loc)}, {}));

    // A module procedure, when a module owns it: that is what gives the
    // declaration of one defined in another object file the same link name as
    // the definition there.
    bool in_module = owner != (ASR::asr_t*)&unit
        && ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(owner));
    ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc, fn_symtab,
        s2c(al, fn_name), nullptr, 0, nullptr, 0,
        defined_elsewhere ? nullptr : body.p,
        defined_elsewhere ? 0 : body.n, nullptr,
        defined_elsewhere ? ASR::abiType::ExternalUndefined : ASR::abiType::Source,
        ASR::accessType::Public,
        defined_elsewhere ? ASR::deftypeType::Interface
                          : ASR::deftypeType::Implementation, nullptr,
        false, false, in_module, false, false, nullptr, 0,
        false, false, false, nullptr);
    scope->add_symbol(fn_name, ASR::down_cast<ASR::symbol_t>(fn));
    *global_init = s2c(al, fn_name);
    return ASR::down_cast<ASR::Function_t>(ASR::down_cast<ASR::symbol_t>(fn));
}

void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
        ASR::stmt_t *stmt) {
    ASR::If_t *guard = guard_of(fn);
    Vec<ASR::stmt_t*> body;
    body.from_pointer_n_copy(al, guard->m_body, guard->n_body);
    body.push_back(al, stmt);
    guard->m_body = body.p;
    guard->n_body = body.size();
}

void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
        const std::vector<ASR::stmt_t*> &stmts) {
    if (stmts.empty()) return;
    ASR::If_t *guard = guard_of(fn);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, guard->n_body + stmts.size());
    // The statement marking the initializer as run stays first, so a cycle in
    // the dependency calls added after it terminates.
    LCOMPILERS_ASSERT(guard->n_body >= 1);
    body.push_back(al, guard->m_body[0]);
    for (ASR::stmt_t *s : stmts) body.push_back(al, s);
    for (size_t i = 1; i < guard->n_body; i++) body.push_back(al, guard->m_body[i]);
    guard->m_body = body.p;
    guard->n_body = body.size();
}

} // namespace ASRUtils

namespace {

// The initializer `sym` names, or nullptr if it needs none.
ASR::Function_t* global_init_of(ASR::symbol_t *sym) {
    char *name = nullptr;
    SymbolTable *scope = nullptr;
    if (ASR::is_a<ASR::Module_t>(*sym)) {
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
        name = m->m_global_init;
        scope = m->m_symtab;
    } else if (ASR::is_a<ASR::Program_t>(*sym)) {
        ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
        name = p->m_global_init;
        scope = p->m_symtab;
    }
    if (name == nullptr) return nullptr;
    ASR::symbol_t *fn = scope->get_symbol(name);
    if (fn == nullptr || !ASR::is_a<ASR::Function_t>(*fn)) return nullptr;
    return ASR::down_cast<ASR::Function_t>(fn);
}

class GlobalInitVisitor {

    private:

        Allocator &al;
        ASR::TranslationUnit_t &unit;
        // Each module is compiled into an object file of its own, so a module
        // read back from a `.mod` file is initialized by that object file and
        // not here.
        bool separate_compilation;

    public:

        GlobalInitVisitor(Allocator &al_, ASR::TranslationUnit_t &unit_,
                bool separate_compilation_):
            al(al_), unit(unit_), separate_compilation(separate_compilation_) {}

        // A declaration initializer that no target can lay out as static data
        // and that therefore has to be assigned by executable statements.
        // Everything a backend can still emit as a constant — an integer
        // array constant, a character array constant — is deliberately left
        // alone, so this only grows as cases are found that need it.
        bool needs_runtime_init(const ASR::Variable_t &v) {
            if (v.m_symbolic_value == nullptr) return false;
            if (v.m_storage == ASR::storage_typeType::Parameter) return false;
            ASR::expr_t *init = v.m_symbolic_value;
            if (is_pointer_initializer(v)) return true;
            if (ASR::is_a<ASR::Cast_t>(*init)) {
                init = ASR::down_cast<ASR::Cast_t>(init)->m_arg;
            }
            if (!ASR::is_a<ASR::ArrayBroadcast_t>(*init)) return false;
            ASR::ttype_t *type = ASRUtils::expr_type(init);
            if (!ASRUtils::is_array(type) ||
                    !ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::type_get_past_array(type))) {
                return false;
            }
            // An element a backend can describe with static data is left on
            // the declaration for it to lay out. That is what lets a
            // specification expression of a later variable read the value:
            // bounds are evaluated while the variable is laid out, before any
            // statement of the body runs. Only an element that needs
            // executable code of its own — a string, array or class member —
            // becomes a statement here.
            ASR::expr_t *var_expr = ASRUtils::EXPR(ASR::make_Var_t(
                al, v.base.base.loc,
                const_cast<ASR::symbol_t*>(&v.base)));
            return ASRUtils::needs_struct_array_member_init(var_expr, v.m_type);
        }

        // The variables of `scope` that need executable initialization, in
        // declaration order so that an initializer reading another variable
        // of the same scope sees it already set.
        std::vector<ASR::Variable_t*> runtime_init_vars(SymbolTable *scope) {
            std::vector<ASR::Variable_t*> vars;
            for (auto &name : ASRUtils::determine_variable_declaration_order(scope)) {
                ASR::symbol_t *sym = scope->get_symbol(name);
                if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                if (needs_runtime_init(*v)) vars.push_back(v);
            }
            return vars;
        }

        // `p => tgt` in a declaration: an association rather than a value,
        // which is why no target can lay it out as static data. `=> null()`
        // is a value and is deliberately not one of these.
        static bool is_pointer_initializer(const ASR::Variable_t &v) {
            return ASRUtils::is_pointer(v.m_type)
                && ASRUtils::is_pointer_association_initializer(
                    v.m_symbolic_value);
        }

        // Take the declaration initializer off `v` and return it as the
        // statement that replaces it.
        ASR::stmt_t* take_initializer(ASR::Variable_t *v) {
            const Location &loc = v->base.base.loc;
            ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(
                al, loc, &v->base));
            ASR::stmt_t *stmt;
            if (is_pointer_initializer(*v)) {
                stmt = ASRUtils::STMT(ASRUtils::make_Associate_t_util(
                    al, loc, target, v->m_symbolic_value));
            } else {
                stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, loc, target, v->m_symbolic_value, nullptr, false, false));
            }
            v->m_symbolic_value = nullptr;
            v->m_value = nullptr;
            return stmt;
        }

        // A module compiled into an object file of its own initializes itself
        // there. Give it the same initializer name this pass would have given
        // it in that object file, as a declaration, so a call from here
        // resolves to the one definition at link time.
        void name_external_global_init(ASR::Module_t *m) {
            std::vector<ASR::Variable_t*> vars = runtime_init_vars(m->m_symtab);
            if (vars.empty()) return;
            ASRUtils::get_or_create_global_init(al, unit, (ASR::asr_t*)&m->base,
                true);
            // Drop the initializers here as well. This translation unit only
            // declares these variables; the object file that defines them is
            // the one that initializes them, through the initializer just
            // named. Leaving them on would tell this unit that the variables
            // are initialized where they are declared, which is what decides
            // whether a backend sets an element's members up at start up: it
            // would then leave that to an initializer that is not in this
            // unit and is not emitted by the unit that is.
            for (ASR::Variable_t *v : vars) {
                v->m_symbolic_value = nullptr;
                v->m_value = nullptr;
            }
        }

        // Move every declaration initializer of `owner`'s scope that needs
        // executable code into `owner`'s initializer.
        void lower_scope(ASR::asr_t *owner, SymbolTable *scope) {
            ASR::Function_t *fn = nullptr;
            for (ASR::Variable_t *v : runtime_init_vars(scope)) {
                if (fn == nullptr) {
                    fn = ASRUtils::get_or_create_global_init(al, unit, owner);
                }
                ASRUtils::global_init_append_stmt(al, fn, take_initializer(v));
            }
        }

        // A procedure or a block initializes its own variables at the top of
        // its own body instead of through an initializer procedure: nothing
        // outside it can observe them, so there is nobody to call one. The
        // variables are saved — Fortran gives every initialized local the
        // save attribute — so the block runs once, the first time control
        // reaches it.
        template <typename T>
        void lower_local_scope(T *owner) {
            std::vector<ASR::Variable_t*> vars = runtime_init_vars(owner->m_symtab);
            if (vars.empty()) return;
            const Location &loc = owner->base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *guard = ASRUtils::make_run_once_guard(al, owner->m_symtab,
                loc, owner->m_symtab->get_unique_name(
                    ASRUtils::global_init_guard_name, false));
            std::vector<ASR::stmt_t*> guarded;
            guarded.push_back(ASRUtils::mark_run_stmt(al, guard, loc));
            for (ASR::Variable_t *v : vars) guarded.push_back(take_initializer(v));
            Vec<ASR::stmt_t*> body;
            body.reserve(al, owner->n_body + 1);
            body.push_back(al, b.If(b.Not(guard), guarded, {}));
            for (size_t i = 0; i < owner->n_body; i++) {
                body.push_back(al, owner->m_body[i]);
            }
            owner->m_body = body.p;
            owner->n_body = body.size();
        }

        // Every procedure and block of `scope`, however deeply nested.
        void lower_local_scopes(SymbolTable *scope) {
            std::vector<ASR::symbol_t*> syms;
            for (auto &item : scope->get_scope()) syms.push_back(item.second);
            for (ASR::symbol_t *sym : syms) {
                if (ASR::is_a<ASR::Function_t>(*sym)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
                    if (ASRUtils::get_FunctionType(f)->m_deftype
                            != ASR::deftypeType::Implementation) continue;
                    lower_local_scope(f);
                    lower_local_scopes(f->m_symtab);
                } else if (ASR::is_a<ASR::Block_t>(*sym)) {
                    ASR::Block_t *bl = ASR::down_cast<ASR::Block_t>(sym);
                    lower_local_scope(bl);
                    lower_local_scopes(bl->m_symtab);
                } else if (ASR::is_a<ASR::AssociateBlock_t>(*sym)) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(sym);
                    lower_local_scope(ab);
                    lower_local_scopes(ab->m_symtab);
                }
            }
        }

        void visit_TranslationUnit() {
            // Dependency order, computed from ASR and therefore the same on
            // every image and in every link order.
            std::vector<std::string> module_order =
                ASRUtils::determine_module_dependencies(unit);

            for (auto &name : module_order) {
                ASR::symbol_t *sym = unit.m_symtab->get_symbol(name);
                if (sym == nullptr || !ASR::is_a<ASR::Module_t>(*sym)) continue;
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
                if (separate_compilation && m->m_loaded_from_mod) {
                    // The module's own object file initializes it. Name the
                    // initializer so it can be called from here, but leave
                    // the one definition where it is.
                    name_external_global_init(m);
                    continue;
                }
                // Otherwise the module's variables are emitted into every
                // translation unit that uses it, so the initializer has to be
                // defined in each of them as well.
                lower_scope((ASR::asr_t*)sym, m->m_symtab);
            }

            std::vector<ASR::symbol_t*> programs;
            for (auto &item : unit.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Program_t>(*item.second)) {
                    programs.push_back(item.second);
                }
            }
            for (ASR::symbol_t *sym : programs) {
                lower_scope((ASR::asr_t*)sym, ASR::down_cast<ASR::Program_t>(
                    sym)->m_symtab);
            }

            // Procedures and blocks come last: the initializers created above
            // own nothing that needs initializing, so walking into them now
            // costs nothing and the walk sees a settled symbol table.
            lower_local_scopes(unit.m_symtab);
            for (auto &item : unit.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Module_t>(*item.second)) {
                    lower_local_scopes(ASR::down_cast<ASR::Module_t>(
                        item.second)->m_symtab);
                } else if (ASR::is_a<ASR::Program_t>(*item.second)) {
                    lower_local_scopes(ASR::down_cast<ASR::Program_t>(
                        item.second)->m_symtab);
                }
            }
        }

};


// Connecting the initializers is a pass of its own, run after everything that
// can create one. The `coarray` pass creates initializers too, and an
// initializer nothing calls would never run.
class GlobalInitWireVisitor {

    private:

        Allocator &al;
        ASR::TranslationUnit_t &unit;
        // One import per (scope, initializer): a scope that calls the same
        // initializer twice must not gain two external symbols for it.
        std::map<std::pair<SymbolTable*, ASR::Function_t*>, ASR::symbol_t*> imports;

    public:

        GlobalInitWireVisitor(Allocator &al_, ASR::TranslationUnit_t &unit_):
            al(al_), unit(unit_) {}

        // A call to `callee` written in `scope`, importing it if it belongs to
        // another module.
        ASR::stmt_t* call_of(SymbolTable *scope, ASR::Function_t *callee,
                             const Location &loc) {
            ASR::symbol_t *sym = &callee->base;
            SymbolTable *decl_scope = callee->m_symtab->parent;
            if (decl_scope != scope && decl_scope->asr_owner &&
                    ASR::is_a<ASR::symbol_t>(*decl_scope->asr_owner) &&
                    ASR::is_a<ASR::Module_t>(
                        *ASR::down_cast<ASR::symbol_t>(decl_scope->asr_owner))) {
                std::string mod_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::symbol_t>(decl_scope->asr_owner));
                std::string sym_name = ASRUtils::symbol_name(sym);
                auto imported = imports.find({scope, callee});
                if (imported != imports.end()) {
                    sym = imported->second;
                } else {
                    std::string local = scope->get_unique_name(sym_name, false);
                    sym = ASR::down_cast<ASR::symbol_t>(
                        ASR::make_ExternalSymbol_t(al, loc, scope, s2c(al, local),
                            &callee->base, s2c(al, mod_name), nullptr, 0,
                            s2c(al, sym_name), ASR::accessType::Private));
                    scope->add_symbol(local, sym);
                    imports[{scope, callee}] = sym;
                }
            }
            Vec<ASR::call_arg_t> args; args.reserve(al, 1);
            return ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, loc, sym, sym, args.p, args.n, nullptr, nullptr, false));
        }

        void visit_TranslationUnit() {
            // Dependency order, computed from ASR and therefore the same on
            // every image and in every link order.
            std::vector<std::string> module_order =
                ASRUtils::determine_module_dependencies(unit);

            for (auto &item : unit.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Program_t>(*item.second)) continue;
                wire_program(ASR::down_cast<ASR::Program_t>(item.second),
                    module_order);
            }
        }

        // Every initializer the program can observe runs before its first
        // statement, modules first and in dependency order. The module calls
        // go inside the program's own initializer, so the program body gains
        // exactly one statement however many modules there are.
        void wire_program(ASR::Program_t *p,
                const std::vector<std::string> &module_order) {
            std::vector<ASR::Function_t*> module_inits;
            for (auto &name : module_order) {
                ASR::symbol_t *sym = unit.m_symtab->get_symbol(name);
                if (sym == nullptr || !ASR::is_a<ASR::Module_t>(*sym)) continue;
                ASR::Function_t *fn = global_init_of(sym);
                if (fn != nullptr) module_inits.push_back(fn);
            }
            if (module_inits.empty() && global_init_of(&p->base) == nullptr) {
                return;
            }
            const Location &loc = p->base.base.loc;
            ASR::Function_t *own = ASRUtils::get_or_create_global_init(
                al, unit, (ASR::asr_t*)&p->base);
            std::vector<ASR::stmt_t*> calls;
            for (ASR::Function_t *fn : module_inits) {
                calls.push_back(call_of(own->m_symtab, fn, loc));
            }
            ASRUtils::global_init_prepend_stmts(al, own, calls);

            Vec<ASR::stmt_t*> body;
            body.reserve(al, p->n_body + 1);
            body.push_back(al, call_of(p->m_symtab, own, loc));
            for (size_t i = 0; i < p->n_body; i++) body.push_back(al, p->m_body[i]);
            p->m_body = body.p;
            p->n_body = body.size();
        }

};

} // anonymous namespace

void pass_global_init(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GlobalInitVisitor v(al, unit, pass_options.separate_compilation);
    v.visit_TranslationUnit();
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

namespace {

// Under `--fast` the run-once guard of a *program* initializer is dead
// weight, and so is the guard of the translation unit's own: the first is
// called once from the program body, the second once by the target's startup
// hook. Unwrap those, so the body is the initialization statements
// themselves.
//
// A module initializer keeps its guard in every mode. It is reached twice by
// construction: once from the chain this pass roots at the program, and once
// from the startup constructor the backend registers in the object file that
// defines the module, which is what initializes the module when no Fortran
// main program exists to root that chain. The guard is what makes those two
// into one initialization.
//
// The guard at the top of a procedure or block body is a different thing —
// it is what gives an initialized local the save attribute Fortran requires,
// so it decides behaviour rather than merely repeating a call that cannot
// happen. It is never touched here, in any mode.
void strip_run_once_guard(Allocator &al, ASR::Function_t *fn) {
    if (fn->n_body != 1 || !ASR::is_a<ASR::If_t>(*fn->m_body[0])) return;
    ASR::If_t *guard = ASR::down_cast<ASR::If_t>(fn->m_body[0]);
    // The first statement inside is the one that marks the guard as taken.
    LCOMPILERS_ASSERT(guard->n_body >= 1);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, guard->n_body - 1);
    for (size_t i = 1; i < guard->n_body; i++) body.push_back(al, guard->m_body[i]);
    fn->m_body = body.p;
    fn->n_body = body.size();
    fn->m_symtab->erase_symbol(ASRUtils::global_init_guard_name);
}

void strip_run_once_guards(ASR::TranslationUnit_t &unit, Allocator &al) {
    auto strip_of = [&](char *name, SymbolTable *scope) {
        if (name == nullptr) return;
        ASR::symbol_t *sym = scope->get_symbol(name);
        if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)) return;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
        // A declaration of an initializer defined in another object file has
        // no body to unwrap.
        if (fn->n_body == 0) return;
        strip_run_once_guard(al, fn);
    };
    strip_of(unit.m_global_init, unit.m_symtab);
    for (auto &item : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Program_t>(*item.second)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(item.second);
            strip_of(p->m_global_init, p->m_symtab);
        }
    }
}

} // anonymous namespace

void pass_global_init_wire(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GlobalInitWireVisitor v(al, unit);
    v.visit_TranslationUnit();
    if (pass_options.fast) strip_run_once_guards(unit, al);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
