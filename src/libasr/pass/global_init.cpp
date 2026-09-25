#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/pass/global_init.h>
#include <libasr/pass/pass_utils.h>

#include <functional>
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

    // The module `fn` initializes, when a module is what owns it. Only a
    // module records an ordering: a program's initializer is called from the
    // program body and the translation unit's from the target's startup, and
    // neither of those is a choice a pass makes.
    ASR::Module_t* owning_module(ASR::Function_t *fn) {
        SymbolTable *scope = fn->m_symtab->parent;
        if (scope == nullptr || scope->asr_owner == nullptr) return nullptr;
        if (!ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) return nullptr;
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
        if (!ASR::is_a<ASR::Module_t>(*sym)) return nullptr;
        return ASR::down_cast<ASR::Module_t>(sym);
    }

    // Record what has just been put into `fn`. `global_init_at_startup` is
    // true only while everything the initializer holds is order-insensitive,
    // so one ordered statement takes it away for good.
    void note_ordering(ASR::Function_t *fn, ASRUtils::InitOrdering ordering) {
        if (ordering == ASRUtils::InitOrdering::OrderInsensitive) return;
        ASR::Module_t *m = owning_module(fn);
        if (m != nullptr) m->m_global_init_at_startup = false;
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
    // An empty body is order-insensitive, so a module starts out able to take
    // the startup hook and loses it to the first ordered statement put in.
    // One defined in another object file never takes it here: that object
    // file holds the definition and registers it with its own startup.
    if (in_module && !defined_elsewhere) {
        ASR::down_cast<ASR::Module_t>(ASR::down_cast<ASR::symbol_t>(owner))
            ->m_global_init_at_startup = true;
    }
    return ASR::down_cast<ASR::Function_t>(ASR::down_cast<ASR::symbol_t>(fn));
}

void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
        ASR::stmt_t *stmt, InitOrdering ordering) {
    note_ordering(fn, ordering);
    ASR::If_t *guard = guard_of(fn);
    Vec<ASR::stmt_t*> body;
    body.from_pointer_n_copy(al, guard->m_body, guard->n_body);
    body.push_back(al, stmt);
    guard->m_body = body.p;
    guard->n_body = body.size();
}

void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
        const std::vector<ASR::stmt_t*> &stmts, InitOrdering ordering) {
    if (stmts.empty()) return;
    note_ordering(fn, ordering);
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

// A copy of an expression written in the scope that defines a derived type --
// the default the type gives one of its components -- for a statement of
// `scope`. Every symbol the expression names is replaced by the one through
// which `scope` reaches the same definition: the variables of a pointer's
// initial target designator and the subscripts in it, a component it selects,
// a procedure. Each is imported where `scope` does not already reach it, once
// per definition, so a target named twice, or named in `scope` by a symbol of
// its own that happens to have the same name, gets one import.
class DefaultValueDuplicator: public ASR::BaseExprStmtDuplicator<DefaultValueDuplicator> {
    public:
        SymbolTable *scope;
        std::map<ASR::symbol_t*, ASR::symbol_t*> &imports;

        DefaultValueDuplicator(Allocator &al, SymbolTable *scope_,
                std::map<ASR::symbol_t*, ASR::symbol_t*> &imports_):
            ASR::BaseExprStmtDuplicator<DefaultValueDuplicator>(al),
            scope(scope_), imports(imports_) {}

        ASR::symbol_t* reachable(ASR::symbol_t *sym) {
            ASR::symbol_t *definition = ASRUtils::symbol_get_past_external(sym);
            auto imported = imports.find(definition);
            if (imported != imports.end()) return imported->second;
            ASR::symbol_t *reached = ASRUtils::import_symbol(al, sym, scope);
            // A module's type can only name what the module itself can: its
            // own entities, those it uses, and the procedures of the whole
            // translation unit, each of which `scope` reaches or can import.
            LCOMPILERS_ASSERT(ASRUtils::is_visible_from(reached, scope));
            imports[definition] = reached;
            return reached;
        }

        ASR::asr_t* duplicate_Var(ASR::Var_t *x) {
            return ASR::make_Var_t(al, x->base.base.loc, reachable(x->m_v));
        }

        ASR::asr_t* duplicate_StructInstanceMember(ASR::StructInstanceMember_t *x) {
            ASR::expr_t *v = duplicate_expr(x->m_v);
            ASR::symbol_t *member = ASRUtils::import_struct_instance_member(al,
                x->m_m, scope);
            return ASR::make_StructInstanceMember_t(al, x->base.base.loc, v,
                member, duplicate_ttype(x->m_type), duplicate_expr(x->m_value));
        }
};

class GlobalInitVisitor {

    private:

        Allocator &al;
        ASR::TranslationUnit_t &unit;
        // Each module is compiled into an object file of its own, so a module
        // read back from a `.mod` file is initialized by that object file and
        // not here.
        bool separate_compilation;
        // What the defaults put into an initializer imported into its scope,
        // by definition; see `DefaultValueDuplicator`.
        std::map<SymbolTable*, std::map<ASR::symbol_t*, ASR::symbol_t*>> default_imports;

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
            // A pointer association whose target has a link-time address is
            // laid out by the backend as the variable's own initializer, so
            // it needs no statement here. Everything else about an
            // association -- an array pointer's descriptor above all -- is
            // built from the target rather than being its address, and stays
            // a statement.
            if (is_pointer_initializer(v)) {
                return !ASRUtils::is_static_pointer_association(v);
            }
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
            if (vars.empty() && !has_default_init_stmts(m->m_symtab)) return;
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

        // Default initialization of a module's storage.
        //
        // A module variable of a derived type with no declaration initializer
        // gets its initial value from its type's default initialization. A
        // backend lays a scalar one out as static data holding every default
        // `ASRUtils::struct_member_default_is_static` accepts, however deeply
        // the derived types it holds by value nest, and an array as zeros,
        // whatever its size. Everything that layout does not hold becomes a
        // statement of the module's initializer here: a default of a scalar
        // that is not static -- one in storage created at run time, a
        // string's buffer above all, or a pointer's initial procedure or
        // target -- and every default of an element of an array, given by one
        // loop over the array. So every member is initialized by exactly one
        // of the two, and a target's startup hook only creates the storage
        // the layout does not hold in place.

        // A designator built only once a statement needs it, so that a member
        // with nothing to initialize adds no symbol to the initializer.
        using Designator = std::function<ASR::expr_t*()>;

        // The derived type whose default initialization gives module variable
        // `v` its initial value, held by value as a scalar or as a fixed-size
        // array, or nullptr.
        static ASR::Struct_t* default_initialized_type(const ASR::Variable_t &v) {
            if (v.m_symbolic_value != nullptr || v.m_value != nullptr
                    || v.m_storage == ASR::storage_typeType::Parameter
                    || v.n_codims > 0 || !ASRUtils::is_module_variable(v)) {
                return nullptr;
            }
            return ASRUtils::struct_member_held_by_value(
                const_cast<ASR::Variable_t*>(&v));
        }

        // Whether any variable of `scope` gets a statement of its default
        // initialization.
        bool has_default_init_stmts(SymbolTable *scope) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                ASR::Struct_t *s = default_initialized_type(*v);
                if (s != nullptr && variable_default_init_stmts(v, s, nullptr, nullptr)) {
                    return true;
                }
            }
            return false;
        }

        // Append to `out` the statements of the default initialization of
        // `v`, of type `s`, in `scope`. With `out` null nothing is built, and
        // the result only says whether there is anything to append.
        bool variable_default_init_stmts(ASR::Variable_t *v, ASR::Struct_t *s,
                SymbolTable *scope, std::vector<ASR::stmt_t*> *out) {
            const Location &loc = v->base.base.loc;
            Designator var = [this, v, &loc]() {
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, &v->base));
            };
            if (ASRUtils::is_array(v->m_type)) {
                return array_default_init_stmts(var, v->m_type, s, nullptr,
                    true, scope, loc, out);
            }
            return default_init_stmts(var, s, nullptr, false, scope, loc, out);
        }

        // Append to `out` the statements that give `target`, storage of type
        // `s`, what static data does not hold of its default initialization:
        // all of it when `all` is set, because the storage is laid out as
        // zeros, and otherwise the defaults that are not static. The defaults
        // are those of `constant`, a structure constant of type `s`, where it
        // gives one, and the members' own otherwise. With `out` null nothing
        // is built, and the result only says whether there is anything.
        bool default_init_stmts(const Designator &target, ASR::Struct_t *s,
                ASR::expr_t *constant, bool all, SymbolTable *scope,
                const Location &loc, std::vector<ASR::stmt_t*> *out) {
            // The members of the parent types come first, as in a structure
            // constant, so the statements run in declaration order.
            std::vector<ASR::Struct_t*> chain;
            for (ASR::Struct_t *c = s; c != nullptr;
                    c = c->m_parent == nullptr ? nullptr
                        : ASR::down_cast<ASR::Struct_t>(
                            ASRUtils::symbol_get_past_external(c->m_parent))) {
                chain.insert(chain.begin(), c);
            }
            bool any = false;
            for (ASR::Struct_t *c : chain) {
                for (size_t i = 0; i < c->n_members; i++) {
                    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                        c->m_symtab->get_symbol(c->m_members[i]));
                    if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
                    ASR::Variable_t *m = ASR::down_cast<ASR::Variable_t>(sym);
                    ASR::expr_t *value = constant == nullptr ? nullptr
                        : ASRUtils::get_struct_member_value_from_constant(
                            constant, sym);
                    if (value == nullptr) {
                        value = m->m_value != nullptr ? m->m_value
                            : m->m_symbolic_value;
                    }
                    Designator member = [this, &target, sym, scope, &loc]() {
                        return ASRUtils::EXPR(ASRUtils::getStructInstanceMember_t(
                            al, loc, (ASR::asr_t*)target(), nullptr, sym, scope));
                    };
                    bool has;
                    if (ASR::Struct_t *held = ASRUtils::struct_member_held_by_value(m)) {
                        if (ASRUtils::is_array(m->m_type)) {
                            has = array_default_init_stmts(member, m->m_type,
                                held, value, all, scope, loc, out);
                        } else {
                            ASR::expr_t *nested = folded(value);
                            if (nested != nullptr
                                    && !ASR::is_a<ASR::StructConstant_t>(*nested)) {
                                nested = nullptr;
                            }
                            has = default_init_stmts(member, held, nested, all,
                                scope, loc, out);
                        }
                    } else {
                        has = value != nullptr
                            && !ASRUtils::is_allocatable(m->m_type)
                            && !ASR::is_a<ASR::PointerNullConstant_t>(*value)
                            && (all || !ASRUtils::struct_member_default_is_static(
                                c, m, value));
                        if (has && out != nullptr) {
                            out->push_back(default_stmt(member(), m, value,
                                scope, loc));
                        }
                    }
                    any = any || has;
                    if (any && out == nullptr) return true;
                }
            }
            return any;
        }

        // `default_init_stmts` for each element of `array`, a fixed-size array
        // of type `s` whose type is `array_type`, with the defaults of
        // `value`, the array's own default, where it gives one.
        bool array_default_init_stmts(const Designator &array,
                ASR::ttype_t *array_type, ASR::Struct_t *s, ASR::expr_t *value,
                bool all, SymbolTable *scope, const Location &loc,
                std::vector<ASR::stmt_t*> *out) {
            ASR::dimension_t *dims = nullptr;
            int n_dims = ASRUtils::extract_dimensions_from_ttype(array_type, dims);
            std::vector<int64_t> starts, lengths;
            for (int d = 0; d < n_dims; d++) {
                int64_t start = 0, length = 0;
                [[maybe_unused]] bool is_constant = ASRUtils::extract_value(
                        ASRUtils::expr_value(dims[d].m_start), start)
                    && ASRUtils::extract_value(
                        ASRUtils::expr_value(dims[d].m_length), length);
                LCOMPILERS_ASSERT(is_constant);
                starts.push_back(start);
                lengths.push_back(length);
            }
            ASR::expr_t *element_value = folded(value);
            if (element_value != nullptr
                    && ASR::is_a<ASR::ArrayConstant_t>(*element_value)) {
                // A default element by element: each element is its own
                // constant, at a constant index.
                ASR::ArrayConstant_t *elements =
                    ASR::down_cast<ASR::ArrayConstant_t>(element_value);
                bool any = false;
                int64_t n = ASRUtils::get_fixed_size_of_array(array_type);
                for (int64_t k = 0; k < n; k++) {
                    std::vector<ASR::expr_t*> index;
                    for (int64_t d = 0, rest = k; d < n_dims; d++) {
                        index.push_back(index_constant(
                            starts[d] + rest % lengths[d], loc));
                        rest /= lengths[d];
                    }
                    Designator element = [this, &array, index]() {
                        ASRUtils::ASRBuilder b(al, array()->base.loc);
                        return b.ArrayItem_01(array(), index);
                    };
                    ASR::expr_t *element_constant = folded(
                        ASRUtils::fetch_ArrayConstant_value(al, elements, k));
                    any = default_init_stmts(element, s, element_constant, all,
                        scope, loc, out) || any;
                    if (any && out == nullptr) return true;
                }
                return any;
            }
            if (element_value != nullptr
                    && ASR::is_a<ASR::ArrayBroadcast_t>(*element_value)) {
                element_value = folded(
                    ASR::down_cast<ASR::ArrayBroadcast_t>(element_value)->m_array);
            }
            if (element_value != nullptr
                    && !ASR::is_a<ASR::StructConstant_t>(*element_value)) {
                element_value = nullptr;
            }
            // One loop over the array, created with its index variables only
            // once an element has a statement to put in it.
            Vec<ASR::expr_t*> index;
            index.reserve(al, n_dims);
            Designator element = [this, &array, &index, n_dims, scope, &loc]() {
                if (index.size() == 0) {
                    SymbolTable *index_scope = scope;
                    PassUtils::create_idx_vars(index, n_dims, loc, al,
                        index_scope, "_default_init_idx");
                }
                return PassUtils::create_array_ref(array(), index, al, scope);
            };
            std::vector<ASR::stmt_t*> body;
            bool has = default_init_stmts(element, s, element_value, all, scope,
                loc, out == nullptr ? nullptr : &body);
            if (has && out != nullptr) {
                ASRUtils::ASRBuilder b(al, loc);
                for (int d = 0; d < n_dims; d++) {
                    ASR::stmt_t *loop = b.DoLoop(index[d],
                        index_constant(starts[d], loc),
                        index_constant(starts[d] + lengths[d] - 1, loc), body);
                    body = {loop};
                }
                out->push_back(body[0]);
            }
            return has;
        }

        // `e` as its compile-time value, where it has one.
        static ASR::expr_t* folded(ASR::expr_t *e) {
            ASR::expr_t *value = e == nullptr ? nullptr : ASRUtils::expr_value(e);
            return value != nullptr ? value : e;
        }

        ASR::expr_t* index_constant(int64_t n, const Location &loc) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, n,
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
                ASR::integerbozType::Decimal));
        }

        // The statement that gives `target`, member `m`, its default `value`:
        // an association for a pointer, an assignment otherwise. The default
        // is written in the scope that defines the type, so the statement
        // gets a copy of it that names everything it refers to through
        // symbols `scope` reaches.
        ASR::stmt_t* default_stmt(ASR::expr_t *target, ASR::Variable_t *m,
                ASR::expr_t *value, SymbolTable *scope, const Location &loc) {
            DefaultValueDuplicator duplicator(al, scope, default_imports[scope]);
            if (ASRUtils::is_pointer(m->m_type)) {
                return ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, loc,
                    target, duplicator.duplicate_expr(value)));
            }
            return ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc,
                target, duplicator.duplicate_expr(folded(value)), nullptr,
                false, false));
        }

        // Move every declaration initializer of `owner`'s scope that needs
        // executable code into `owner`'s initializer, and, for a module, put
        // there the default initialization of its variables that static data
        // does not hold, all in declaration order.
        void lower_scope(ASR::asr_t *owner, SymbolTable *scope) {
            ASR::Function_t *fn = nullptr;
            auto initializer = [&]() {
                if (fn == nullptr) {
                    fn = ASRUtils::get_or_create_global_init(al, unit, owner);
                }
                return fn;
            };
            for (auto &name : ASRUtils::determine_variable_declaration_order(scope)) {
                ASR::symbol_t *sym = scope->get_symbol(name);
                if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                // A declaration initializer and a default are each an
                // assignment or an association that reads a constant or the
                // address of a variable with `save` or of a procedure, and
                // nothing else.
                if (needs_runtime_init(*v)) {
                    ASRUtils::global_init_append_stmt(al, initializer(),
                        take_initializer(v), ASRUtils::InitOrdering::OrderInsensitive);
                    continue;
                }
                ASR::Struct_t *s = default_initialized_type(*v);
                if (s == nullptr || !variable_default_init_stmts(v, s, nullptr, nullptr)) {
                    continue;
                }
                std::vector<ASR::stmt_t*> stmts;
                variable_default_init_stmts(v, s, initializer()->m_symtab, &stmts);
                for (ASR::stmt_t *stmt : stmts) {
                    ASRUtils::global_init_append_stmt(al, fn, stmt,
                        ASRUtils::InitOrdering::OrderInsensitive);
                }
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
            // Calls, so `Ordered` — the program's own initializer is
            // called from the program body and never from a startup hook,
            // which is why this changes nothing here.
            ASRUtils::global_init_prepend_stmts(al, own, calls,
                ASRUtils::InitOrdering::Ordered);

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
// A module initializer keeps its guard when `global_init_at_startup` says the
// target's own startup runs it too, in the object file that defines the
// module, which is what initializes the module when no Fortran main program
// exists to root that chain. It is then reached twice by construction, and
// the guard is what makes those two into one initialization. A module the
// startup does not run is called once from the chain like a program, so its
// guard goes the same way.
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
        } else if (ASR::is_a<ASR::Module_t>(*item.second)) {
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(item.second);
            if (!m->m_global_init_at_startup) {
                strip_of(m->m_global_init, m->m_symtab);
            }
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
