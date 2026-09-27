#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/pass/global_init.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/diagnostics.h>

#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace LCompilers {

namespace ASRUtils {

namespace {

    // Some targets (C, C++) do not qualify the names of module procedures
    // and variables by their module, so an initializer and its state are
    // named after their owner: deterministically, so that every compilation
    // names them alike, and never with a counter.
    const std::string global_init_name = "__lcompilers_global_init_";
    const std::string global_init_state_name = "__lcompilers_global_init_state_";

    // The part of the names of `owner`'s initializer and state that tells
    // it apart from every other owner of a program.
    std::string owner_name_part(ASR::asr_t *owner) {
        if (owner->type == ASR::asrType::unit) return "tu";
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
        if (ASR::is_a<ASR::Module_t>(*sym)) {
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
            if (m->m_parent_module != nullptr) {
                return std::string(m->m_parent_module) + "_" + m->m_name;
            }
            return m->m_name;
        }
        return ASRUtils::symbol_name(sym);
    }

    // `name` in `scope`, which nothing else of it can be called: a Fortran
    // name cannot begin with an underscore.
    std::string reserved_name(SymbolTable *scope, const std::string &name) {
        if (scope->get_symbol(name) != nullptr) {
            throw LCompilersException("the startup initialization name '"
                + name + "' is already taken");
        }
        return name;
    }
    // The guard of the initialized saved locals of a procedure or a block.
    const std::string local_init_guard_name = "__lfortran_global_init_done";

    bool is_translation_unit(const ASR::asr_t *owner) {
        return owner->type == ASR::asrType::unit;
    }

    SymbolTable* owner_symtab(ASR::asr_t *owner) {
        if (is_translation_unit(owner)) {
            return ASR::down_cast2<ASR::TranslationUnit_t>(owner)->m_symtab;
        }
        return ASRUtils::symbol_symtab(ASR::down_cast<ASR::symbol_t>(owner));
    }

    // The two links of `owner`, or nullptr for something that cannot own an
    // initializer.
    std::pair<char**, char**> owner_links(ASR::asr_t *owner) {
        if (is_translation_unit(owner)) {
            ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(owner);
            return {&tu->m_global_init, &tu->m_global_init_state};
        }
        if (owner->type != ASR::asrType::symbol) return {nullptr, nullptr};
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
        switch (sym->type) {
            case ASR::symbolType::Module: {
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
                return {&m->m_global_init, &m->m_global_init_state};
            }
            case ASR::symbolType::Program: {
                ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
                return {&p->m_global_init, &p->m_global_init_state};
            }
            default:
                return {nullptr, nullptr};
        }
    }

    ASR::ttype_t* state_type(Allocator &al, const Location &loc) {
        return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    }

    ASR::expr_t* int32_constant(Allocator &al, const Location &loc, int64_t n) {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, n,
            state_type(al, loc), ASR::integerbozType::Decimal));
    }

    // Create the state word of an initializer in `scope`: saved, private and
    // statically `lcompilers_init_uninitialized`.
    char* create_state(Allocator &al, SymbolTable *scope, const Location &loc,
            const std::string &state_name, ASR::abiType abi) {
        std::string name = reserved_name(scope, state_name);
        ASR::expr_t *zero = int32_constant(al, loc, 0);
        ASR::symbol_t *state = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                nullptr, 0, ASR::intentType::Local, zero, zero,
                ASR::storage_typeType::Save, state_type(al, loc), nullptr, abi,
                ASR::accessType::Private, ASR::presenceType::Required, false));
        scope->add_symbol(name, state);
        return s2c(al, name);
    }

    // Create an initializer with an empty body in `scope`: an ordinary
    // procedure of its owner, never a separate module procedure, which is
    // what `module` in a procedure's type says.
    char* create_initializer(Allocator &al, SymbolTable *scope,
            const Location &loc, const std::string &name) {
        std::string fn_name = reserved_name(scope, name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc, fn_symtab,
            s2c(al, fn_name), nullptr, 0, nullptr, 0, nullptr, 0, nullptr,
            ASR::abiType::Source, ASR::accessType::Private,
            ASR::deftypeType::Implementation, nullptr,
            false, false, false, false, false, nullptr, 0,
            false, false, false, nullptr);
        scope->add_symbol(fn_name, ASR::down_cast<ASR::symbol_t>(fn));
        return s2c(al, fn_name);
    }

    // A saved logical of `scope`, false until the initialization it guards
    // has run: the save attribute of the initialized locals of a procedure or
    // a block, which initialize on the first entry.
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

    ASR::stmt_t* mark_run_stmt(Allocator &al, ASR::expr_t *guard,
            const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        return ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, guard,
            b.bool_t(true, ASRUtils::expr_type(guard)), nullptr, false, false));
    }

    const char* init_runtime_function_name(InitRuntimeFn kind) {
        switch (kind) {
            case InitRuntimeFn::Begin: return "_lcompilers_init_begin";
            case InitRuntimeFn::End: return "_lcompilers_init_end";
            case InitRuntimeFn::RequireCollective:
                return "_lcompilers_init_require_collective";
        }
        return nullptr;
    }

    bool has_saved_coarray(SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                if (v->n_codims > 0 && !ASRUtils::is_allocatable(v->m_type)) {
                    return true;
                }
            } else if (ASR::is_a<ASR::Function_t>(*sym) || ASR::is_a<ASR::Block_t>(*sym)
                    || ASR::is_a<ASR::AssociateBlock_t>(*sym)) {
                if (has_saved_coarray(ASRUtils::symbol_symtab(sym))) return true;
            }
        }
        return false;
    }

} // anonymous namespace

ASR::Function_t* get_global_init(ASR::asr_t *owner) {
    char **link = owner_links(owner).first;
    if (link == nullptr || *link == nullptr) return nullptr;
    ASR::symbol_t *sym = owner_symtab(owner)->get_symbol(*link);
    if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)) return nullptr;
    return ASR::down_cast<ASR::Function_t>(sym);
}

ASR::Variable_t* get_global_init_state(ASR::asr_t *owner) {
    char **link = owner_links(owner).second;
    if (link == nullptr || *link == nullptr) return nullptr;
    ASR::symbol_t *sym = owner_symtab(owner)->get_symbol(*link);
    if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    return ASR::down_cast<ASR::Variable_t>(sym);
}

ASR::asr_t* global_init_owner(const ASR::Function_t *fn) {
    SymbolTable *scope = fn->m_symtab->parent;
    if (scope == nullptr || scope->asr_owner == nullptr) return nullptr;
    ASR::asr_t *owner = scope->asr_owner;
    if (owner_links(owner).first == nullptr) return nullptr;
    return get_global_init(owner) == fn ? owner : nullptr;
}

SymbolTable* global_init_owner_scope(const ASR::Function_t *fn) {
    ASR::asr_t *owner = global_init_owner(fn);
    return owner == nullptr ? nullptr : owner_symtab(owner);
}

bool is_owner_global_init(const ASR::Function_t *fn) {
    return global_init_owner(fn) != nullptr;
}

bool global_init_defined_here(const ASR::Function_t *fn) {
    ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
    return type->m_abi == ASR::abiType::Source
        && type->m_deftype == ASR::deftypeType::Implementation;
}

bool global_init_is_collective(ASR::asr_t *owner) {
    if (is_translation_unit(owner)) {
        return ASR::down_cast2<ASR::TranslationUnit_t>(owner)->m_global_init_collective;
    }
    ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
    if (ASR::is_a<ASR::Module_t>(*sym)) {
        return ASR::down_cast<ASR::Module_t>(sym)->m_global_init_collective;
    }
    return false;
}

std::string global_init_stable_id(ASR::asr_t *owner) {
    if (is_translation_unit(owner)) {
        ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(owner);
        LCOMPILERS_ASSERT(tu->m_global_init != nullptr);
        return std::string("t:") + tu->m_global_init;
    }
    ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
    if (ASR::is_a<ASR::Module_t>(*sym)) {
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
        if (m->m_parent_module != nullptr) {
            return std::string("s:") + m->m_parent_module + ":" + m->m_name;
        }
        return std::string("m:") + m->m_name;
    }
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Program_t>(*sym));
    return std::string("p:") + ASR::down_cast<ASR::Program_t>(sym)->m_name;
}

ASR::Function_t* get_global_init_bootstrap(ASR::TranslationUnit_t &unit) {
    if (unit.m_global_init_bootstrap == nullptr) return nullptr;
    ASR::symbol_t *sym = unit.m_symtab->get_symbol(unit.m_global_init_bootstrap);
    return sym != nullptr && ASR::is_a<ASR::Function_t>(*sym)
        ? ASR::down_cast<ASR::Function_t>(sym) : nullptr;
}

bool is_global_init_bootstrap(const ASR::Function_t *fn) {
    SymbolTable *parent = fn->m_symtab->parent;
    if (parent == nullptr || parent->asr_owner == nullptr
            || parent->asr_owner->type != ASR::asrType::unit) {
        return false;
    }
    ASR::TranslationUnit_t *unit = ASR::down_cast2<ASR::TranslationUnit_t>(
        parent->asr_owner);
    return get_global_init_bootstrap(*unit) == fn;
}

std::vector<GlobalInitRoot> global_init_roots(ASR::TranslationUnit_t &unit) {
    std::vector<GlobalInitRoot> roots;
    auto add = [&](ASR::asr_t *owner) {
        ASR::Function_t *fn = get_global_init(owner);
        if (fn == nullptr || !global_init_defined_here(fn)) return;
        ASR::Variable_t *state = get_global_init_state(owner);
        LCOMPILERS_ASSERT(state != nullptr);
        roots.push_back({global_init_stable_id(owner), fn, owner, state,
            global_init_is_collective(owner)});
    };
    for (auto &item : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Module_t>(*item.second)) {
            add((ASR::asr_t*)item.second);
        }
    }
    add((ASR::asr_t*)&unit);
    if (ASR::Function_t *bootstrap = get_global_init_bootstrap(unit)) {
        roots.push_back({"b:" + std::string(bootstrap->m_name), bootstrap,
            (ASR::asr_t*)&unit, nullptr, false, true});
    }
    std::sort(roots.begin(), roots.end(),
        [](const GlobalInitRoot &a, const GlobalInitRoot &b) {
            return a.stable_id < b.stable_id;
        });
    return roots;
}

ASR::stmt_t* make_global_init_call(Allocator &al, SymbolTable *scope,
        ASR::Function_t *ensure, const Location &loc) {
    ASR::symbol_t *sym = &ensure->base;
    SymbolTable *decl_scope = ensure->m_symtab->parent;
    bool visible = false;
    for (SymbolTable *s = scope; s != nullptr; s = s->parent) {
        if (s == decl_scope) { visible = true; break; }
    }
    if (!visible) {
        ASR::asr_t *owner = decl_scope->asr_owner;
        LCOMPILERS_ASSERT(owner != nullptr && owner->type == ASR::asrType::symbol
            && ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(owner)));
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(
            ASR::down_cast<ASR::symbol_t>(owner));
        sym = nullptr;
        for (auto &item : scope->get_scope()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)
                    && ASR::down_cast<ASR::ExternalSymbol_t>(item.second)->m_external
                        == &ensure->base) {
                sym = item.second;
                break;
            }
        }
        if (sym == nullptr) {
            // The initializer's name already says whose it is.
            std::string local = scope->get_unique_name(ensure->m_name, false);
            sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                al, loc, scope, s2c(al, local), &ensure->base, m->m_name,
                nullptr, 0, ensure->m_name, ASR::accessType::Private));
            scope->add_symbol(local, sym);
        }
    }
    return ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc, sym,
        sym, nullptr, 0, nullptr, nullptr, false));
}

bool global_init_borrows_storage(const ASR::Module_t &m) {
    return m.m_global_init == nullptr && !m.m_intrinsic;
}

std::vector<SymbolTable*> global_init_storage_scopes(ASR::asr_t *owner) {
    if (owner->type != ASR::asrType::unit) {
        return {ASRUtils::symbol_symtab(ASR::down_cast<ASR::symbol_t>(owner))};
    }
    SymbolTable *global = ASR::down_cast2<ASR::TranslationUnit_t>(owner)->m_symtab;
    std::vector<SymbolTable*> scopes = {global};
    for (auto &item : global->get_scope()) {
        if (ASR::is_a<ASR::Module_t>(*item.second)
                && global_init_borrows_storage(
                    *ASR::down_cast<ASR::Module_t>(item.second))) {
            scopes.push_back(ASR::down_cast<ASR::Module_t>(item.second)->m_symtab);
        }
    }
    return scopes;
}

bool needs_runtime_storage_setup(const ASR::Variable_t &v) {
    if (v.m_storage == ASR::storage_typeType::Parameter) return false;
    // Storage with a declaration initializer is laid out, members and all,
    // by that initializer.
    if (v.m_symbolic_value != nullptr || v.m_value != nullptr) return false;
    if (ASRUtils::is_allocatable_or_pointer(v.m_type)) return false;
    if (ASRUtils::is_array(v.m_type) && ASRUtils::extract_physical_type(v.m_type)
            != ASR::array_physical_typeType::FixedSizeArray) {
        return false;
    }
    ASR::ttype_t *element = ASRUtils::type_get_past_array(v.m_type);
    if (!ASR::is_a<ASR::StructType_t>(*element) || ASRUtils::is_class_type(element)
            || v.m_type_declaration == nullptr) {
        return false;
    }
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(v.m_type_declaration);
    if (!ASR::is_a<ASR::Struct_t>(*s)) return false;
    std::set<ASR::Struct_t*> visited;
    return ASRUtils::struct_needs_member_init(ASR::down_cast<ASR::Struct_t>(s),
        visited);
}

InitObjectFormat init_object_format(Platform platform) {
    switch (platform) {
        case Platform::macOS_Intel:
        case Platform::macOS_ARM:
        case Platform::macOS_PowerPC:
            return InitObjectFormat::MachO;
        case Platform::Windows:
            return InitObjectFormat::COFF;
        case Platform::Linux:
        case Platform::FreeBSD:
        case Platform::OpenBSD:
            return InitObjectFormat::ELF;
    }
    return InitObjectFormat::ELF;
}

ASR::symbol_t* get_init_runtime_function(Allocator &al,
        ASR::TranslationUnit_t &unit, InitRuntimeFn kind) {
    const char *c_name = init_runtime_function_name(kind);
    SymbolTable *global = unit.m_symtab;
    if (ASR::symbol_t *existing = global->get_symbol(c_name)) {
        LCOMPILERS_ASSERT(is_init_runtime_function(existing));
        return existing;
    }
    Location loc = unit.base.base.loc;
    SymbolTable *fn_symtab = al.make_new<SymbolTable>(global);
    Vec<ASR::expr_t*> args; args.reserve(al, 1);
    ASR::expr_t *return_var = nullptr;
    if (kind != InitRuntimeFn::RequireCollective) {
        ASR::symbol_t *state = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_symtab, s2c(al, "state"),
                nullptr, 0, ASR::intentType::InOut, nullptr, nullptr,
                ASR::storage_typeType::Default, state_type(al, loc), nullptr,
                ASR::abiType::BindC, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        fn_symtab->add_symbol("state", state);
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, state)));
    }
    if (kind == InitRuntimeFn::Begin) {
        ASR::symbol_t *result = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_symtab, s2c(al, "run"),
                nullptr, 0, ASR::intentType::ReturnVar, nullptr, nullptr,
                ASR::storage_typeType::Default, state_type(al, loc), nullptr,
                ASR::abiType::BindC, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        fn_symtab->add_symbol("run", result);
        return_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, result));
    }
    ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc, fn_symtab,
        s2c(al, c_name), nullptr, 0, args.p, args.n, nullptr, 0, return_var,
        ASR::abiType::BindC, ASR::accessType::Public,
        ASR::deftypeType::Interface, s2c(al, c_name),
        false, false, false, false, false, nullptr, 0,
        false, false, false, nullptr);
    ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(fn);
    global->add_symbol(c_name, sym);
    return sym;
}

bool is_init_runtime_function(const ASR::symbol_t *sym) {
    sym = ASRUtils::symbol_get_past_external(const_cast<ASR::symbol_t*>(sym));
    if (!ASR::is_a<ASR::Function_t>(*sym)) return false;
    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
    ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
    if (type->m_abi != ASR::abiType::BindC
            || type->m_deftype != ASR::deftypeType::Interface
            || type->m_bindc_name == nullptr
            || fn->m_symtab->parent == nullptr
            || fn->m_symtab->parent->parent != nullptr) {
        return false;
    }
    for (InitRuntimeFn kind : {InitRuntimeFn::Begin, InitRuntimeFn::End,
            InitRuntimeFn::RequireCollective}) {
        if (std::string(type->m_bindc_name) == init_runtime_function_name(kind)) {
            return true;
        }
    }
    return false;
}

ASR::Function_t* create_module_global_init(Allocator &al, ASR::Module_t *m) {
    LCOMPILERS_ASSERT(m->m_global_init == nullptr && m->m_global_init_state == nullptr);
    const Location &loc = m->base.base.loc;
    std::string part = owner_name_part((ASR::asr_t*)&m->base);
    m->m_global_init_state = create_state(al, m->m_symtab, loc,
        global_init_state_name + part, ASR::abiType::Source);
    m->m_global_init = create_initializer(al, m->m_symtab, loc,
        global_init_name + part);
    return get_global_init((ASR::asr_t*)&m->base);
}

ASR::Function_t* get_or_create_global_init(Allocator &al,
        ASR::TranslationUnit_t &unit, ASR::asr_t *owner) {
    return get_or_create_global_init(al, unit, owner, "");
}

ASR::Function_t* get_or_create_global_init(Allocator &al,
        ASR::TranslationUnit_t &/*unit*/, ASR::asr_t *owner,
        const std::string &name) {
    if (ASR::Function_t *fn = get_global_init(owner)) return fn;
    std::pair<char**, char**> links = owner_links(owner);
    if (links.first == nullptr) {
        throw LCompilersException("Only a module, a program or the "
            "translation unit can own a startup initializer");
    }
    SymbolTable *scope = owner_symtab(owner);
    Location loc = owner->loc;
    if (*links.second == nullptr) {
        std::string state = name.empty()
            ? global_init_state_name + owner_name_part(owner) : name + "_state";
        *links.second = create_state(al, scope, loc, state, ASR::abiType::Source);
    }
    *links.first = create_initializer(al, scope, loc,
        name.empty() ? global_init_name + owner_name_part(owner) : name);
    return get_global_init(owner);
}

void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
        ASR::stmt_t *stmt) {
    Vec<ASR::stmt_t*> body;
    body.from_pointer_n_copy(al, fn->m_body, fn->n_body);
    body.push_back(al, stmt);
    fn->m_body = body.p;
    fn->n_body = body.size();
}

void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
        const std::vector<ASR::stmt_t*> &stmts) {
    if (stmts.empty()) return;
    Vec<ASR::stmt_t*> body;
    body.reserve(al, fn->n_body + stmts.size());
    for (ASR::stmt_t *s : stmts) body.push_back(al, s);
    for (size_t i = 0; i < fn->n_body; i++) body.push_back(al, fn->m_body[i]);
    fn->m_body = body.p;
    fn->n_body = body.size();
}

void update_global_init_collective(ASR::TranslationUnit_t &unit, bool coarrays) {
    std::map<ASR::Module_t*, int> memo; // 0 visiting, 1 no, 2 yes
    std::function<bool(ASR::Module_t*)> collective = [&](ASR::Module_t *m) -> bool {
        auto it = memo.find(m);
        if (it != memo.end()) return it->second == 2;
        if (m->m_loaded_from_mod || m->m_intrinsic) {
            // Its own semantics computed it before its `.mod` file was
            // written.
            memo[m] = m->m_global_init_collective ? 2 : 1;
            return m->m_global_init_collective;
        }
        memo[m] = 0;
        bool result = coarrays && m->m_global_init != nullptr
            && has_saved_coarray(m->m_symtab);
        auto depend = [&](const char *name) {
            if (name == nullptr) return;
            ASR::symbol_t *sym = unit.m_symtab->resolve_symbol(name);
            if (sym != nullptr && ASR::is_a<ASR::Module_t>(*sym)
                    && ASR::down_cast<ASR::Module_t>(sym) != m
                    && collective(ASR::down_cast<ASR::Module_t>(sym))) {
                result = true;
            }
        };
        depend(m->m_parent_module);
        for (size_t i = 0; i < m->n_dependencies; i++) depend(m->m_dependencies[i]);
        result = result && m->m_global_init != nullptr;
        m->m_global_init_collective = result;
        memo[m] = result ? 2 : 1;
        return result;
    };
    for (auto &item : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Module_t>(*item.second)) {
            collective(ASR::down_cast<ASR::Module_t>(item.second));
        }
    }
}

namespace {

    bool is_init_runtime_call(ASR::symbol_t *sym, InitRuntimeFn kind) {
        if (!is_init_runtime_function(sym)) return false;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(sym));
        return std::string(ASRUtils::get_FunctionType(fn)->m_bindc_name)
            == init_runtime_function_name(kind);
    }

    // The state an initializer's guard tests, when `test` is that guard:
    // `_lcompilers_init_begin(state) /= 0`.
    ASR::expr_t* guarded_state(ASR::expr_t *test) {
        if (!ASR::is_a<ASR::IntegerCompare_t>(*test)) return nullptr;
        ASR::IntegerCompare_t *cmp = ASR::down_cast<ASR::IntegerCompare_t>(test);
        if (cmp->m_op != ASR::cmpopType::NotEq
                || !ASR::is_a<ASR::FunctionCall_t>(*cmp->m_left)) {
            return nullptr;
        }
        ASR::FunctionCall_t *call = ASR::down_cast<ASR::FunctionCall_t>(cmp->m_left);
        if (!is_init_runtime_call(call->m_name, InitRuntimeFn::Begin)
                || call->n_args != 1) {
            return nullptr;
        }
        return call->m_args[0].m_value;
    }

    class ClosedWorldExpander {
        private:
            Allocator &al;
            ASR::TranslationUnit_t &unit;
            std::vector<GlobalInitRoot> roots;

            ASR::stmt_t* assign_state(ASR::expr_t *state, int64_t value,
                    const Location &loc) {
                return ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc,
                    state, int32_constant(al, loc, value), nullptr, false, false));
            }

            ASR::expr_t* compare_state(ASR::expr_t *state, ASR::cmpopType op,
                    int64_t value, const Location &loc) {
                return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc, state,
                    op, int32_constant(al, loc, value),
                    ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
            }

            // The guard, as plain code on its state: a cycle stops the
            // program, and the state is ready only once the body has run.
            ASR::stmt_t* expand_guard(ASR::If_t *guard, ASR::expr_t *state) {
                const Location &loc = guard->base.base.loc;
                ASRUtils::ASRBuilder b(al, loc);
                std::string message = "initialization cycle detected";
                ASR::ttype_t *message_type = ASRUtils::TYPE(ASR::make_String_t(al,
                    loc, 1, int32_constant(al, loc, message.size()),
                    ASR::string_length_kindType::ExpressionLength,
                    ASR::string_physical_typeType::DescriptorString));
                ASR::stmt_t *cycle = ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc,
                    b.StringConstant(message, message_type)));
                std::vector<ASR::stmt_t*> body;
                body.push_back(b.If(compare_state(state, ASR::cmpopType::Eq, 1, loc),
                    {cycle}, {}));
                body.push_back(assign_state(state, 1, loc));
                for (size_t i = 0; i < guard->n_body; i++) {
                    ASR::stmt_t *s = guard->m_body[i];
                    if (ASR::is_a<ASR::SubroutineCall_t>(*s)) {
                        ASR::SubroutineCall_t *call = ASR::down_cast<ASR::SubroutineCall_t>(s);
                        if (is_init_runtime_call(call->m_name, InitRuntimeFn::End)) {
                            body.push_back(assign_state(state, 2, loc));
                            continue;
                        }
                        if (is_init_runtime_call(call->m_name,
                                InitRuntimeFn::RequireCollective)) {
                            continue;
                        }
                    }
                    body.push_back(s);
                }
                return b.If(compare_state(state, ASR::cmpopType::NotEq, 2, loc),
                    body, {});
            }

            void expand_body(SymbolTable *scope, ASR::stmt_t **&m_body, size_t &n_body) {
                Vec<ASR::stmt_t*> body;
                body.reserve(al, n_body);
                bool changed = false;
                for (size_t i = 0; i < n_body; i++) {
                    ASR::stmt_t *s = m_body[i];
                    if (ASR::is_a<ASR::GlobalInitDispatch_t>(*s)) {
                        ASR::GlobalInitDispatch_t *d = ASR::down_cast<ASR::GlobalInitDispatch_t>(s);
                        // The local roots, and at the collective boundary
                        // the bootstraps and then the collective roots.
                        for (int pass = 0; pass < 3; pass++) {
                            if (pass > 0 && d->m_phase != ASR::init_dispatch_phaseType::InitDispatchCollective) break;
                            for (GlobalInitRoot &root : roots) {
                                int root_pass = root.bootstrap ? 1 : (root.collective ? 2 : 0);
                                if (root_pass != pass) continue;
                                body.push_back(al, make_global_init_call(al, scope,
                                    root.ensure, s->base.loc));
                            }
                        }
                        for (size_t j = 0; j < d->n_ensures; j++) {
                            body.push_back(al, d->m_ensures[j]);
                        }
                        changed = true;
                        continue;
                    }
                    if (ASR::is_a<ASR::If_t>(*s)) {
                        ASR::If_t *guard = ASR::down_cast<ASR::If_t>(s);
                        if (ASR::expr_t *state = guarded_state(guard->m_test)) {
                            body.push_back(al, expand_guard(guard, state));
                            changed = true;
                            continue;
                        }
                    }
                    body.push_back(al, s);
                }
                if (changed) {
                    m_body = body.p;
                    n_body = body.size();
                }
            }

            void expand_scope(SymbolTable *scope) {
                for (auto &item : scope->get_scope()) {
                    ASR::symbol_t *sym = item.second;
                    if (ASR::is_a<ASR::Function_t>(*sym)) {
                        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
                        expand_body(f->m_symtab, f->m_body, f->n_body);
                        expand_scope(f->m_symtab);
                    } else if (ASR::is_a<ASR::Program_t>(*sym)) {
                        ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
                        expand_body(p->m_symtab, p->m_body, p->n_body);
                        expand_scope(p->m_symtab);
                    } else if (ASR::is_a<ASR::Module_t>(*sym)) {
                        expand_scope(ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
                    } else if (ASR::is_a<ASR::Block_t>(*sym)) {
                        expand_scope(ASR::down_cast<ASR::Block_t>(sym)->m_symtab);
                    }
                }
            }

            // The roots, and the initializers of the modules this
            // translation unit only declares: output compiled file by file
            // runs those through the initializer the other file defines.
            static std::vector<GlobalInitRoot> closed_world_roots(
                    ASR::TranslationUnit_t &unit) {
                std::vector<GlobalInitRoot> all = global_init_roots(unit);
                for (auto &item : unit.m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Module_t>(*item.second)) continue;
                    ASR::asr_t *owner = (ASR::asr_t*)item.second;
                    ASR::Function_t *fn = get_global_init(owner);
                    if (fn == nullptr || global_init_defined_here(fn)) continue;
                    all.push_back({global_init_stable_id(owner), fn, owner,
                        get_global_init_state(owner), global_init_is_collective(owner)});
                }
                std::sort(all.begin(), all.end(),
                    [](const GlobalInitRoot &a, const GlobalInitRoot &b) {
                        return a.stable_id < b.stable_id;
                    });
                return all;
            }

        public:
            ClosedWorldExpander(Allocator &al_, ASR::TranslationUnit_t &unit_):
                al(al_), unit(unit_), roots(closed_world_roots(unit_)) {}

            void expand() {
                expand_scope(unit.m_symtab);
                for (InitRuntimeFn kind : {InitRuntimeFn::Begin, InitRuntimeFn::End,
                        InitRuntimeFn::RequireCollective}) {
                    const char *name = init_runtime_function_name(kind);
                    ASR::symbol_t *sym = unit.m_symtab->get_symbol(name);
                    if (sym != nullptr && is_init_runtime_function(sym)) {
                        unit.m_symtab->erase_symbol(name);
                    }
                }
            }
    };

} // anonymous namespace

void expand_closed_world_dispatch(Allocator &al, ASR::TranslationUnit_t &unit) {
    ClosedWorldExpander e(al, unit);
    e.expand();
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace ASRUtils

namespace {

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
        // What the defaults put into an initializer imported into its scope,
        // by definition; see `DefaultValueDuplicator`.
        std::map<SymbolTable*, std::map<ASR::symbol_t*, ASR::symbol_t*>> default_imports;

    public:

        const PassOptions &pass_options;

        GlobalInitVisitor(Allocator &al_, ASR::TranslationUnit_t &unit_,
                const PassOptions &pass_options_):
            al(al_), unit(unit_), pass_options(pass_options_) {}

        // Valid source this lowering cannot give the startup order
        // Fortran requires: an error at `loc`, and the pass stops there.
        void report_unsupported(const std::string &message, const Location &loc) {
            if (pass_options.diagnostics == nullptr) {
                throw LCompilersException(message);
            }
            pass_options.diagnostics->message_label(message, {loc},
                "not supported yet", diag::Level::Error, diag::Stage::ASRPass);
        }

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
                    || v.n_codims > 0 || !(ASRUtils::is_module_variable(v)
                        || ASRUtils::is_tu_scope(v.m_parent_symtab))) {
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

        // A foreign program can read a variable with a binding label
        // directly, before anything of the engine has run, so its initial
        // value has to be static data.
        bool require_static_exported_data(const ASR::Variable_t &v) {
            if (v.m_abi == ASR::abiType::BindC) {
                report_unsupported("the initial value of the bind(c) "
                    "variable '" + std::string(v.m_name) + "' cannot be laid "
                    "out as static data, which a variable with a binding "
                    "label requires", v.base.base.loc);
                return false;
            }
            return true;
        }

        // Move every declaration initializer of `owner`'s scope that needs
        // executable code into `owner`'s initializer, and, for a module, put
        // there the default initialization of its variables that static data
        // does not hold, all in declaration order.
        void lower_scope(ASR::asr_t *owner, SymbolTable *scope) {
            // Lowered and guarded by an earlier run of the pipeline.
            if (ASR::Function_t *existing = ASRUtils::get_global_init(owner)) {
                if (existing->n_body == 1 && ASR::is_a<ASR::If_t>(*existing->m_body[0])
                        && ASRUtils::guarded_state(ASR::down_cast<ASR::If_t>(
                            existing->m_body[0])->m_test) != nullptr) {
                    return;
                }
            }
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
                    if (!require_static_exported_data(*v)) continue;
                    ASRUtils::global_init_append_stmt(al, initializer(),
                        take_initializer(v));
                    continue;
                }
                ASR::Struct_t *s = default_initialized_type(*v);
                if (s == nullptr || !variable_default_init_stmts(v, s, nullptr, nullptr)) {
                    continue;
                }
                if (!require_static_exported_data(*v)) continue;
                std::vector<ASR::stmt_t*> stmts;
                variable_default_init_stmts(v, s, initializer()->m_symtab, &stmts);
                for (ASR::stmt_t *stmt : stmts) {
                    ASRUtils::global_init_append_stmt(al, fn, stmt);
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
                    ASRUtils::local_init_guard_name, false));
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


        // A module this translation unit only declares -- one another object
        // file defines -- is left alone: that object file lowers it.
        bool defines_module(ASR::Module_t *m) {
            if (m->m_intrinsic) return false;
            ASR::Function_t *fn = ASRUtils::get_global_init((ASR::asr_t*)&m->base);
            // Every user module owns one from semantics on, and a module file
            // written without one is rejected by its version. A module owning
            // none is one semantics or a pass made for storage of its own,
            // COMMON above all, which is defined wherever it is used.
            if (fn == nullptr) return !m->m_loaded_from_mod;
            return ASRUtils::global_init_defined_here(fn);
        }

        // The modules whose initializers a foreign entry into `fn` has to
        // run: the module that contains it and every module a symbol of its
        // scope, or of a scope it contains, is imported from.
        void collect_module_imports(SymbolTable *scope, std::set<ASR::Module_t*> &out) {
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
                    ASR::ExternalSymbol_t *e = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
                    ASR::symbol_t *m = unit.m_symtab->resolve_symbol(e->m_module_name);
                    if (m != nullptr && ASR::is_a<ASR::Module_t>(*m)) {
                        out.insert(ASR::down_cast<ASR::Module_t>(m));
                    }
                } else if (ASR::is_a<ASR::Block_t>(*sym)
                        || ASR::is_a<ASR::AssociateBlock_t>(*sym)
                        || ASR::is_a<ASR::Function_t>(*sym)) {
                    collect_module_imports(ASRUtils::symbol_symtab(sym), out);
                }
            }
        }

        ASR::Module_t* enclosing_module(SymbolTable *scope) {
            for (SymbolTable *s = scope; s != nullptr; s = s->parent) {
                if (s->asr_owner != nullptr && s->asr_owner->type == ASR::asrType::symbol
                        && ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(s->asr_owner))) {
                    return ASR::down_cast<ASR::Module_t>(ASR::down_cast<ASR::symbol_t>(s->asr_owner));
                }
            }
            return nullptr;
        }

        // A procedure with a binding label can be entered by a foreign
        // caller before any of the startup this program's own entry points
        // run has happened, a C constructor above all. It enters the engine
        // first, and then runs, explicitly, the initializers of the modules
        // it needs: those are ready when it returns, or it has reported a
        // genuine cycle.
        // The initializers a foreign entry into a procedure whose own scope
        // is `scope` has to run, as calls written in `target`: those of the
        // module that contains it and of every module a symbol of its scope,
        // or of a scope it contains, is imported from.
        Vec<ASR::stmt_t*> entry_ensures(SymbolTable *scope, SymbolTable *target,
                const Location &loc) {
            std::set<ASR::Module_t*> modules;
            if (ASR::Module_t *m = enclosing_module(scope->parent)) {
                modules.insert(m);
            }
            collect_module_imports(scope, modules);
            // What a host procedure or program imports, the procedure reaches
            // by host association.
            for (SymbolTable *s = scope->parent; s != nullptr && s->asr_owner != nullptr
                    && s->asr_owner->type == ASR::asrType::symbol
                    && !ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(
                        s->asr_owner)); s = s->parent) {
                for (auto &item : s->get_scope()) {
                    if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
                    ASR::symbol_t *m = unit.m_symtab->resolve_symbol(
                        ASR::down_cast<ASR::ExternalSymbol_t>(item.second)->m_module_name);
                    if (m != nullptr && ASR::is_a<ASR::Module_t>(*m)) {
                        modules.insert(ASR::down_cast<ASR::Module_t>(m));
                    }
                }
            }
            std::vector<std::pair<std::string, ASR::Function_t*>> ensures;
            for (ASR::Module_t *m : modules) {
                ASR::Function_t *init = ASRUtils::get_global_init((ASR::asr_t*)&m->base);
                if (init == nullptr) continue;
                ensures.push_back({ASRUtils::global_init_stable_id(
                    (ASR::asr_t*)&m->base), init});
            }
            std::sort(ensures.begin(), ensures.end(),
                [](const std::pair<std::string, ASR::Function_t*> &a,
                   const std::pair<std::string, ASR::Function_t*> &b) {
                    return a.first < b.first;
                });
            Vec<ASR::stmt_t*> calls; calls.reserve(al, ensures.size());
            for (auto &e : ensures) {
                calls.push_back(al, ASRUtils::make_global_init_call(al,
                    target, e.second, loc));
            }
            return calls;
        }

        ASR::stmt_t* entry_dispatch(Vec<ASR::stmt_t*> &calls, const Location &loc) {
            return ASRUtils::STMT(ASR::make_GlobalInitDispatch_t(al, loc,
                ASR::init_dispatch_phaseType::InitDispatchLocal, calls.p, calls.n));
        }

        // A procedure with a binding label can be entered by a foreign
        // caller before any of the startup this program's own entry points
        // run has happened, a C constructor above all. It enters the engine
        // first, and then runs, explicitly, the initializers of the modules
        // it needs: those are ready when it returns, or it has reported a
        // genuine cycle. This is for a procedure `make_entry_wrapper` does
        // not apply to; see there.
        void add_entry_dispatch(ASR::Function_t *fn) {
            const Location &loc = fn->base.base.loc;
            Vec<ASR::stmt_t*> calls = entry_ensures(fn->m_symtab, fn->m_symtab, loc);
            Vec<ASR::stmt_t*> body; body.reserve(al, fn->n_body + 1);
            body.push_back(al, entry_dispatch(calls, loc));
            for (size_t i = 0; i < fn->n_body; i++) body.push_back(al, fn->m_body[i]);
            fn->m_body = body.p;
            fn->n_body = body.size();
        }

        // Copies the declarations of a procedure's dummy arguments into the
        // scope of its entry wrapper: a dummy becomes the wrapper's own, a
        // symbol the procedure imported is imported again, and a named
        // constant of the procedure becomes its value. Whether a declaration
        // reads anything else -- a variable of a module, above all, whose
        // value startup initialization gives -- is recorded, because
        // evaluating that before the dispatch is what the wrapper avoids.
        class EntryDeclarationDuplicator:
                public ASR::BaseExprStmtDuplicator<EntryDeclarationDuplicator> {
            public:
                SymbolTable *procedure_scope;
                SymbolTable *entry_scope;
                std::map<ASR::symbol_t*, ASR::symbol_t*> &copies;
                bool reads_state = false;

                EntryDeclarationDuplicator(Allocator &al, SymbolTable *procedure_scope_,
                        SymbolTable *entry_scope_,
                        std::map<ASR::symbol_t*, ASR::symbol_t*> &copies_):
                    ASR::BaseExprStmtDuplicator<EntryDeclarationDuplicator>(al),
                    procedure_scope(procedure_scope_), entry_scope(entry_scope_),
                    copies(copies_) {}

                static bool is_state(ASR::symbol_t *sym) {
                    sym = ASRUtils::symbol_get_past_external(sym);
                    return ASR::is_a<ASR::Variable_t>(*sym)
                        && ASR::down_cast<ASR::Variable_t>(sym)->m_storage
                            != ASR::storage_typeType::Parameter;
                }

                ASR::symbol_t* resolve(ASR::symbol_t *sym) {
                    auto it = copies.find(sym);
                    if (it != copies.end()) return it->second;
                    if (ASRUtils::symbol_parent_symtab(sym) != procedure_scope) {
                        // Declared around the procedure, so equally visible
                        // from the wrapper, which is declared beside it.
                        if (is_state(sym)) reads_state = true;
                        return sym;
                    }
                    if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
                        if (is_state(sym)) reads_state = true;
                        ASR::ExternalSymbol_t *e = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
                        std::string name = entry_scope->get_symbol(e->m_name) == nullptr
                            ? std::string(e->m_name)
                            : entry_scope->get_unique_name(e->m_name, false);
                        ASR::symbol_t *copy = ASR::down_cast<ASR::symbol_t>(
                            ASR::make_ExternalSymbol_t(al, e->base.base.loc,
                                entry_scope, s2c(al, name), e->m_external,
                                e->m_module_name, e->m_scope_names, e->n_scope_names,
                                e->m_original_name, e->m_access));
                        entry_scope->add_symbol(name, copy);
                        copies[sym] = copy;
                        return copy;
                    }
                    reads_state = true;
                    return sym;
                }

                ASR::asr_t* duplicate_Var(ASR::Var_t *x) {
                    ASR::symbol_t *sym = x->m_v;
                    if (ASRUtils::symbol_parent_symtab(sym) == procedure_scope
                            && ASR::is_a<ASR::Variable_t>(*sym)) {
                        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                        if (v->m_storage == ASR::storage_typeType::Parameter
                                && v->m_value != nullptr
                                && copies.find(sym) == copies.end()) {
                            return (ASR::asr_t*)duplicate_expr(v->m_value);
                        }
                    }
                    return ASR::make_Var_t(al, x->base.base.loc, resolve(sym));
                }
        };

        // The type of `arg` as the wrapper declares it: the procedure's own,
        // unless that reads startup state, in which case the declaration that
        // evaluates nothing and is passed the same way -- an assumed-size
        // array for one of explicit shape, an assumed-shape array without
        // lower bounds for one of assumed shape.
        ASR::ttype_t* entry_argument_type(ASR::Variable_t *arg,
                EntryDeclarationDuplicator &dup) {
            dup.reads_state = false;
            ASR::ttype_t *type = dup.duplicate_ttype(arg->m_type);
            if (!dup.reads_state || !ASR::is_a<ASR::Array_t>(*arg->m_type)) {
                return type;
            }
            ASR::Array_t *array = ASR::down_cast<ASR::Array_t>(arg->m_type);
            const Location &loc = arg->base.base.loc;
            Vec<ASR::dimension_t> dims;
            if (array->m_physical_type == ASR::array_physical_typeType::PointerArray) {
                dims.reserve(al, 1);
                ASR::dimension_t d; d.loc = loc; d.m_start = nullptr; d.m_length = nullptr;
                dims.push_back(al, d);
            } else {
                dims.reserve(al, array->n_dims);
                for (size_t i = 0; i < array->n_dims; i++) {
                    ASR::dimension_t d; d.loc = loc; d.m_start = nullptr; d.m_length = nullptr;
                    dims.push_back(al, d);
                }
            }
            return ASRUtils::TYPE(ASR::make_Array_t(al, loc, array->m_type,
                dims.p, dims.n, array->m_physical_type, array->m_memory_space));
        }

        // Move what the declarations of `fn`'s dummy arguments and result
        // name from its own scope into the scope around it, which the entry
        // and the implementation share: derived types, named constants,
        // procedure interfaces and imports, and what those name in turn.
        // Each of them is a declaration that evaluates nothing, so it is
        // equally right in either scope, and the implementation reaches it
        // by host association. A name the scope around already has is
        // qualified by the procedure's. Anything else -- a variable -- cannot
        // move, which is reported.
        bool hoist_declaration_symbols(ASR::Function_t *fn, SymbolTable *from,
                SymbolTable *to) {
            struct Referenced : ASR::BaseWalkVisitor<Referenced> {
                SymbolTable *scope;
                std::vector<ASR::symbol_t*> found;
                void note(ASR::symbol_t *sym) {
                    if (sym != nullptr && ASRUtils::symbol_parent_symtab(sym) == scope) {
                        found.push_back(sym);
                    }
                }
                void visit_Var(const ASR::Var_t &x) { note(x.m_v); }
            };
            Referenced ref;
            ref.scope = from;
            std::vector<ASR::expr_t*> vars(fn->m_args, fn->m_args + fn->n_args);
            if (fn->m_return_var != nullptr) vars.push_back(fn->m_return_var);
            for (ASR::expr_t *e : vars) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                ref.note(v->m_type_declaration);
                ref.visit_ttype(*v->m_type);
            }
            std::set<ASR::symbol_t*> moved;
            while (!ref.found.empty()) {
                ASR::symbol_t *sym = ref.found.back();
                ref.found.pop_back();
                if (!moved.insert(sym).second) continue;
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                    bool dummy = false;
                    for (ASR::expr_t *e : vars) {
                        dummy = dummy || ASR::down_cast<ASR::Var_t>(e)->m_v == sym;
                    }
                    if (dummy) { moved.erase(sym); continue; }
                    if (v->m_storage != ASR::storage_typeType::Parameter) {
                        report_unsupported("a dummy argument of '"
                            + std::string(fn->m_name) + "', which has a binding "
                            "label, is declared with the variable '"
                            + std::string(v->m_name) + "' of the procedure itself",
                            v->base.base.loc);
                        return false;
                    }
                    ref.note(v->m_type_declaration);
                    ref.visit_ttype(*v->m_type);
                    if (v->m_symbolic_value) ref.visit_expr(*v->m_symbolic_value);
                } else if (ASR::is_a<ASR::Struct_t>(*sym)) {
                    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(sym);
                    ref.note(st->m_parent);
                    for (auto &item : st->m_symtab->get_scope()) {
                        if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                            ASR::Variable_t *m = ASR::down_cast<ASR::Variable_t>(item.second);
                            ref.note(m->m_type_declaration);
                            ref.visit_ttype(*m->m_type);
                            if (m->m_symbolic_value) ref.visit_expr(*m->m_symbolic_value);
                        } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*item.second)) {
                            ref.note(ASR::down_cast<ASR::StructMethodDeclaration_t>(
                                item.second)->m_proc);
                        }
                    }
                } else if (ASR::is_a<ASR::Function_t>(*sym)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
                    if (ASRUtils::get_FunctionType(f)->m_deftype
                            != ASR::deftypeType::Interface) {
                        report_unsupported("a dummy argument of '"
                            + std::string(fn->m_name) + "', which has a binding "
                            "label, is declared with the procedure '"
                            + std::string(f->m_name) + "' it contains",
                            f->base.base.loc);
                        return false;
                    }
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
                    // An import this procedure made for the declaration; the
                    // procedure keeps reaching it from the entry.
                } else if (!ASR::is_a<ASR::GenericProcedure_t>(*sym)) {
                    moved.erase(sym);
                }
            }
            for (ASR::symbol_t *sym : moved) {
                std::string name = ASRUtils::symbol_name(sym);
                from->erase_symbol(name);
                // A name the enclosing scope already has is qualified by the
                // procedure's, the same way in every compilation.
                std::string new_name = name;
                if (to->get_symbol(new_name) != nullptr) {
                    new_name = "__lcompilers_" + std::string(fn->m_name) + "_" + name;
                    if (to->get_symbol(new_name) != nullptr) {
                        throw LCompilersException("the name '" + new_name
                            + "' of a declaration hoisted out of a procedure "
                            "with a binding label is taken");
                    }
                    char *c = s2c(al, new_name);
                    switch (sym->type) {
                        case ASR::symbolType::Variable:
                            ASR::down_cast<ASR::Variable_t>(sym)->m_name = c; break;
                        case ASR::symbolType::Struct:
                            ASR::down_cast<ASR::Struct_t>(sym)->m_name = c; break;
                        case ASR::symbolType::Function:
                            ASR::down_cast<ASR::Function_t>(sym)->m_name = c; break;
                        case ASR::symbolType::GenericProcedure:
                            ASR::down_cast<ASR::GenericProcedure_t>(sym)->m_name = c; break;
                        case ASR::symbolType::ExternalSymbol:
                            ASR::down_cast<ASR::ExternalSymbol_t>(sym)->m_name = c; break;
                        default: break;
                    }
                }
                to->add_symbol(new_name, sym);
                switch (sym->type) {
                    case ASR::symbolType::Variable:
                        ASR::down_cast<ASR::Variable_t>(sym)->m_parent_symtab = to;
                        break;
                    case ASR::symbolType::Struct:
                        ASR::down_cast<ASR::Struct_t>(sym)->m_symtab->parent = to;
                        break;
                    case ASR::symbolType::Function:
                        ASR::down_cast<ASR::Function_t>(sym)->m_symtab->parent = to;
                        break;
                    case ASR::symbolType::GenericProcedure:
                        ASR::down_cast<ASR::GenericProcedure_t>(sym)->m_parent_symtab = to;
                        break;
                    case ASR::symbolType::ExternalSymbol:
                        ASR::down_cast<ASR::ExternalSymbol_t>(sym)->m_parent_symtab = to;
                        break;
                    default:
                        break;
                }
            }
            return true;
        }

        // The procedure `fn` with a binding label becomes the entry a foreign
        // caller enters by: it keeps its identity -- what `c_funloc`, a
        // generic or an import refers to -- and its binding label, and gets a
        // scope of its own holding only copies of its dummy arguments and its
        // result, declared so that none reads startup state. Its body enters
        // the engine and runs the initializers it needs, and then calls the
        // procedure as the user wrote it, which moves, scope, specification
        // part and body, into a new procedure with the same interface: the
        // entry's sibling, so that nothing gains a level of nesting. That
        // one is an ordinary procedure with a binding label too, so that it
        // is called with exactly the interoperable calling convention the
        // entry was: its label is its name, unique in a program because it is
        // made of every scope around the procedure. Every specification
        // expression of the procedure is thereby evaluated after the
        // dispatch, whatever the target.
        void make_entry_wrapper(ASR::Function_t *fn) {
            const Location &loc = fn->base.base.loc;
            SymbolTable *parent = fn->m_symtab->parent;
            SymbolTable *procedure_scope = fn->m_symtab;
            ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);

            // Named after every scope around it, so that no two procedures
            // of a program get the same one, and deterministically.
            std::string owner;
            for (SymbolTable *s = parent; s != nullptr && s->asr_owner != nullptr
                    && s->asr_owner->type == ASR::asrType::symbol; s = s->parent) {
                owner = std::string(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::symbol_t>(s->asr_owner))) + "_" + owner;
            }
            std::string impl_name = "__lcompilers_impl_" + owner + fn->m_name;

            // The procedure as written becomes a sibling of the entry, so
            // that no scope gains a level of nesting Fortran does not allow:
            // what it contains stays one level below it. The types, named
            // constants and interfaces its dummy arguments are declared with
            // move to the scope around both, which each reaches by host
            // association, so the entry declares its copies of the dummies
            // with the very same type.
            if (parent->get_symbol(impl_name) != nullptr) {
                throw LCompilersException("the name '" + impl_name + "' of the "
                    "implementation of a procedure with a binding label is taken");
            }
            if (!hoist_declaration_symbols(fn, procedure_scope, parent)) return;
            SymbolTable *entry_scope = al.make_new<SymbolTable>(parent);
            entry_scope->asr_owner = (ASR::asr_t*)&fn->base;

            // The procedure as written, under its new name.
            ASR::asr_t *impl_asr = ASRUtils::make_Function_t_util(al, loc,
                procedure_scope, s2c(al, impl_name), fn->m_dependencies,
                fn->n_dependencies, fn->m_args, fn->n_args, fn->m_body, fn->n_body,
                fn->m_return_var, ASR::abiType::BindC, ASR::accessType::Private,
                ASR::deftypeType::Implementation, s2c(al, impl_name), type->m_elemental,
                type->m_pure, type->m_module, type->m_inline, type->m_static,
                type->m_restrictions, type->n_restrictions, type->m_is_restriction,
                fn->m_deterministic, fn->m_side_effect_free, nullptr, nullptr,
                nullptr, type->m_exec_space);
            ASR::Function_t *impl = ASR::down_cast2<ASR::Function_t>(impl_asr);
            procedure_scope->asr_owner = impl_asr;
            parent->add_symbol(impl_name, &impl->base);

            // The entry: copies of the dummies and the result.
            std::map<ASR::symbol_t*, ASR::symbol_t*> copies;
            std::vector<ASR::Variable_t*> originals;
            for (size_t i = 0; i < fn->n_args; i++) {
                originals.push_back(ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v));
            }
            if (fn->m_return_var != nullptr) {
                originals.push_back(ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v));
            }
            std::vector<ASR::Variable_t*> entry_vars;
            for (ASR::Variable_t *v : originals) {
                ASR::symbol_t *copy = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, v->base.base.loc, entry_scope,
                        v->m_name, nullptr, 0, v->m_intent, nullptr, nullptr,
                        v->m_storage, v->m_type, nullptr, v->m_abi, v->m_access,
                        v->m_presence, v->m_value_attr, v->m_target_attr,
                        v->m_contiguous_attr, v->m_bindc_name, v->m_is_volatile,
                        v->m_is_protected));
                entry_scope->add_symbol(v->m_name, copy);
                copies[&v->base] = copy;
                entry_vars.push_back(ASR::down_cast<ASR::Variable_t>(copy));
            }
            EntryDeclarationDuplicator dup(al, procedure_scope, entry_scope, copies);
            for (size_t i = 0; i < originals.size(); i++) {
                ASR::Variable_t *v = originals[i];
                ASR::Variable_t *e = entry_vars[i];
                e->m_type = entry_argument_type(v, dup);
                if (v->m_type_declaration != nullptr) {
                    e->m_type_declaration = dup.resolve(v->m_type_declaration);
                }
                SetChar deps; deps.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(al, deps, e->m_type,
                    nullptr, nullptr, e->m_name);
                e->m_dependencies = deps.p;
                e->n_dependencies = deps.size();
            }

            // Its body: the dispatch, then the procedure.
            Vec<ASR::expr_t*> args; args.reserve(al, fn->n_args);
            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, fn->n_args);
            for (size_t i = 0; i < fn->n_args; i++) {
                ASR::expr_t *arg = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    &entry_vars[i]->base));
                args.push_back(al, arg);
                ASR::call_arg_t call_arg; call_arg.loc = loc; call_arg.m_value = arg;
                call_args.push_back(al, call_arg);
            }
            ASR::expr_t *return_var = fn->m_return_var == nullptr ? nullptr
                : ASRUtils::EXPR(ASR::make_Var_t(al, loc, &entry_vars.back()->base));
            Vec<ASR::stmt_t*> ensures = entry_ensures(procedure_scope, entry_scope, loc);
            Vec<ASR::stmt_t*> body; body.reserve(al, 2);
            body.push_back(al, entry_dispatch(ensures, loc));
            if (return_var == nullptr) {
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al,
                    loc, &impl->base, &impl->base, call_args.p, call_args.n, nullptr,
                    nullptr, false)));
            } else {
                ASR::expr_t *call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al,
                    loc, &impl->base, &impl->base, call_args.p, call_args.n,
                    ASRUtils::expr_type(return_var), nullptr, nullptr));
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
                    loc, return_var, call, nullptr, false, false)));
            }

            fn->m_symtab = entry_scope;
            fn->m_args = args.p;
            fn->n_args = args.n;
            fn->m_return_var = return_var;
            fn->m_body = body.p;
            fn->n_body = body.size();
            fn->m_function_signature = ASRUtils::TYPE(ASRUtils::make_FunctionType_t_util(
                al, loc, args.p, args.n, return_var, type, entry_scope));
        }

        // Whether `fn` can have its entry wrapped: every procedure with a
        // binding label but one whose result is allocatable or a pointer,
        // which is not interoperable, so only a compiler-made procedure has
        // one, and a later pass returns it through an argument forwarding
        // would have to reproduce.
        bool wraps_entry(ASR::Function_t *fn) {
            if (fn->m_return_var != nullptr && ASRUtils::is_allocatable_or_pointer(
                    ASRUtils::expr_type(fn->m_return_var))) {
                return false;
            }
            for (size_t i = 0; i < fn->n_args; i++) {
                if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])
                        || !ASR::is_a<ASR::Variable_t>(*ASR::down_cast<ASR::Var_t>(
                            fn->m_args[i])->m_v)) {
                    return false;
                }
            }
            return true;
        }

        // Whether a declaration of `fn`'s scope reads what startup
        // initialization gives: a variable of a host, a module or the
        // translation unit, or anything a call can reach.
        static bool reads_startup_state(ASR::Function_t *fn) {
            struct Reader : ASR::BaseWalkVisitor<Reader> {
                SymbolTable *scope;
                bool found = false;
                void visit_Var(const ASR::Var_t &x) {
                    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
                    if (ASR::is_a<ASR::Variable_t>(*sym)) {
                        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                        if (v->m_parent_symtab != scope
                                && v->m_storage != ASR::storage_typeType::Parameter) {
                            found = true;
                        }
                    }
                }
                void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) {
                    found = true;
                }
            };
            Reader reader;
            reader.scope = fn->m_symtab;
            for (auto &item : fn->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                if (v->m_storage == ASR::storage_typeType::Parameter) continue;
                reader.visit_ttype(*v->m_type);
                if (reader.found) return true;
            }
            return false;
        }

        static ASR::GlobalInitDispatch_t* entry_dispatch_of(ASR::Function_t *f) {
            for (size_t i = 0; i < f->n_body; i++) {
                if (ASR::is_a<ASR::GlobalInitDispatch_t>(*f->m_body[i])) {
                    return ASR::down_cast<ASR::GlobalInitDispatch_t>(f->m_body[i]);
                }
            }
            return nullptr;
        }

        // The implementation an entry wrapper forwards to, or nullptr.
        static ASR::Function_t* wrapped_implementation(ASR::Function_t *f) {
            if (f->n_body != 2 || !ASR::is_a<ASR::GlobalInitDispatch_t>(*f->m_body[0])) {
                return nullptr;
            }
            ASR::symbol_t *callee = nullptr;
            if (ASR::is_a<ASR::SubroutineCall_t>(*f->m_body[1])) {
                callee = ASR::down_cast<ASR::SubroutineCall_t>(f->m_body[1])->m_name;
            } else if (ASR::is_a<ASR::Assignment_t>(*f->m_body[1])) {
                ASR::expr_t *value = ASR::down_cast<ASR::Assignment_t>(f->m_body[1])->m_value;
                if (ASR::is_a<ASR::FunctionCall_t>(*value)) {
                    callee = ASR::down_cast<ASR::FunctionCall_t>(value)->m_name;
                }
            }
            if (callee == nullptr || !ASR::is_a<ASR::Function_t>(*callee)
                    || ASRUtils::symbol_parent_symtab(callee) != f->m_symtab->parent) {
                return nullptr;
            }
            return ASR::down_cast<ASR::Function_t>(callee);
        }

        void add_entry_dispatches(SymbolTable *scope) {
            std::vector<ASR::Function_t*> functions;
            // What an earlier run of the pipeline already wrapped.
            std::set<ASR::Function_t*> done;
            for (auto &item : scope->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*item.second)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(item.second);
                    functions.push_back(f);
                    if (entry_dispatch_of(f) != nullptr) {
                        done.insert(f);
                        // The implementation an entry forwards to is its
                        // sibling, and not an entry itself.
                        if (ASR::Function_t *impl = wrapped_implementation(f)) done.insert(impl);
                    }
                }
            }
            for (ASR::Function_t *f : functions) {
                if (done.find(f) != done.end()) {
                    add_entry_dispatches(f->m_symtab);
                    continue;
                }
                SymbolTable *own_scope = f->m_symtab;
                ASR::FunctionType_t *type = ASRUtils::get_FunctionType(f);
                if (type->m_abi == ASR::abiType::BindC
                        && type->m_deftype == ASR::deftypeType::Implementation
                        && !ASRUtils::is_device_only_function(*f)) {
                    // The one contract: the entry enters the engine before
                    // anything of the procedure is evaluated. When none of
                    // its declarations reads startup state, entering it
                    // first in the body already meets it; only a procedure
                    // whose declarations do needs the wrapper.
                    bool reads = reads_startup_state(f);
                    if (!reads) {
                        add_entry_dispatch(f);
                    } else if (wraps_entry(f)) {
                        make_entry_wrapper(f);
                    } else {
                        report_unsupported("the procedure '"
                            + std::string(f->m_name) + "' has a binding label, "
                            "a declaration that reads startup state and a "
                            "result that is allocatable or a pointer: that "
                            "declaration cannot be ordered after the "
                            "initialization a foreign caller needs",
                            f->base.base.loc);
                    }
                }
                add_entry_dispatches(own_scope);
            }
        }

        void visit_TranslationUnit() {
            std::vector<std::string> module_order =
                ASRUtils::determine_module_dependencies(unit);
            std::vector<ASR::Module_t*> defined;
            for (auto &name : module_order) {
                ASR::symbol_t *sym = unit.m_symtab->get_symbol(name);
                if (sym == nullptr || !ASR::is_a<ASR::Module_t>(*sym)) continue;
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
                if (!defines_module(m)) continue;
                defined.push_back(m);
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
            // The variables of the translation unit itself, those of an
            // interactive cell.
            lower_scope((ASR::asr_t*)&unit, unit.m_symtab);

            // Procedures and blocks come last: the initializers created above
            // own nothing that needs initializing, so walking into them now
            // costs nothing and the walk sees a settled symbol table.
            lower_local_scopes(unit.m_symtab);
            for (ASR::Module_t *m : defined) lower_local_scopes(m->m_symtab);
            for (ASR::symbol_t *sym : programs) {
                lower_local_scopes(ASR::down_cast<ASR::Program_t>(sym)->m_symtab);
            }

            add_entry_dispatches(unit.m_symtab);
            for (ASR::Module_t *m : defined) add_entry_dispatches(m->m_symtab);
            // A program's internal procedure with a binding label is as
            // reachable from a foreign caller, through its C address.
            for (ASR::symbol_t *sym : programs) {
                add_entry_dispatches(ASR::down_cast<ASR::Program_t>(sym)->m_symtab);
            }
        }

};


// Every pass that can put a statement into an initializer has run: give
// each initializer this translation unit defines its guarded shape, and
// start every program with the dispatch.
class GlobalInitWireVisitor {

    private:

        Allocator &al;
        ASR::TranslationUnit_t &unit;

    public:

        GlobalInitWireVisitor(Allocator &al_, ASR::TranslationUnit_t &unit_):
            al(al_), unit(unit_) {}

        void dependency(ASR::asr_t *owner, const char *name,
                std::map<std::string, ASR::Function_t*> &deps) {
            if (name == nullptr) return;
            ASR::symbol_t *sym = unit.m_symtab->resolve_symbol(name);
            if (sym == nullptr || !ASR::is_a<ASR::Module_t>(*sym)
                    || (ASR::asr_t*)sym == owner) {
                return;
            }
            ASR::Function_t *fn = ASRUtils::get_global_init((ASR::asr_t*)sym);
            if (fn == nullptr) return;
            deps[ASRUtils::global_init_stable_id((ASR::asr_t*)sym)] = fn;
        }

        void imported_dependencies(ASR::asr_t *owner, SymbolTable *scope,
                std::map<std::string, ASR::Function_t*> &deps) {
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
                    ASR::ExternalSymbol_t *e = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
                    // The initializers the guard calls are imported too, and
                    // are not what the initialization reads.
                    ASR::symbol_t *target = ASRUtils::symbol_get_past_external(sym);
                    if (ASR::is_a<ASR::Function_t>(*target)
                            && ASRUtils::is_owner_global_init(
                                ASR::down_cast<ASR::Function_t>(target))) {
                        continue;
                    }
                    dependency(owner, e->m_module_name, deps);
                } else if (ASR::is_a<ASR::Block_t>(*sym)
                        || ASR::is_a<ASR::AssociateBlock_t>(*sym)
                        || ASR::is_a<ASR::Function_t>(*sym)) {
                    imported_dependencies(owner, ASRUtils::symbol_symtab(sym), deps);
                }
            }
        }

        // The storage of `owner`'s variables that a backend creates at run
        // time.
        // A module this translation unit defines storage for but that owns
        // no initializer: one semantics made for storage of its own, a COMMON
        // block above all, which every translation unit that uses it defines.
        static bool storage_module_without_initializer(SymbolTable *scope) {
            if (scope == nullptr || scope->asr_owner == nullptr
                    || scope->asr_owner->type != ASR::asrType::symbol) {
                return false;
            }
            ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
            return ASR::is_a<ASR::Module_t>(*sym) && ASRUtils::global_init_borrows_storage(
                *ASR::down_cast<ASR::Module_t>(sym));
        }

        std::vector<ASR::expr_t*> storage_targets(ASR::asr_t *owner) {
            std::vector<ASR::expr_t*> targets;
            if (owner->type == ASR::asrType::symbol
                    && ASR::is_a<ASR::Program_t>(*ASR::down_cast<ASR::symbol_t>(owner))) {
                // A program's variables are set up where its frame is.
                return targets;
            }
            // The translation unit's own initializer sets up, besides its own
            // variables, those of every module without one of its own.
            for (SymbolTable *scope : ASRUtils::global_init_storage_scopes(owner)) {
                for (auto &name : ASRUtils::determine_variable_declaration_order(scope)) {
                    ASR::symbol_t *sym = scope->get_symbol(name);
                    if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                    if (v->m_abi != ASR::abiType::Source) continue;
                    if (!ASRUtils::needs_runtime_storage_setup(*v)) continue;
                    targets.push_back(ASRUtils::EXPR(ASR::make_Var_t(al,
                        v->base.base.loc, sym)));
                }
            }
            return targets;
        }

        void finalize(ASR::asr_t *owner) {
            ASR::Function_t *fn = ASRUtils::get_global_init(owner);
            if (fn == nullptr || !ASRUtils::global_init_defined_here(fn)) return;
            // Already guarded: the pipeline can run its passes twice.
            if (fn->n_body == 1 && ASR::is_a<ASR::If_t>(*fn->m_body[0])
                    && ASRUtils::guarded_state(
                        ASR::down_cast<ASR::If_t>(fn->m_body[0])->m_test) != nullptr) {
                return;
            }
            ASR::Variable_t *state = ASRUtils::get_global_init_state(owner);
            LCOMPILERS_ASSERT(state != nullptr);
            const Location &loc = fn->base.base.loc;
            std::map<std::string, ASR::Function_t*> deps;
            if (owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
                // A program's initializer is frame local and runs after the
                // dispatch that starts the program has run every module's,
                // so it depends only on what its own statements import.
                if (ASR::is_a<ASR::Module_t>(*sym)) {
                    ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
                    dependency(owner, m->m_parent_module, deps);
                    for (size_t i = 0; i < m->n_dependencies; i++) {
                        dependency(owner, m->m_dependencies[i], deps);
                    }
                }
            }
            imported_dependencies(owner, fn->m_symtab, deps);

            ASR::expr_t *state_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                &state->base));
            std::vector<ASR::stmt_t*> body;
            for (auto &d : deps) {
                body.push_back(ASRUtils::make_global_init_call(al, fn->m_symtab,
                    d.second, loc));
            }
            std::vector<ASR::expr_t*> targets = storage_targets(owner);
            Vec<ASR::expr_t*> targets_vec; targets_vec.reserve(al, targets.size());
            for (ASR::expr_t *t : targets) {
                // A variable of a module without an initializer is out of
                // the translation unit initializer's sight: import it.
                ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(t);
                var->m_v = ASRUtils::import_symbol(al, var->m_v, fn->m_symtab);
                targets_vec.push_back(al, t);
            }
            body.push_back(ASRUtils::STMT(ASR::make_GlobalInitStorage_t(al, loc,
                targets_vec.p, targets_vec.n)));
            for (size_t i = 0; i < fn->n_body; i++) body.push_back(fn->m_body[i]);

            Vec<ASR::call_arg_t> args; args.reserve(al, 1);
            ASR::call_arg_t arg; arg.loc = loc; arg.m_value = state_var;
            args.push_back(al, arg);
            ASR::symbol_t *end = ASRUtils::get_init_runtime_function(al, unit,
                ASRUtils::InitRuntimeFn::End);
            body.push_back(ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al,
                loc, end, end, args.p, args.n, nullptr, nullptr, false)));

            ASR::symbol_t *begin = ASRUtils::get_init_runtime_function(al, unit,
                ASRUtils::InitRuntimeFn::Begin);
            ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::expr_t *begin_call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(
                al, loc, begin, begin, args.p, args.n, int_type, nullptr, nullptr));
            ASR::expr_t *test = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                begin_call, ASR::cmpopType::NotEq, ASRUtils::int32_constant(al, loc, 0),
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
            ASRUtils::ASRBuilder b(al, loc);
            Vec<ASR::stmt_t*> fn_body; fn_body.reserve(al, 1);
            fn_body.push_back(al, b.If(test, body, {}));
            fn->m_body = fn_body.p;
            fn->n_body = fn_body.size();
        }

        void wire_program(ASR::Program_t *p) {
            const Location &loc = p->base.base.loc;
            finalize((ASR::asr_t*)&p->base);
            for (size_t i = 0; i < p->n_body; i++) {
                if (ASR::is_a<ASR::GlobalInitDispatch_t>(*p->m_body[i])) return;
            }
            Vec<ASR::stmt_t*> body;
            body.reserve(al, p->n_body + 2);
            body.push_back(al, ASRUtils::STMT(ASR::make_GlobalInitDispatch_t(al, loc,
                ASR::init_dispatch_phaseType::InitDispatchCollective, nullptr, 0)));
            if (ASR::Function_t *own = ASRUtils::get_global_init((ASR::asr_t*)&p->base)) {
                body.push_back(al, ASRUtils::make_global_init_call(al, p->m_symtab,
                    own, loc));
            }
            for (size_t i = 0; i < p->n_body; i++) body.push_back(al, p->m_body[i]);
            p->m_body = body.p;
            p->n_body = body.size();
        }

        // Whether `f`, or a procedure of the translation unit it calls,
        // refers to a variable of the translation unit itself, the storage
        // its initializer sets up: a saved coarray a procedure declares is
        // hoisted there.
        bool uses_translation_unit_storage(ASR::Function_t *f,
                std::set<ASR::Function_t*> &visited) {
            if (!visited.insert(f).second) return false;
            struct Finder : ASR::BaseWalkVisitor<Finder> {
                SymbolTable *global;
                bool found = false;
                std::vector<ASR::Function_t*> callees;
                void visit_Var(const ASR::Var_t &x) {
                    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
                    if (ASR::is_a<ASR::Variable_t>(*sym)) {
                        SymbolTable *parent = ASR::down_cast<ASR::Variable_t>(sym)->m_parent_symtab;
                        if (parent == global || storage_module_without_initializer(parent)) {
                            found = true;
                        }
                    }
                }
                void callee(ASR::symbol_t *name) {
                    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(name);
                    if (ASR::is_a<ASR::Function_t>(*sym)
                            && ASRUtils::symbol_parent_symtab(sym) == global) {
                        callees.push_back(ASR::down_cast<ASR::Function_t>(sym));
                    }
                }
                void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
                    callee(x.m_name);
                    ASR::BaseWalkVisitor<Finder>::visit_SubroutineCall(x);
                }
                void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                    callee(x.m_name);
                    ASR::BaseWalkVisitor<Finder>::visit_FunctionCall(x);
                }
            };
            Finder finder;
            finder.global = unit.m_symtab;
            for (size_t i = 0; i < f->n_body && !finder.found; i++) {
                finder.visit_stmt(*f->m_body[i]);
            }
            if (finder.found) return true;
            for (auto &item : f->m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*item.second)
                        && uses_translation_unit_storage(
                            ASR::down_cast<ASR::Function_t>(item.second), visited)) {
                    return true;
                }
            }
            for (ASR::Function_t *c : finder.callees) {
                if (!ASRUtils::is_owner_global_init(c)
                        && uses_translation_unit_storage(c, visited)) {
                    return true;
                }
            }
            return false;
        }

        // A foreign entry into a procedure of the translation unit that
        // uses the translation unit's own storage also needs its
        // initializer.
        void add_translation_unit_ensure(ASR::Function_t *tu_init) {
            for (auto &item : unit.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(item.second);
                std::set<ASR::Function_t*> visited;
                if (f == tu_init || !uses_translation_unit_storage(f, visited)) continue;
                ASR::GlobalInitDispatch_t *d = nullptr;
                for (size_t i = 0; i < f->n_body && d == nullptr; i++) {
                    if (ASR::is_a<ASR::GlobalInitDispatch_t>(*f->m_body[i])) {
                        d = ASR::down_cast<ASR::GlobalInitDispatch_t>(f->m_body[i]);
                    }
                }
                if (d == nullptr) continue;
                bool present = false;
                for (size_t i = 0; i < d->n_ensures; i++) {
                    if (ASR::is_a<ASR::SubroutineCall_t>(*d->m_ensures[i])
                            && ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::SubroutineCall_t>(d->m_ensures[i])->m_name)
                                == &tu_init->base) {
                        present = true;
                    }
                }
                if (present) continue;
                Vec<ASR::stmt_t*> ensures;
                ensures.reserve(al, d->n_ensures + 1);
                for (size_t i = 0; i < d->n_ensures; i++) ensures.push_back(al, d->m_ensures[i]);
                ensures.push_back(al, ASRUtils::make_global_init_call(al, f->m_symtab,
                    tu_init, d->base.base.loc));
                d->m_ensures = ensures.p;
                d->n_ensures = ensures.size();
            }
        }

        // Device code has no host to enter the engine from; a routine the
        // device partition moved there loses its entry dispatch.
        void strip_device_dispatches(SymbolTable *scope) {
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (ASR::is_a<ASR::Function_t>(*sym)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
                    if (ASRUtils::is_device_only_function(*f)) {
                        Vec<ASR::stmt_t*> body;
                        body.reserve(al, f->n_body);
                        for (size_t i = 0; i < f->n_body; i++) {
                            if (!ASR::is_a<ASR::GlobalInitDispatch_t>(*f->m_body[i])) {
                                body.push_back(al, f->m_body[i]);
                            }
                        }
                        f->m_body = body.p;
                        f->n_body = body.size();
                    }
                    strip_device_dispatches(f->m_symtab);
                } else if (ASR::is_a<ASR::Module_t>(*sym)) {
                    strip_device_dispatches(ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
                }
            }
        }

        void visit_TranslationUnit() {
            for (auto &item : unit.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Module_t>(*item.second)) {
                    finalize((ASR::asr_t*)item.second);
                }
            }
            // The translation unit's own variables whose storage is created
            // at run time -- those of an interactive cell -- need its
            // initializer even when nothing else put anything there.
            if (unit.m_global_init == nullptr
                    && !storage_targets((ASR::asr_t*)&unit).empty()) {
                ASRUtils::get_or_create_global_init(al, unit, (ASR::asr_t*)&unit);
            }
            finalize((ASR::asr_t*)&unit);
            if (ASR::Function_t *tu_init = ASRUtils::get_global_init((ASR::asr_t*)&unit)) {
                add_translation_unit_ensure(tu_init);
            }
            std::vector<ASR::Program_t*> programs;
            for (auto &item : unit.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Program_t>(*item.second)) {
                    programs.push_back(ASR::down_cast<ASR::Program_t>(item.second));
                }
            }
            for (ASR::Program_t *p : programs) wire_program(p);
            strip_device_dispatches(unit.m_symtab);
        }

};

} // anonymous namespace

void pass_global_init(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GlobalInitVisitor v(al, unit, pass_options);
    v.visit_TranslationUnit();
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

void pass_global_init_wire(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    GlobalInitWireVisitor v(al, unit);
    v.visit_TranslationUnit();
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
