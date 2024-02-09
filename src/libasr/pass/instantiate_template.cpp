#include <cmath>
#include <libasr/asr_utils.h>
#include <libasr/asr.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

class SymbolRenamer : public ASR::BaseExprStmtDuplicator<SymbolRenamer>
{
public:
    SymbolTable* current_scope;
    std::map<std::string, ASR::ttype_t*> &type_subs;
    std::string new_sym_name;

    SymbolRenamer(Allocator& al, std::map<std::string, ASR::ttype_t*>& type_subs,
            SymbolTable* current_scope, std::string new_sym_name):
        BaseExprStmtDuplicator(al),
        current_scope{current_scope},
        type_subs{type_subs},
        new_sym_name{new_sym_name}
        {}

    ASR::symbol_t* rename_symbol(ASR::symbol_t *x) {
        switch (x->type) {
            case (ASR::symbolType::Variable): {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(x);
                return rename_Variable(v);
            }
            case (ASR::symbolType::Function): {
                if (current_scope->get_symbol(new_sym_name)) {
                    return current_scope->get_symbol(new_sym_name);
                }
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(x);
                return rename_Function(f);
            }
            default: {
                std::string sym_name = ASRUtils::symbol_name(x);
                throw new LCompilersException("Symbol renaming not supported "
                    " for " + sym_name);
            }
        }
    }

    ASR::symbol_t* rename_Variable(ASR::Variable_t *x) {
        ASR::ttype_t *t = x->m_type;
        ASR::dimension_t* tp_m_dims = nullptr;
        int tp_n_dims = ASRUtils::extract_dimensions_from_ttype(t, tp_m_dims);

        if (ASR::is_a<ASR::TypeParameter_t>(*t)) {
            ASR::TypeParameter_t *tp = ASR::down_cast<ASR::TypeParameter_t>(t);
            if (type_subs.find(tp->m_param) != type_subs.end()) {
                t = ASRUtils::make_Array_t_util(al, tp->base.base.loc,
                    ASRUtils::duplicate_type(al, type_subs[tp->m_param]),
                    tp_m_dims, tp_n_dims);
            } else {
                t = ASRUtils::make_Array_t_util(al, tp->base.base.loc, ASRUtils::TYPE(
                    ASR::make_TypeParameter_t(al, tp->base.base.loc,
                    s2c(al, new_sym_name))), tp_m_dims, tp_n_dims);
                type_subs[tp->m_param] = t;
            }
        }

        if (current_scope->get_symbol(new_sym_name)) {
            return current_scope->get_symbol(new_sym_name);
        }

        ASR::symbol_t* new_v = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
            al, x->base.base.loc,
            current_scope, s2c(al, new_sym_name), x->m_dependencies,
            x->n_dependencies, x->m_intent, x->m_symbolic_value,
            x->m_value, x->m_storage, t, x->m_type_declaration,
            x->m_abi, x->m_access, x->m_presence, x->m_value_attr));

        current_scope->add_symbol(new_sym_name, new_v);

        return new_v;
    }

    ASR::symbol_t* rename_Function(ASR::Function_t *x) {
        ASR::FunctionType_t* ftype = ASR::down_cast<ASR::FunctionType_t>(x->m_function_signature);

        SymbolTable* parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        Vec<ASR::expr_t*> args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            ASR::expr_t *new_arg = duplicate_expr(x->m_args[i]);
            args.push_back(al, new_arg);
        }

        ASR::expr_t *new_return_var_ref = nullptr;
        if (x->m_return_var != nullptr) {
            new_return_var_ref = duplicate_expr(x->m_return_var);
        }

        ASR::symbol_t *new_f = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(
            al, x->base.base.loc, current_scope, s2c(al, new_sym_name), x->m_dependencies,
            x->n_dependencies, args.p, args.size(), nullptr, 0, new_return_var_ref, ftype->m_abi,
            x->m_access, ftype->m_deftype, ftype->m_bindc_name, ftype->m_elemental,
            ftype->m_pure, ftype->m_module, ftype->m_inline, ftype->m_static, ftype->m_restrictions,
            ftype->n_restrictions, ftype->m_is_restriction, x->m_deterministic, x->m_side_effect_free));

        parent_scope->add_symbol(new_sym_name, new_f);
        current_scope = parent_scope;

        return new_f;
    }

    ASR::asr_t* duplicate_Var(ASR::Var_t *x) {
        std::string sym_name = ASRUtils::symbol_name(x->m_v);
        ASR::symbol_t* sym = duplicate_symbol(x->m_v);
        return ASR::make_Var_t(al, x->base.base.loc, sym);
    }

    ASR::symbol_t* duplicate_symbol(ASR::symbol_t *x) {
        ASR::symbol_t* new_symbol = nullptr;
        switch (x->type) {
            case ASR::symbolType::Variable: {
                new_symbol = duplicate_Variable(ASR::down_cast<ASR::Variable_t>(x));
                break;
            }
            default: {
                throw LCompilersException("Unsupported symbol for symbol renaming");
            }
        }
        return new_symbol;
    }

    ASR::symbol_t* duplicate_Variable(ASR::Variable_t *x) {
        ASR::symbol_t *v = current_scope->get_symbol(x->m_name);
        if (!v) {
            ASR::ttype_t *t = substitute_type(x->m_type);
            v = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
                al, x->base.base.loc, current_scope, x->m_name, x->m_dependencies,
                x->n_dependencies, x->m_intent, x->m_symbolic_value,
                x->m_value, x->m_storage, t, x->m_type_declaration,
                x->m_abi, x->m_access, x->m_presence, x->m_value_attr));
            current_scope->add_symbol(x->m_name, v);
        }
        return v;
    }

    ASR::ttype_t* substitute_type(ASR::ttype_t *ttype) {
        switch (ttype->type) {
            case (ASR::ttypeType::TypeParameter) : {
                ASR::TypeParameter_t *tp = ASR::down_cast<ASR::TypeParameter_t>(ttype);
                LCOMPILERS_ASSERT(type_subs.find(tp->m_param) != type_subs.end());
                return ASRUtils::duplicate_type(al, type_subs[tp->m_param]);
            }
            case (ASR::ttypeType::Array) : {
                ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(ttype);
                ASR::ttype_t *t = substitute_type(a->m_type);
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(ttype, m_dims);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, n_dims);
                for (size_t i = 0; i < n_dims; i++) {
                    ASR::dimension_t old_dim = m_dims[i];
                    ASR::dimension_t new_dim;
                    new_dim.loc = old_dim.loc;
                    new_dim.m_start = duplicate_expr(old_dim.m_start);
                    new_dim.m_length = duplicate_expr(old_dim.m_length);
                    new_dims.push_back(al, new_dim);
                }
                return ASRUtils::make_Array_t_util(al, t->base.loc,
                    t, new_dims.p, new_dims.size());
            }
            default : return ttype;
        }
    }

};

class SymbolInstantiator : public ASR::BaseExprStmtDuplicator<SymbolInstantiator>
{
public:
    SymbolTable* target_scope;                          // scope where the instantiation is
    SymbolTable* new_scope;                             // scope of the new symbol
    std::map<std::string,ASR::ttype_t*> type_subs;      // type name -> ASR type map based on instantiation's args
    std::map<std::string,ASR::symbol_t*>& symbol_subs;  // symbol name -> ASR symbol map based on instantiation's args
    std::string new_sym_name;                           // name for the new symbol
    ASR::symbol_t* sym;
    SetChar dependencies;

    SymbolInstantiator(Allocator &al,
            SymbolTable* target_scope,
            std::map<std::string,ASR::ttype_t*> type_subs,
            std::map<std::string,ASR::symbol_t*>& symbol_subs,
            std::string new_sym_name, ASR::symbol_t* sym):
        BaseExprStmtDuplicator(al),
        target_scope{target_scope},
        new_scope{target_scope},
        type_subs{type_subs},
        symbol_subs{symbol_subs},
        new_sym_name{new_sym_name}, sym{sym}
        {}

    ASR::symbol_t* instantiate() {
        std::string sym_name = ASRUtils::symbol_name(sym);

        // if passed as instantiation's argument
        if (symbol_subs.find(sym_name) != symbol_subs.end()) {
            ASR::symbol_t* added_sym = symbol_subs[sym_name];
            std::string added_sym_name = ASRUtils::symbol_name(added_sym);
            if (new_scope->resolve_symbol(added_sym_name)) {
                return new_scope->resolve_symbol(added_sym_name);
            }
        }

        // check current scope
        if (target_scope->get_symbol(sym_name) != nullptr) {
            return target_scope->get_symbol(sym_name);
        }

        switch (sym->type) {
            case (ASR::symbolType::Function) : {
                ASR::Function_t* x = ASR::down_cast<ASR::Function_t>(sym);
                return instantiate_Function(x);
            }
            case (ASR::symbolType::Variable) : {
                ASR::Variable_t* x = ASR::down_cast<ASR::Variable_t>(sym);
                return instantiate_Variable(x);
            }
            case (ASR::symbolType::Template) : {
                ASR::Template_t* x = ASR::down_cast<ASR::Template_t>(sym);
                return instantiate_Template(x);
            }
            case (ASR::symbolType::StructType) : {
                ASR::StructType_t* x = ASR::down_cast<ASR::StructType_t>(sym);
                return instantiate_StructType(x);
            }
            case (ASR::symbolType::ExternalSymbol) : {
                ASR::ExternalSymbol_t* x = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
                return instantiate_ExternalSymbol(x);
            }
            case (ASR::symbolType::ClassProcedure) : {
                ASR::ClassProcedure_t* x = ASR::down_cast<ASR::ClassProcedure_t>(sym);
                return instantiate_ClassProcedure(x);
            }
            case (ASR::symbolType::CustomOperator) : {
                ASR::CustomOperator_t* x = ASR::down_cast<ASR::CustomOperator_t>(sym);
                return instantiate_CustomOperator(x);
            }
            default: {
                std::string sym_name = ASRUtils::symbol_name(sym);
                throw LCompilersException("Instantiation of " + sym_name
                    + " symbol is not supported");
            };
        }
    }

    ASR::symbol_t* instantiate_Function(ASR::Function_t* x) {
        dependencies.clear(al);
        new_scope = al.make_new<SymbolTable>(target_scope);

        // duplicate symbol table
        for (auto const &sym_pair: x->m_symtab->get_scope()) {
            SymbolInstantiator t(al, new_scope, type_subs, symbol_subs,
                ASRUtils::symbol_name(sym_pair.second), sym_pair.second);
            t.instantiate();
        }

        Vec<ASR::expr_t*> args;
        args.reserve(al, x->n_args);
        ASR::expr_t *new_return_var_ref = nullptr;
        SetChar deps_vec;
        deps_vec.reserve(al, dependencies.size());

        for (size_t i=0; i<x->n_args; i++) {
            ASR::expr_t *new_arg = duplicate_expr(x->m_args[i]);
            args.push_back(al, new_arg);
        }

        if (x->m_return_var != nullptr) {
            new_return_var_ref = duplicate_expr(x->m_return_var);
        }

        for( size_t i = 0; i < dependencies.size(); i++ ) {
            char* dep = dependencies[i];
            deps_vec.push_back(al, dep);
        }

        ASR::asr_t *result = ASRUtils::make_Function_t_util(
            al, x->base.base.loc, new_scope, s2c(al, new_sym_name),
            deps_vec.p, deps_vec.size(), args.p, args.size(),
            nullptr, 0, new_return_var_ref,
            ASRUtils::get_FunctionType(x)->m_abi, x->m_access,
            ASRUtils::get_FunctionType(x)->m_deftype, ASRUtils::get_FunctionType(x)->m_bindc_name,
            ASRUtils::get_FunctionType(x)->m_elemental, ASRUtils::get_FunctionType(x)->m_pure,
            ASRUtils::get_FunctionType(x)->m_module, ASRUtils::get_FunctionType(x)->m_inline,
            ASRUtils::get_FunctionType(x)->m_static, ASRUtils::get_FunctionType(x)->m_restrictions,
            ASRUtils::get_FunctionType(x)->n_restrictions, false, false, false);

        ASR::symbol_t *f = ASR::down_cast<ASR::symbol_t>(result);
        target_scope->add_symbol(new_sym_name, f);
        symbol_subs[x->m_name] = f;

        return f;
    }

    ASR::symbol_t* instantiate_Variable(ASR::Variable_t* x) {
        ASR::ttype_t *new_type = substitute_type(x->m_type);

        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, new_type);

        ASR::symbol_t* s = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al,
            x->base.base.loc, target_scope, s2c(al, x->m_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), x->m_intent, nullptr, nullptr, x->m_storage,
            new_type, nullptr, x->m_abi, x->m_access, x->m_presence, x->m_value_attr));
        target_scope->add_symbol(x->m_name, s);

        return s;
    }

    ASR::symbol_t* instantiate_Template(ASR::Template_t* x) {
        new_scope = al.make_new<SymbolTable>(target_scope);

        // duplicate symbol table
        for (auto const &sym_pair: x->m_symtab->get_scope()) {
            SymbolInstantiator t(al, new_scope, type_subs, symbol_subs,
                ASRUtils::symbol_name(sym_pair.second), sym_pair.second);
            t.instantiate();
        }

        SetChar args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            char* arg_i = x->m_args[i];
            args.push_back(al, arg_i);
        }

        // TODO: fill the requires
        Vec<ASR::require_instantiation_t*> m_requires;
        m_requires.reserve(al, x->n_requires);
        for (size_t i=0; i<x->n_requires; i++) {
            m_requires.push_back(al, duplicate_Require(ASR::down_cast<ASR::Require_t>(x->m_requires[i])));
        }

        ASR::asr_t *result = ASR::make_Template_t(al, x->base.base.loc, new_scope,
            s2c(al, new_sym_name), args.p, args.size(), m_requires.p, m_requires.size());

        ASR::symbol_t *t = ASR::down_cast<ASR::symbol_t>(result);
        target_scope->add_symbol(new_sym_name, t);

        return t;
    }

    ASR::symbol_t* instantiate_StructType(ASR::StructType_t* x) {
        new_scope = al.make_new<SymbolTable>(target_scope);

        Vec<char*> data_member_names;
        data_member_names.reserve(al, x->n_members);
        for (size_t i=0; i<x->n_members; i++) {
            data_member_names.push_back(al, x->m_members[i]);
        }

        ASR::expr_t* m_alignment = duplicate_expr(x->m_alignment);

        ASR::asr_t* result = ASR::make_StructType_t(al, x->base.base.loc,
            new_scope, s2c(al, new_sym_name), nullptr, 0, data_member_names.p,
            data_member_names.size(), x->m_abi, x->m_access, x->m_is_packed,
            x->m_is_abstract, nullptr, 0, m_alignment, nullptr);

        ASR::symbol_t* s = ASR::down_cast<ASR::symbol_t>(result);
        target_scope->add_symbol(new_sym_name, s);
        symbol_subs[x->m_name] = s;

        for (auto const &sym_pair: x->m_symtab->get_scope()) {
            SymbolInstantiator t(al, new_scope, type_subs, symbol_subs,
                ASRUtils::symbol_name(sym_pair.second), sym_pair.second);
            t.instantiate();
        }

        return s;
    }

    ASR::symbol_t* instantiate_ExternalSymbol(ASR::ExternalSymbol_t* x) {
        std::string m_name = x->m_module_name;

        if (symbol_subs.find(m_name) != symbol_subs.end()) {
            std::string new_m_name = ASRUtils::symbol_name(symbol_subs[m_name]);
            std::string member_name = x->m_original_name;
            std::string new_e_name = "1_" + new_m_name + "_" + member_name;

            if (target_scope->get_symbol(new_e_name)) {
                return target_scope->get_symbol(new_e_name);
            }

            ASR::symbol_t* new_m_sym = target_scope->resolve_symbol(new_m_name);
            ASR::symbol_t* member_sym = ASRUtils::symbol_symtab(new_m_sym)->resolve_symbol(member_name);

            ASR::symbol_t* e = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                al, x->base.base.loc, target_scope, s2c(al, new_e_name), member_sym,
                s2c(al, new_m_name), nullptr, 0, s2c(al, member_name), x->m_access));
            target_scope->add_symbol(new_e_name, e);
            symbol_subs[x->m_name] = e;

            return e;
        }

        ASRUtils::SymbolDuplicator d(al);
        d.duplicate_symbol(x->m_parent_symtab->get_symbol(x->m_name), target_scope);
        return target_scope->get_symbol(x->m_name);
    }

    ASR::symbol_t* instantiate_ClassProcedure(ASR::ClassProcedure_t* x) {
        std::string new_cp_name = target_scope->parent->get_unique_name("__asr_" + std::string(x->m_name), false);
        ASR::symbol_t* cp_proc = x->m_proc;

        SymbolInstantiator t(al, target_scope->parent, type_subs, symbol_subs, new_cp_name, cp_proc);
        ASR::symbol_t* new_cp_proc = t.instantiate();
        symbol_subs[ASRUtils::symbol_name(cp_proc)] = new_cp_proc;

        ASR::symbol_t *new_x = ASR::down_cast<ASR::symbol_t>(ASR::make_ClassProcedure_t(
            al, x->base.base.loc, target_scope, x->m_name, x->m_self_argument,
            s2c(al, new_cp_name), new_cp_proc, x->m_abi, x->m_is_deferred, x->m_is_nopass));
        target_scope->add_symbol(x->m_name, new_x);

        return new_x;
    }

    ASR::symbol_t* instantiate_CustomOperator(ASR::CustomOperator_t* x) {
        return new_scope->resolve_symbol(x->m_name);
    }

    ASR::asr_t* duplicate_Var(ASR::Var_t *x) {
        std::string sym_name = ASRUtils::symbol_name(x->m_v);

        SymbolInstantiator t(al, new_scope, type_subs, symbol_subs, sym_name, x->m_v);
        ASR::symbol_t* sym = t.instantiate();

        return ASR::make_Var_t(al, x->base.base.loc, sym);
    }

    /* require */

    ASR::require_instantiation_t* duplicate_Require(ASR::Require_t* x) {
        SetChar r_args;
        r_args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            char* r_arg_i = x->m_args[i];
            r_args.push_back(al, r_arg_i);
        }

        return ASR::down_cast<ASR::require_instantiation_t>(
            ASR::make_Require_t(al, x->base.base.loc, s2c(al, x->m_name), r_args.p, r_args.size()));
    }

    /* utility */

    ASR::ttype_t* substitute_type(ASR::ttype_t *ttype) {
        switch (ttype->type) {
            case (ASR::ttypeType::TypeParameter) : {
                ASR::TypeParameter_t *param = ASR::down_cast<ASR::TypeParameter_t>(ttype);
                return ASRUtils::duplicate_type(al, type_subs[param->m_param]);
            }
            case (ASR::ttypeType::List) : {
                ASR::List_t *tlist = ASR::down_cast<ASR::List_t>(ttype);
                return ASRUtils::TYPE(ASR::make_List_t(al, ttype->base.loc,
                    substitute_type(tlist->m_type)));
            }
            case (ASR::ttypeType::Struct) : {
                ASR::Struct_t *s = ASR::down_cast<ASR::Struct_t>(ttype);
                std::string struct_name = ASRUtils::symbol_name(s->m_derived_type);
                if (symbol_subs.find(struct_name) != symbol_subs.end()) {
                    ASR::symbol_t *sym = symbol_subs[struct_name];
                    return ASRUtils::TYPE(ASR::make_Struct_t(al, ttype->base.loc, sym));
                }
                return ttype;
            }
            case (ASR::ttypeType::Array) : {
                ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(ttype);
                ASR::ttype_t *t = substitute_type(a->m_type);
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(ttype, m_dims);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, n_dims);
                for (size_t i = 0; i < n_dims; i++) {
                    ASR::dimension_t old_dim = m_dims[i];
                    ASR::dimension_t new_dim;
                    new_dim.loc = old_dim.loc;
                    new_dim.m_start = duplicate_expr(old_dim.m_start);
                    new_dim.m_length = duplicate_expr(old_dim.m_length);
                    new_dims.push_back(al, new_dim);
                }
                return ASRUtils::make_Array_t_util(al, t->base.loc,
                    t, new_dims.p, new_dims.size());
            }
            case (ASR::ttypeType::Allocatable) : {
                ASR::Allocatable_t *a = ASR::down_cast<ASR::Allocatable_t>(ttype);
                return ASRUtils::TYPE(ASR::make_Allocatable_t(al, ttype->base.loc,
                    substitute_type(a->m_type)));
            }
            case (ASR::ttypeType::Class) : {
                ASR::Class_t *c = ASR::down_cast<ASR::Class_t>(ttype);
                std::string class_name = ASRUtils::symbol_name(c->m_class_type);
                if (symbol_subs.find(class_name) != symbol_subs.end()) {
                    ASR::symbol_t *new_c = symbol_subs[class_name];
                    return ASRUtils::TYPE(ASR::make_Class_t(al, ttype->base.loc, new_c));
                }
                return ttype;
            }
            default : return ttype;
        }
    }

};

class BodyInstantiator : public ASR::BaseExprStmtDuplicator<BodyInstantiator>
{
public:
    SymbolTable* new_scope;
    std::map<std::string,ASR::ttype_t*> type_subs;
    std::map<std::string,ASR::symbol_t*>& symbol_subs;
    ASR::symbol_t* new_sym;
    ASR::symbol_t* sym;
    SetChar dependencies;

    BodyInstantiator(Allocator &al,
            std::map<std::string,ASR::ttype_t*> type_subs,
            std::map<std::string,ASR::symbol_t*>& symbol_subs,
            ASR::symbol_t* new_sym, ASR::symbol_t* sym):
        BaseExprStmtDuplicator(al),
        type_subs{type_subs},
        symbol_subs{symbol_subs},
        new_sym{new_sym}, sym{sym}
        {}

    void instantiate() {
        switch (sym->type) {
            case (ASR::symbolType::Function) : {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*new_sym));
                ASR::Function_t* x = ASR::down_cast<ASR::Function_t>(sym);
                instantiate_Function(x);
                break;
            }
            case (ASR::symbolType::Template) : {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Template_t>(*new_sym));
                ASR::Template_t* x = ASR::down_cast<ASR::Template_t>(sym);
                instantiate_Template(x);
                break;
            }
            case (ASR::symbolType::Variable) : {
                break;
            }
            case (ASR::symbolType::StructType) : {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::StructType_t>(*new_sym));
                ASR::StructType_t* x = ASR::down_cast<ASR::StructType_t>(sym);
                instantiate_StructType(x);
                break;
            }
            case (ASR::symbolType::ClassProcedure) : {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::ClassProcedure_t>(*new_sym));
                ASR::ClassProcedure_t* x = ASR::down_cast<ASR::ClassProcedure_t>(sym);
                instantiate_ClassProcedure(x);
                break;
            }
            case (ASR::symbolType::ExternalSymbol) : {
                break;
            }
            case (ASR::symbolType::CustomOperator) : {
                break;
            }
            default: {
                std::string sym_name = ASRUtils::symbol_name(sym);
                throw LCompilersException("Instantiation body of " + sym_name
                    + " symbol is not supported");
            };
        }
    }

    void instantiate_Function(ASR::Function_t* x) {
        ASR::Function_t* new_f = ASR::down_cast<ASR::Function_t>(new_sym);
        new_scope = new_f->m_symtab;

        for (auto const &sym_pair: x->m_symtab->get_scope()) {
            ASR::symbol_t* sym_i = sym_pair.second;

            SymbolInstantiator t_i(al, new_scope, type_subs, symbol_subs, sym_pair.first, sym_i);
            ASR::symbol_t* new_sym_i = t_i.instantiate();

            BodyInstantiator t_b(al, type_subs, symbol_subs, new_sym_i, sym_i);
            t_b.instantiate();
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, x->n_body);
        for (size_t i=0; i<x->n_body; i++) {
            ASR::stmt_t *new_body = this->duplicate_stmt(x->m_body[i]);
            if (new_body != nullptr) {
                body.push_back(al, new_body);
            }
        }

        SetChar deps_vec;
        deps_vec.reserve(al, new_f->n_dependencies + dependencies.size());
        for (size_t i=0; i<new_f->n_dependencies; i++) {
            char* dep = new_f->m_dependencies[i];
            deps_vec.push_back(al, dep);
        }
        for (size_t i=0; i<dependencies.size(); i++) {
            char* dep = dependencies[i];
            deps_vec.push_back(al, dep);
        }

        new_f->m_body = body.p;
        new_f->n_body = body.size();
        new_f->m_dependencies = deps_vec.p;
        new_f->n_dependencies = deps_vec.size();
    }

    void instantiate_Template(ASR::Template_t* x) {
        ASR::Template_t* new_t = ASR::down_cast<ASR::Template_t>(new_sym);

        for (auto const &sym_pair: new_t->m_symtab->get_scope()) {
            ASR::symbol_t* new_sym_i = sym_pair.second;
            ASR::symbol_t* sym_i = x->m_symtab->get_symbol(sym_pair.first);

            BodyInstantiator t(al, type_subs, symbol_subs, new_sym_i, sym_i);
            t.instantiate();
        }
    }

    void instantiate_StructType(ASR::StructType_t* x) {
        ASR::StructType_t* new_s = ASR::down_cast<ASR::StructType_t>(new_sym);

        for (auto const &sym_pair: new_s->m_symtab->get_scope()) {
            ASR::symbol_t* new_sym_i = sym_pair.second;
            ASR::symbol_t* sym_i = x->m_symtab->get_symbol(sym_pair.first);

            BodyInstantiator t(al, type_subs, symbol_subs, new_sym_i, sym_i);
            t.instantiate();
        }
    }

    void instantiate_ClassProcedure(ASR::ClassProcedure_t* x) {
        ASR::ClassProcedure_t* new_c = ASR::down_cast<ASR::ClassProcedure_t>(new_sym);

        ASR::symbol_t* new_proc = new_c->m_proc;
        ASR::symbol_t* proc = x->m_proc;

        BodyInstantiator t(al, type_subs, symbol_subs, new_proc, proc);
        t.instantiate();
    }

    /* expr */

    ASR::asr_t* duplicate_Var(ASR::Var_t* x) {
        std::string sym_name = ASRUtils::symbol_name(x->m_v);

        SymbolInstantiator t_i(al, new_scope, type_subs, symbol_subs, sym_name, x->m_v);
        ASR::symbol_t* sym = t_i.instantiate();

        BodyInstantiator t_b(al, type_subs, symbol_subs, sym, x->m_v);
        t_b.instantiate();

        return ASR::make_Var_t(al, x->base.base.loc, sym);
    }

    ASR::asr_t* duplicate_FunctionCall(ASR::FunctionCall_t* x) {
        Vec<ASR::call_arg_t> args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            ASR::call_arg_t new_arg;
            new_arg.loc = x->m_args[i].loc;
            new_arg.m_value = duplicate_expr(x->m_args[i].m_value);
            args.push_back(al, new_arg);
        }

        ASR::ttype_t* type = substitute_type(x->m_type);
        ASR::expr_t* value = duplicate_expr(x->m_value);
        ASR::expr_t* dt = duplicate_expr(x->m_dt);

        std::string call_name = ASRUtils::symbol_name(x->m_name);
        ASR::symbol_t* name = new_scope->resolve_symbol(call_name);

        // requirement function
        if (symbol_subs.find(call_name) != symbol_subs.end()) {
            name = symbol_subs[call_name];
        }

        // function call found in body that needs to be instantiated
        if (name == nullptr) {
            ASR::symbol_t* nested_sym = ASRUtils::symbol_symtab(sym)->resolve_symbol(call_name);
            SymbolTable* target_scope = ASRUtils::symbol_parent_symtab(new_sym);
            std::string nested_sym_name = target_scope->get_unique_name("__asr_" + call_name, false);

            SymbolInstantiator t_i(al, target_scope, type_subs, symbol_subs, nested_sym_name, nested_sym);
            name = t_i.instantiate();
            symbol_subs[call_name] = name;

            BodyInstantiator t_b(al, type_subs, symbol_subs, name, nested_sym);
            t_b.instantiate();
        }

        if (ASRUtils::symbol_parent_symtab(name)->get_counter() != new_scope->get_counter()
                && !ASR::is_a<ASR::ExternalSymbol_t>(*name)) {
            ADD_ASR_DEPENDENCIES(new_scope, name, dependencies);
        }

        return ASRUtils::make_FunctionCall_t_util(al, x->base.base.loc, name,
            x->m_original_name, args.p, args.size(), type, value, dt);
    }

    ASR::asr_t* duplicate_SubroutineCall(ASR::SubroutineCall_t* x) {
        Vec<ASR::call_arg_t> args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            ASR::call_arg_t new_arg;
            new_arg.loc = x->m_args[i].loc;
            new_arg.m_value = duplicate_expr(x->m_args[i].m_value);
            args.push_back(al, new_arg);
        }

        ASR::expr_t* dt = duplicate_expr(x->m_dt);

        std::string call_name = ASRUtils::symbol_name(x->m_name);
        ASR::symbol_t* name = new_scope->resolve_symbol(call_name);

        // requirement function
        if (symbol_subs.find(call_name) != symbol_subs.end()) {
            name = symbol_subs[call_name];
        }

        // function call found in body that needs to be instantiated
        if (name == nullptr) {
            ASR::symbol_t* nested_sym = ASRUtils::symbol_symtab(sym)->resolve_symbol(call_name);
            SymbolTable* target_scope = ASRUtils::symbol_parent_symtab(new_sym);
            std::string nested_sym_name = target_scope->get_unique_name("__asr_" + call_name, false);

            SymbolInstantiator t_i(al, target_scope, type_subs, symbol_subs, nested_sym_name, nested_sym);
            name = t_i.instantiate();
            symbol_subs[call_name] = name;

            BodyInstantiator t_b(al, type_subs, symbol_subs, name, nested_sym);
            t_b.instantiate();
        }

        if (ASRUtils::symbol_parent_symtab(name)->get_counter() != new_scope->get_counter()
                && !ASR::is_a<ASR::ExternalSymbol_t>(*name)) {
            ADD_ASR_DEPENDENCIES(new_scope, name, dependencies);
        }

        return ASRUtils::make_SubroutineCall_t_util(al, x->base.base.loc, name,
            x->m_original_name, args.p, args.size(), dt, nullptr, false,
            ASRUtils::get_class_proc_nopass_val(x->m_name));
    }

    ASR::asr_t* duplicate_DoLoop(ASR::DoLoop_t *x) {
        Vec<ASR::stmt_t*> m_body;
        m_body.reserve(al, x->n_body);
        for (size_t i=0; i<x->n_body; i++) {
            m_body.push_back(al, duplicate_stmt(x->m_body[i]));
        }
        ASR::do_loop_head_t head;
        head.m_v = duplicate_expr(x->m_head.m_v);
        head.m_start = duplicate_expr(x->m_head.m_start);
        head.m_end = duplicate_expr(x->m_head.m_end);
        head.m_increment = duplicate_expr(x->m_head.m_increment);
        head.loc = x->m_head.m_v->base.loc;
        return ASR::make_DoLoop_t(al, x->base.base.loc, x->m_name, head, m_body.p, x->n_body);
    }

    ASR::asr_t* duplicate_ArrayItem(ASR::ArrayItem_t *x) {
        ASR::expr_t *m_v = duplicate_expr(x->m_v);
        ASR::expr_t *m_value = duplicate_expr(x->m_value);

        Vec<ASR::array_index_t> args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            args.push_back(al, duplicate_array_index(x->m_args[i]));
        }

        ASR::ttype_t *type = substitute_type(x->m_type);

        return ASRUtils::make_ArrayItem_t_util(al, x->base.base.loc, m_v, args.p, x->n_args,
            ASRUtils::type_get_past_pointer(
                ASRUtils::type_get_past_allocatable(type)), x->m_storage_format, m_value);
    }

    ASR::asr_t* duplicate_ArrayConstant(ASR::ArrayConstant_t *x) {
        Vec<ASR::expr_t*> m_args;
        m_args.reserve(al, x->n_args);
        for (size_t i = 0; i < x->n_args; i++) {
            m_args.push_back(al, self().duplicate_expr(x->m_args[i]));
        }
        ASR::ttype_t* m_type = substitute_type(x->m_type);
        return make_ArrayConstant_t(al, x->base.base.loc, m_args.p, x->n_args, m_type, x->m_storage_format);
    }

    ASR::asr_t* duplicate_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t *x) {
        ASR::expr_t *arg = duplicate_expr(x->m_arg);
        ASR::ttype_t *ttype = substitute_type(x->m_type);
        ASR::expr_t *value = duplicate_expr(x->m_value);
        return ASR::make_ArrayPhysicalCast_t(al, x->base.base.loc,
            arg, x->m_old, x->m_new, ttype, value);
    }

    ASR::asr_t* duplicate_ArraySection(ASR::ArraySection_t *x) {
        ASR::expr_t *v = duplicate_expr(x->m_v);

        Vec<ASR::array_index_t> args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            args.push_back(al, duplicate_array_index(x->m_args[i]));
        }

        ASR::ttype_t *ttype = substitute_type(x->m_type);
        ASR::expr_t *value = duplicate_expr(x->m_value);

        return ASR::make_ArraySection_t(al, x->base.base.loc,
            v, args.p, args.size(), ttype, value);
    }

    ASR::asr_t* duplicate_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        ASR::expr_t *v = duplicate_expr(x->m_v);
        ASR::ttype_t *t = substitute_type(x->m_type);
        ASR::expr_t *value = duplicate_expr(x->m_value);

        std::string s_name = ASRUtils::symbol_name(x->m_m);
        SymbolInstantiator t_i(al, new_scope, type_subs, symbol_subs, s_name, x->m_m);
        ASR::symbol_t *s = t_i.instantiate();

        return ASR::make_StructInstanceMember_t(al, x->base.base.loc, v, s, t, value);
    }

    ASR::asr_t* duplicate_ListItem(ASR::ListItem_t *x) {
        ASR::expr_t *m_a = duplicate_expr(x->m_a);
        ASR::expr_t *m_pos = duplicate_expr(x->m_pos);
        ASR::ttype_t *type = substitute_type(x->m_type);
        ASR::expr_t *m_value = duplicate_expr(x->m_value);

        return ASR::make_ListItem_t(al, x->base.base.loc,
            m_a, m_pos, type, m_value);
    }

    ASR::asr_t* duplicate_Cast(ASR::Cast_t *x) {
        ASR::expr_t *arg = duplicate_expr(x->m_arg);
        ASR::ttype_t *type = substitute_type(ASRUtils::expr_type(x->m_arg));
        if (ASRUtils::is_real(*type)) {
            return (ASR::asr_t*) arg;
        }
        return ASRUtils::make_Cast_t_value(al, x->base.base.loc, arg, ASR::cast_kindType::IntegerToReal, x->m_type);
    }

    /* stmt */

    ASR::asr_t* duplicate_Assignment(ASR::Assignment_t *x) {
        ASR::expr_t *target = duplicate_expr(x->m_target);
        ASR::expr_t *value = duplicate_expr(x->m_value);
        ASR::stmt_t *overloaded = duplicate_stmt(x->m_overloaded);
        return ASR::make_Assignment_t(al, x->base.base.loc, target, value, overloaded);
    }

    /* array_index */

    ASR::array_index_t duplicate_array_index(ASR::array_index_t x) {
        ASR::expr_t *left = duplicate_expr(x.m_left);
        ASR::expr_t *right = duplicate_expr(x.m_right);
        ASR::expr_t *step = duplicate_expr(x.m_step);
        ASR::array_index_t result;
        result.m_left = left;
        result.m_right = right;
        result.m_step = step;
        return result;
    }

    /* utility */

    // TODO: join this with the other substitute_type
    ASR::ttype_t* substitute_type(ASR::ttype_t *ttype) {
        switch (ttype->type) {
            case (ASR::ttypeType::TypeParameter) : {
                ASR::TypeParameter_t *param = ASR::down_cast<ASR::TypeParameter_t>(ttype);
                return ASRUtils::duplicate_type(al, type_subs[param->m_param]);
            }
            case (ASR::ttypeType::List) : {
                ASR::List_t *tlist = ASR::down_cast<ASR::List_t>(ttype);
                return ASRUtils::TYPE(ASR::make_List_t(al, ttype->base.loc,
                    substitute_type(tlist->m_type)));
            }
            case (ASR::ttypeType::Struct) : {
                ASR::Struct_t *s = ASR::down_cast<ASR::Struct_t>(ttype);
                std::string struct_name = ASRUtils::symbol_name(s->m_derived_type);
                if (symbol_subs.find(struct_name) != symbol_subs.end()) {
                    ASR::symbol_t *sym = symbol_subs[struct_name];
                    ttype = ASRUtils::TYPE(ASR::make_Struct_t(al, s->base.base.loc, sym));
                }
                return ttype;
            }
            case (ASR::ttypeType::Array) : {
                ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(ttype);
                ASR::ttype_t *t = substitute_type(a->m_type);
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(ttype, m_dims);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, n_dims);
                for (size_t i = 0; i < n_dims; i++) {
                    ASR::dimension_t old_dim = m_dims[i];
                    ASR::dimension_t new_dim;
                    new_dim.loc = old_dim.loc;
                    new_dim.m_start = duplicate_expr(old_dim.m_start);
                    new_dim.m_length = duplicate_expr(old_dim.m_length);
                    new_dims.push_back(al, new_dim);
                }
                return ASRUtils::make_Array_t_util(al, t->base.loc,
                    t, new_dims.p, new_dims.size());
            }
            case (ASR::ttypeType::Allocatable): {
                ASR::Allocatable_t *a = ASR::down_cast<ASR::Allocatable_t>(ttype);
                return ASRUtils::TYPE(ASR::make_Allocatable_t(al, ttype->base.loc,
                    substitute_type(a->m_type)));
            }
            case (ASR::ttypeType::Class) : {
                ASR::Class_t *c = ASR::down_cast<ASR::Class_t>(ttype);
                std::string class_name = ASRUtils::symbol_name(c->m_class_type);
                if (symbol_subs.find(class_name) != symbol_subs.end()) {
                    ASR::symbol_t *new_c = symbol_subs[class_name];
                    return ASRUtils::TYPE(ASR::make_Class_t(al, ttype->base.loc, new_c));
                }
                return ttype;
            }
            default : return ttype;
        }
    }
};

ASR::symbol_t* instantiate_symbol(Allocator &al,
        SymbolTable *target_scope,
        std::map<std::string,ASR::ttype_t*> type_subs,
        std::map<std::string,ASR::symbol_t*>& symbol_subs,
        std::string new_sym_name, ASR::symbol_t *sym) {
    SymbolInstantiator t(al, target_scope, type_subs, symbol_subs, new_sym_name, sym);
    return t.instantiate();
}

void instantiate_body(Allocator &al,
        std::map<std::string,ASR::ttype_t*> type_subs,
        std::map<std::string,ASR::symbol_t*>& symbol_subs,
        ASR::symbol_t *new_sym, ASR::symbol_t *sym) {
    BodyInstantiator t(al, type_subs, symbol_subs, new_sym, sym);
    t.instantiate();
}

ASR::symbol_t* rename_symbol(Allocator &al,
        std::map<std::string, ASR::ttype_t*> &type_subs,
        SymbolTable *current_scope,
        std::string new_sym_name, ASR::symbol_t *sym) {
    SymbolRenamer t(al, type_subs, current_scope, new_sym_name);
    return t.rename_symbol(sym);
}

bool check_restriction(std::map<std::string, ASR::ttype_t*> type_subs,
        std::map<std::string, ASR::symbol_t*> &symbol_subs,
        ASR::Function_t *f, ASR::symbol_t *sym_arg, const Location &loc,
        diag::Diagnostics &diagnostics,
        const std::function<void ()> semantic_abort, bool report=true) {
    std::string f_name = f->m_name;
    ASR::Function_t *arg = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(sym_arg));
    std::string arg_name = arg->m_name;
    if (f->n_args != arg->n_args) {
        if (report) {
            std::string f_narg = std::to_string(f->n_args);
            std::string arg_narg = std::to_string(arg->n_args);
            diagnostics.add(diag::Diagnostic(
                "Number of arguments mismatch, restriction expects a function with " + f_narg
                    + " parameters, but a function with " + arg_narg + " parameters is provided",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label(arg_name + " has " + arg_narg + " parameters",
                                {loc, arg->base.base.loc}),
                        diag::Label(f_name + " has " + f_narg + " parameters",
                                {f->base.base.loc})
                    }
            ));
            semantic_abort();
        }
        return false;
    }
    for (size_t i = 0; i < f->n_args; i++) {
        ASR::ttype_t *f_param = ASRUtils::expr_type(f->m_args[i]);
        ASR::ttype_t *arg_param = ASRUtils::expr_type(arg->m_args[i]);
        if (!ASRUtils::types_equal_with_substitution(f_param, arg_param, type_subs)) {
            if (report) {
                std::string rtype = ASRUtils::type_to_str_with_substitution(f_param, type_subs);
                std::string rvar = ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
                std::string atype = ASRUtils::type_to_str(arg_param);
                std::string avar = ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(arg->m_args[i])->m_v);
                diagnostics.add(diag::Diagnostic(
                    "Restriction type mismatch with provided function argument",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc}),
                        diag::Label("Restriction's parameter " + rvar + " of type " + rtype,
                                {f->m_args[i]->base.loc}),
                        diag::Label("Function's parameter " + avar + " of type " + atype,
                                {arg->m_args[i]->base.loc})
                    }
                ));
                semantic_abort();
            }
            return false;
        }
    }
    if (f->m_return_var) {
        if (!arg->m_return_var) {
            if (report) {
                std::string msg = "The restriction argument " + arg_name
                    + " should have a return value";
                diagnostics.add(diag::Diagnostic(msg,
                    diag::Level::Error, diag::Stage::Semantic, {diag::Label("", {loc})}));
                semantic_abort();
            }
            return false;
        }
        ASR::ttype_t *f_ret = ASRUtils::expr_type(f->m_return_var);
        ASR::ttype_t *arg_ret = ASRUtils::expr_type(arg->m_return_var);
        if (!ASRUtils::types_equal_with_substitution(f_ret, arg_ret, type_subs)) {
            if (report) {
                std::string rtype = ASRUtils::type_to_str_with_substitution(f_ret, type_subs);
                std::string atype = ASRUtils::type_to_str(arg_ret);
                diagnostics.add(diag::Diagnostic(
                    "Restriction type mismatch with provided function argument",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc}),
                        diag::Label("Requirement's return type " + rtype,
                            {f->m_return_var->base.loc}),
                        diag::Label("Function's return type " + atype,
                            {arg->m_return_var->base.loc})
                    }
                ));
                semantic_abort();
            }
            return false;
        }
    } else {
        if (arg->m_return_var) {
            if (report) {
                std::string msg = "The restriction argument " + arg_name
                    + " should not have a return value";
                diagnostics.add(diag::Diagnostic(msg,
                    diag::Level::Error, diag::Stage::Semantic, {diag::Label("", {loc})}));
                semantic_abort();
            }
            return false;
        }
    }
    symbol_subs[f_name] = sym_arg;
    return true;
}

} // namespace LCompilers
