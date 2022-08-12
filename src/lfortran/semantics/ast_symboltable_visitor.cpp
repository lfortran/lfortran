#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <cmath>
#include <limits>
#include <queue>

#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/exception.h>
#include <lfortran/semantics/asr_implicit_cast_rules.h>
#include <lfortran/semantics/ast_common_visitor.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/parser/parser_stype.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>

namespace LFortran {

template <typename T>
void extract_bind(T &x, ASR::abiType &abi_type, char *&bindc_name) {
    if (x.m_bind) {
        AST::Bind_t *bind = AST::down_cast<AST::Bind_t>(x.m_bind);
        if (bind->n_args == 1) {
            if (AST::is_a<AST::Name_t>(*bind->m_args[0])) {
                AST::Name_t *name = AST::down_cast<AST::Name_t>(
                    bind->m_args[0]);
                if (to_lower(std::string(name->m_id)) == "c") {
                    abi_type=ASR::abiType::BindC;
                } else {
                    throw SemanticError("Unsupported language in bind()",
                        x.base.base.loc);
                }
            } else {
                    throw SemanticError("Language name must be specified in bind() as plain text",
                        x.base.base.loc);
            }
        } else {
            throw SemanticError("At least one argument needed in bind()",
                x.base.base.loc);
        }
        if (bind->n_kwargs == 1) {
            char *arg = bind->m_kwargs[0].m_arg;
            AST::expr_t *value = bind->m_kwargs[0].m_value;
            if (to_lower(std::string(arg)) == "name") {
                if (AST::is_a<AST::String_t>(*value)) {
                    AST::String_t *name = AST::down_cast<AST::String_t>(value);
                    bindc_name = name->m_s;
                } else {
                    throw SemanticError("The value of the 'name' keyword argument in bind(c) must be a string",
                        x.base.base.loc);
                }
            } else {
                throw SemanticError("Unsupported keyword argument in bind()",
                    x.base.base.loc);
            }
        }
    }
}

class SymbolTableVisitor : public CommonVisitor<SymbolTableVisitor> {
public:
    SymbolTable *global_scope;
    std::map<std::string, std::vector<std::string>> generic_procedures;
    std::map<std::string, std::map<std::string, std::vector<std::string>>> generic_class_procedures;
    std::map<AST::intrinsicopType, std::vector<std::string>> overloaded_op_procs;
    std::map<std::string, std::vector<std::string>> defined_op_procs;
    std::map<std::string, std::map<std::string, std::string>> class_procedures;
    std::vector<std::string> assgn_proc_names;
    std::string dt_name;
    bool in_submodule = false;
    bool is_interface = false;
    std::string interface_name = "";
    ASR::symbol_t *current_module_sym;

    std::map<AST::intrinsicopType, std::string> intrinsic2str = {
        {AST::intrinsicopType::STAR, "~mul"},
        {AST::intrinsicopType::PLUS, "~add"},
        {AST::intrinsicopType::EQ, "~eq"},
        {AST::intrinsicopType::NOTEQ, "~noteq"},
        {AST::intrinsicopType::LT, "~lt"},
        {AST::intrinsicopType::LTE, "~lte"},
        {AST::intrinsicopType::GT, "~gt"},
        {AST::intrinsicopType::GTE, "~gte"}
    };

    SymbolTableVisitor(Allocator &al, SymbolTable *symbol_table,
        diag::Diagnostics &diagnostics, CompilerOptions &compiler_options)
      : CommonVisitor(al, symbol_table, diagnostics, compiler_options) {}


    ASR::symbol_t* resolve_symbol(const Location &loc, const std::string &sub_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *sub = scope->resolve_symbol(sub_name);
        if (!sub) {
            throw SemanticError("Symbol '" + sub_name + "' not declared", loc);
        }
        return sub;
    }

    void visit_TranslationUnit(const AST::TranslationUnit_t &x) {
        if (!current_scope) {
            current_scope = al.make_new<SymbolTable>(nullptr);
        }
        LFORTRAN_ASSERT(current_scope != nullptr);
        global_scope = current_scope;

        // Create the TU early, so that asr_owner is set, so that
        // ASRUtils::get_tu_symtab() can be used, which has an assert
        // for asr_owner.
        ASR::asr_t *tmp0 = ASR::make_TranslationUnit_t(al, x.base.base.loc,
            current_scope, nullptr, 0);

        for (size_t i=0; i<x.n_items; i++) {
            AST::astType t = x.m_items[i]->type;
            if (t != AST::astType::expr && t != AST::astType::stmt) {
                visit_ast(*x.m_items[i]);
            }
        }
        global_scope = nullptr;
        tmp = tmp0;
    }

    void visit_Private(const AST::Private_t&) {
        // To Be Implemented
    }

    void visit_FinalName(const AST::FinalName_t&) {
        // To Be Implemented
    }

    void fix_type_info() {
        for( TypeMissingData* data: type_info ) {
            if( data->sym_type == -1 ) {
                continue;
            }
            ASR::expr_t* expr = nullptr;
            if( data->sym_type == (int64_t) ASR::symbolType::Function ) {
                SymbolTable* current_scope_copy = current_scope;
                current_scope = data->scope;
                visit_expr(*data->expr);
                expr = ASRUtils::EXPR(tmp);
                current_scope = current_scope_copy;
            }
            if( expr ) {
                switch( data->type->type ) {
                    case ASR::ttypeType::Character: {
                        ASR::Character_t* char_type = ASR::down_cast<ASR::Character_t>(data->type);
                        if( expr->type == ASR::exprType::FunctionCall ) {
                            char_type->m_len_expr = expr;
                            char_type->m_len = -3;
                        }
                        break;
                    }
                    default: {
                        throw SemanticError("Only Character type is supported as of now.", data->type->base.loc);
                    }
                }
            }
        }
        type_info.clear();
    }

    template <typename T, typename R>
    void visit_ModuleSubmoduleCommon(const T &x, std::string parent_name="") {
        assgn_proc_names.clear();
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        current_module_dependencies.reserve(al, 4);
        generic_procedures.clear();
        ASR::asr_t *tmp0 = ASR::make_Module_t(al, x.base.base.loc,
                                            /* a_symtab */ current_scope,
                                            /* a_name */ s2c(al, to_lower(x.m_name)),
                                            nullptr,
                                            0,
                                            false, false);
        current_module_sym = ASR::down_cast<ASR::symbol_t>(tmp0);
        if( x.class_type == AST::modType::Submodule ) {
            std::string rl_path = get_runtime_library_dir();
            ASR::symbol_t* submod_parent = (ASR::symbol_t*)(ASRUtils::load_module(al, global_scope,
                                                parent_name, x.base.base.loc, false,
                                                rl_path, true,
                                                [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }
                                                ));
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(submod_parent);
            std::string unsupported_sym_name = import_all(m);
            if( !unsupported_sym_name.empty() ) {
                throw LCompilersException("'" + unsupported_sym_name + "' is not supported yet for declaring with use.");
            }
        }
        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }
        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }
        current_module_sym = nullptr;
        add_generic_procedures();
        add_overloaded_procedures();
        add_class_procedures();
        add_generic_class_procedures();
        add_assignment_procedures();
        tmp = tmp0;
        // Add module dependencies
        R *m = ASR::down_cast2<R>(tmp);
        m->m_dependencies = current_module_dependencies.p;
        m->n_dependencies = current_module_dependencies.n;
        std::string sym_name = to_lower(x.m_name);
        if (parent_scope->get_symbol(sym_name) != nullptr) {
            throw SemanticError("Module already defined", tmp->loc);
        }
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        fix_type_info();
    }

    void visit_Module(const AST::Module_t &x) {
        in_module = true;
        visit_ModuleSubmoduleCommon<AST::Module_t, ASR::Module_t>(x);
        in_module = false;
    }

    void visit_Submodule(const AST::Submodule_t &x) {
        in_submodule = true;
        visit_ModuleSubmoduleCommon<AST::Submodule_t, ASR::Module_t>(x, std::string(x.m_id));
        in_submodule = false;
    }

    void visit_Program(const AST::Program_t &x) {
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        current_module_dependencies.reserve(al, 4);
        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }
        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }
        tmp = ASR::make_Program_t(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(x.m_name)),
            current_module_dependencies.p,
            current_module_dependencies.n,
            /* a_body */ nullptr,
            /* n_body */ 0);
        std::string sym_name = to_lower(x.m_name);
        if (parent_scope->get_symbol(sym_name) != nullptr) {
            throw SemanticError("Program already defined", tmp->loc);
        }
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        fix_type_info();
    }

    void visit_Subroutine(const AST::Subroutine_t &x) {
        ASR::accessType s_access = dflt_access;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            current_procedure_args.push_back(to_lower(arg));
        }
        current_procedure_abi_type = ASR::abiType::Source;
        char *bindc_name=nullptr;
        extract_bind(x, current_procedure_abi_type, bindc_name);

        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }
        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }
        Vec<ASR::expr_t*> args;
        args.reserve(al, x.n_args);
        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            std::string arg_s = to_lower(arg);
            if (current_scope->get_symbol(arg_s) == nullptr) {
                if (compiler_options.implicit_typing) {
                    declare_implicit_variable(x.base.base.loc, arg_s,
                        ASRUtils::intent_unspecified);
                } else {
                    throw SemanticError("Dummy argument '" + arg_s + "' not defined", x.base.base.loc);
                }
            }
            ASR::symbol_t *var = current_scope->get_symbol(arg_s);
            args.push_back(al, LFortran::ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                var)));
        }
        std::string sym_name = to_lower(x.m_name);
        if (assgnd_access.count(sym_name)) {
            s_access = assgnd_access[sym_name];
        }
        if (is_interface){
            deftype = ASR::deftypeType::Interface;
        }
        bool is_pure = false, is_module = false;
        for( size_t i = 0; i < x.n_attributes; i++ ) {
            switch( x.m_attributes[i]->type ) {
                case AST::decl_attributeType::SimpleAttribute: {
                    AST::SimpleAttribute_t* simple_attr = AST::down_cast<AST::SimpleAttribute_t>(x.m_attributes[i]);
                    if( simple_attr->m_attr == AST::simple_attributeType::AttrPure ) {
                        is_pure = true;
                    } else if( simple_attr->m_attr == AST::simple_attributeType::AttrModule ) {
                        is_module = true;
                    }
                    break;
                }
                default: {
                    // Continue with the original behaviour
                    // of not processing unrequired attributes
                    break;
                }
            }
        }
        if (parent_scope->get_symbol(sym_name) != nullptr) {
            ASR::symbol_t *f1 = parent_scope->get_symbol(sym_name);
            ASR::Function_t *f2 = nullptr;
            if( ASR::is_a<ASR::Function_t>(*f1) ) {
                f2 = ASR::down_cast<ASR::Function_t>(f1);
            }
            if ((f1->type == ASR::symbolType::ExternalSymbol && in_submodule) ||
                f2->m_abi == ASR::abiType::Interactive) {
                // Previous declaration will be shadowed
                parent_scope->erase_symbol(sym_name);
            } else {
                throw SemanticError("Subroutine already defined", tmp->loc);
            }
        }
        if( sym_name == interface_name ) {
            parent_scope->erase_symbol(sym_name);
            sym_name = sym_name + "~genericprocedure";
        }


        tmp = ASR::make_Function_t(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(sym_name)),
            /* a_args */ args.p,
            /* n_args */ args.size(),
            nullptr, 0,
            /* a_body */ nullptr,
            /* n_body */ 0,
            nullptr,
            current_procedure_abi_type,
            s_access, deftype, bindc_name,
            is_pure, is_module, false);
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        /* FIXME: This can become incorrect/get cleared prematurely, perhaps
           in nested functions, and also in callback.f90 test, but it may not
           matter since we would have already checked the intent */
        current_procedure_args.clear();
        current_procedure_abi_type = ASR::abiType::Source;
    }

    AST::AttrType_t* find_return_type(AST::decl_attribute_t** attributes,
            size_t n, const Location &loc) {
        AST::AttrType_t* r = nullptr;
        bool found = false;
        for (size_t i=0; i<n; i++) {
            if (AST::is_a<AST::AttrType_t>(*attributes[i])) {
                if (found) {
                    throw SemanticError("Return type declared twice", loc);
                } else {
                    r = AST::down_cast<AST::AttrType_t>(attributes[i]);
                    found = true;
                }
            }
        }
        return r;
    }

    void visit_Function(const AST::Function_t &x) {
        // Extract local (including dummy) variables first
        current_symbol = (int64_t) ASR::symbolType::Function;
        ASR::accessType s_access = dflt_access;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            current_procedure_args.push_back(to_lower(arg));
        }

        // Determine the ABI (Source or BindC for now)
        current_procedure_abi_type = ASR::abiType::Source;
        char *bindc_name=nullptr;
        extract_bind(x, current_procedure_abi_type, bindc_name);

        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }
        for (size_t i=0; i<x.n_contains; i++) {
            visit_program_unit(*x.m_contains[i]);
        }
        // Convert and check arguments
        Vec<ASR::expr_t*> args;
        args.reserve(al, x.n_args);
        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            std::string arg_s = to_lower(arg);
            if (current_scope->get_symbol(arg_s) == nullptr) {
                if (compiler_options.implicit_typing) {
                    declare_implicit_variable(x.base.base.loc, arg_s,
                        ASRUtils::intent_unspecified);
                } else {
                    throw SemanticError("Dummy argument '" + arg_s + "' not defined", x.base.base.loc);
                }
            }
            ASR::symbol_t *var = current_scope->get_symbol(arg_s);
            args.push_back(al, LFortran::ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                var)));
        }

        // Handle the return variable and type
        // First determine the name of the variable: either the function name
        // or result(...)
        std::string return_var_name;
        if (x.m_return_var) {
            if (x.m_return_var->type == AST::exprType::Name) {
                return_var_name = to_lower(((AST::Name_t*)(x.m_return_var))->m_id);
            } else {
                throw SemanticError("Return variable must be an identifier",
                    x.m_return_var->base.loc);
            }
        } else {
            return_var_name = to_lower(x.m_name);
        }

        // Determine the type of the variable, the type is either specified as
        //     integer function f()
        // or in local variables as
        //     integer :: f
        ASR::asr_t *return_var;
        AST::AttrType_t *return_type = find_return_type(x.m_attributes,
            x.n_attributes, x.base.base.loc);
        if (current_scope->get_symbol(return_var_name) == nullptr) {
            // The variable is not defined among local variables, extract the
            // type from "integer function f()" and add the variable.
            if (!return_type) {
                throw SemanticError("Return type not specified",
                        x.base.base.loc);
            }
            ASR::ttype_t *type;
            int a_kind = 4;
            int a_len = -10;
            if (return_type->m_kind != nullptr) {
                if (return_type->n_kind == 1) {
                    visit_expr(*return_type->m_kind->m_value);
                    ASR::expr_t* kind_expr = LFortran::ASRUtils::EXPR(tmp);
                    if (return_type->m_type == AST::decl_typeType::TypeCharacter) {
                        a_len = ASRUtils::extract_len<SemanticError>(kind_expr, x.base.base.loc);
                    } else {
                        a_kind = ASRUtils::extract_kind<SemanticError>(kind_expr, x.base.base.loc);
                    }
                } else {
                    throw SemanticError("Only one kind item supported for now", x.base.base.loc);
                }
            }
            switch (return_type->m_type) {
                case (AST::decl_typeType::TypeInteger) : {
                    type = LFortran::ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, a_kind, nullptr, 0));
                    break;
                }
                case (AST::decl_typeType::TypeReal) : {
                    type = LFortran::ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, a_kind, nullptr, 0));
                    break;
                }
                case (AST::decl_typeType::TypeDoublePrecision) : {
                    type = LFortran::ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 8, nullptr, 0));
                    break;
                }
                case (AST::decl_typeType::TypeComplex) : {
                    type = LFortran::ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc, a_kind, nullptr, 0));
                    break;
                }
                case (AST::decl_typeType::TypeLogical) : {
                    type = LFortran::ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));
                    break;
                }
                case (AST::decl_typeType::TypeCharacter) : {
                    type = LFortran::ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc, 1, a_len, nullptr, nullptr, 0));
                    break;
                }
                case (AST::decl_typeType::TypeType) : {
                    LFORTRAN_ASSERT(return_type->m_name);
                    std::string derived_type_name = to_lower(return_type->m_name);
                    ASR::symbol_t *v = current_scope->resolve_symbol(derived_type_name);
                    if (!v) {
                        throw SemanticError("Derived type '"
                            + derived_type_name + "' not declared", x.base.base.loc);

                    }
                    type = LFortran::ASRUtils::TYPE(ASR::make_Derived_t(al, x.base.base.loc, v,
                        nullptr, 0));
                    break;
                }
                default :
                    throw SemanticError("Return type not supported",
                            x.base.base.loc);
            }
            // Add it as a local variable:
            return_var = ASR::make_Variable_t(al, x.base.base.loc,
                current_scope, s2c(al, return_var_name), LFortran::ASRUtils::intent_return_var, nullptr, nullptr,
                ASR::storage_typeType::Default, type,
                current_procedure_abi_type, ASR::Public, ASR::presenceType::Required,
                false);
            current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
        } else {
            if (return_type) {
                throw SemanticError("Cannot specify the return type twice",
                    x.base.base.loc);
            }
            // Extract the variable from the local scope
            return_var = (ASR::asr_t*) current_scope->get_symbol(return_var_name);
            ASR::down_cast2<ASR::Variable_t>(return_var)->m_intent = LFortran::ASRUtils::intent_return_var;
        }

        ASR::asr_t *return_var_ref = ASR::make_Var_t(al, x.base.base.loc,
            ASR::down_cast<ASR::symbol_t>(return_var));

        // Create and register the function
        std::string sym_name = to_lower(x.m_name);
        if (assgnd_access.count(sym_name)) {
            s_access = assgnd_access[sym_name];
        }

        if (is_interface) {
            deftype = ASR::deftypeType::Interface;
        }

        if( generic_procedures.find(sym_name) != generic_procedures.end() ) {
            parent_scope->erase_symbol(sym_name);
            sym_name = sym_name + "~genericprocedure";
        }

        if (parent_scope->get_symbol(sym_name) != nullptr) {
            ASR::symbol_t *f1 = parent_scope->get_symbol(sym_name);
            ASR::Function_t *f2 = nullptr;
            if( f1->type == ASR::symbolType::Function ) {
                f2 = ASR::down_cast<ASR::Function_t>(f1);
            }
            if ((f1->type == ASR::symbolType::ExternalSymbol && in_submodule) ||
                f2->m_abi == ASR::abiType::Interactive) {
                // Previous declaration will be shadowed
                parent_scope->erase_symbol(sym_name);
            } else {
                throw SemanticError("Function already defined", tmp->loc);
            }
        }

        bool is_elemental = false;
        for(size_t i = 0; i < x.n_attributes && !is_elemental; i++) {
            AST::decl_attribute_t* func_attr = x.m_attributes[i];
            if( AST::is_a<AST::SimpleAttribute_t>(*func_attr) ) {
                AST::SimpleAttribute_t* simple_func_attr = AST::down_cast<AST::SimpleAttribute_t>(func_attr);
                is_elemental = is_elemental || simple_func_attr->m_attr == AST::simple_attributeType::AttrElemental;
            }
        }

        tmp = ASR::make_Function_t(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(sym_name)),
            /* a_args */ args.p,
            /* n_args */ args.size(),
            nullptr, 0,
            /* a_body */ nullptr,
            /* n_body */ 0,
            /* a_return_var */ LFortran::ASRUtils::EXPR(return_var_ref),
            current_procedure_abi_type, s_access, deftype,
            bindc_name, is_elemental, false, false);
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        current_procedure_args.clear();
        current_procedure_abi_type = ASR::abiType::Source;
        current_symbol = -1;
    }

    void visit_Declaration(const AST::Declaration_t& x) {
        visit_DeclarationUtil(x);
    }

    void visit_DerivedType(const AST::DerivedType_t &x) {
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        data_member_names.reserve(al, 0);
        is_derived_type = true;
        dt_name = to_lower(x.m_name);
        AST::AttrExtends_t *attr_extend = nullptr;
        for( size_t i = 0; i < x.n_attrtype; i++ ) {
            switch( x.m_attrtype[i]->type ) {
                case AST::decl_attributeType::AttrExtends: {
                    if( attr_extend != nullptr ) {
                        throw SemanticError("DerivedType can only extend one another DerivedType",
                                            x.base.base.loc);
                    }
                    attr_extend = (AST::AttrExtends_t*)(&(x.m_attrtype[i]->base));
                    break;
                }
                default:
                    break;
            }
        }
        for (size_t i=0; i<x.n_items; i++) {
            this->visit_unit_decl2(*x.m_items[i]);
        }
        for (size_t i=0; i<x.n_contains; i++) {
            visit_procedure_decl(*x.m_contains[i]);
        }
        std::string sym_name = to_lower(x.m_name);
        if (current_scope->get_symbol(sym_name) != nullptr) {
            throw SemanticError("DerivedType already defined", x.base.base.loc);
        }
        ASR::symbol_t* parent_sym = nullptr;
        if( attr_extend != nullptr ) {
            std::string parent_sym_name = to_lower(attr_extend->m_name);
            if( parent_scope->get_symbol(parent_sym_name) == nullptr ) {
                throw SemanticError(parent_sym_name + " is not defined.", x.base.base.loc);
            }
            parent_sym = parent_scope->get_symbol(parent_sym_name);
        }
        tmp = ASR::make_DerivedType_t(al, x.base.base.loc, current_scope,
                s2c(al, to_lower(x.m_name)), data_member_names.p, data_member_names.size(),
                ASR::abiType::Source, dflt_access, parent_sym);
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        is_derived_type = false;
    }

    void visit_InterfaceProc(const AST::InterfaceProc_t &x) {
        is_interface = true;
        visit_program_unit(*x.m_proc);
        is_interface = false;
        return;
    }

    void visit_DerivedTypeProc(const AST::DerivedTypeProc_t &x) {
        for (size_t i = 0; i < x.n_symbols; i++) {
            AST::UseSymbol_t *use_sym = AST::down_cast<AST::UseSymbol_t>(
                x.m_symbols[i]);
            std::string remote_sym_str = "";
            if( x.m_name ) {
                remote_sym_str = to_lower(x.m_name);
            } else {
                remote_sym_str = to_lower(use_sym->m_remote_sym);
            }
            if (use_sym->m_local_rename) {
                class_procedures[dt_name][to_lower(use_sym->m_local_rename)] = remote_sym_str;
            } else {
                class_procedures[dt_name][to_lower(use_sym->m_remote_sym)] = remote_sym_str;
            }
        }
    }

    void fill_interface_proc_names(const AST::Interface_t& x,
                                    std::vector<std::string>& proc_names) {
        for (size_t i = 0; i < x.n_items; i++) {
            AST::interface_item_t *item = x.m_items[i];
            if (AST::is_a<AST::InterfaceModuleProcedure_t>(*item)) {
                AST::InterfaceModuleProcedure_t *proc
                    = AST::down_cast<AST::InterfaceModuleProcedure_t>(item);
                for (size_t i = 0; i < proc->n_names; i++) {
                    /* Check signatures of procedures
                    * to ensure there are no two procedures
                    * with same signatures.
                    */
                    char *proc_name = proc->m_names[i];
                    proc_names.push_back(std::string(proc_name));
                }
            } else if(AST::is_a<AST::InterfaceProc_t>(*item)) {
                visit_interface_item(*item);
                AST::InterfaceProc_t *proc
                    = AST::down_cast<AST::InterfaceProc_t>(item);
                switch(proc->m_proc->type) {
                    case AST::program_unitType::Subroutine: {
                        AST::Subroutine_t* subrout = AST::down_cast<AST::Subroutine_t>(proc->m_proc);
                        char* proc_name = subrout->m_name;
                        proc_names.push_back(std::string(proc_name));
                        break;
                    }
                    case AST::program_unitType::Function: {
                        AST::Function_t* subrout = AST::down_cast<AST::Function_t>(proc->m_proc);
                        char* proc_name = subrout->m_name;
                        proc_names.push_back(std::string(proc_name));
                        break;
                    }
                    default: {
                        LFORTRAN_ASSERT(false);
                        break;
                    }
                }
            } else {
                throw SemanticError("Interface procedure type not imlemented yet", item->base.loc);
            }
        }
    }

    void visit_Interface(const AST::Interface_t &x) {
        if (AST::is_a<AST::InterfaceHeaderName_t>(*x.m_header)) {
            std::string generic_name = to_lower(AST::down_cast<AST::InterfaceHeaderName_t>(x.m_header)->m_name);
            interface_name = generic_name;
            std::vector<std::string> proc_names;
            fill_interface_proc_names(x, proc_names);
            generic_procedures[std::string(generic_name)] = proc_names;
            interface_name.clear();
        } else if (AST::is_a<AST::InterfaceHeader_t>(*x.m_header) ||
                   AST::is_a<AST::AbstractInterfaceHeader_t>(*x.m_header)) {
            std::vector<std::string> proc_names;
            for (size_t i = 0; i < x.n_items; i++) {
                visit_interface_item(*x.m_items[i]);
            }
        } else if (AST::is_a<AST::InterfaceHeaderOperator_t>(*x.m_header)) {
            AST::intrinsicopType opType = AST::down_cast<AST::InterfaceHeaderOperator_t>(x.m_header)->m_op;
            std::vector<std::string> proc_names;
            fill_interface_proc_names(x, proc_names);
            overloaded_op_procs[opType] = proc_names;
        } else if (AST::is_a<AST::InterfaceHeaderDefinedOperator_t>(*x.m_header)) {
            std::string op_name = to_lower(AST::down_cast<AST::InterfaceHeaderDefinedOperator_t>(x.m_header)->m_operator_name);
            std::vector<std::string> proc_names;
            fill_interface_proc_names(x, proc_names);
            defined_op_procs[op_name] = proc_names;
        }  else if (AST::is_a<AST::InterfaceHeaderAssignment_t>(*x.m_header)) {
            fill_interface_proc_names(x, assgn_proc_names);
        }  else if (AST::is_a<AST::InterfaceHeaderWrite_t>(*x.m_header)) {
            std::string op_name = to_lower(AST::down_cast<AST::InterfaceHeaderWrite_t>(x.m_header)->m_id);
            std::vector<std::string> proc_names;
            fill_interface_proc_names(x, proc_names);
            defined_op_procs[op_name] = proc_names;
        }  else if (AST::is_a<AST::InterfaceHeaderRead_t>(*x.m_header)) {
            std::string op_name = to_lower(AST::down_cast<AST::InterfaceHeaderRead_t>(x.m_header)->m_id);
            std::vector<std::string> proc_names;
            fill_interface_proc_names(x, proc_names);
            defined_op_procs[op_name] = proc_names;
        }  else {
            throw SemanticError("Interface type not imlemented yet", x.base.base.loc);
        }
    }

    void add_overloaded_procedures() {
        for (auto &proc : overloaded_op_procs) {
            // FIXME LOCATION (we need to pass Location in, not initialize it
            // here)
            Location loc;
            loc.first = 1;
            loc.last = 1;
            Str s;
            s.from_str_view(intrinsic2str[proc.first]);
            char *generic_name = s.c_str(al);
            Vec<ASR::symbol_t*> symbols;
            symbols.reserve(al, proc.second.size());
            for (auto &pname : proc.second) {
                ASR::symbol_t *x;
                Str s;
                s.from_str_view(pname);
                char *name = s.c_str(al);
                x = resolve_symbol(loc, name);
                symbols.push_back(al, x);
            }
            ASR::asr_t *v = ASR::make_CustomOperator_t(al, loc, current_scope,
                                generic_name, symbols.p, symbols.size(), ASR::Public);
            current_scope->add_symbol(intrinsic2str[proc.first], ASR::down_cast<ASR::symbol_t>(v));
        }
        overloaded_op_procs.clear();

        for (auto &proc : defined_op_procs) {
            // FIXME LOCATION
            Location loc;
            loc.first = 1;
            loc.last = 1;
            Str s;
            s.from_str_view(proc.first);
            char *generic_name = s.c_str(al);
            Vec<ASR::symbol_t*> symbols;
            symbols.reserve(al, proc.second.size());
            for (auto &pname : proc.second) {
                ASR::symbol_t *x;
                Str s;
                s.from_str_view(pname);
                char *name = s.c_str(al);
                x = resolve_symbol(loc, name);
                symbols.push_back(al, x);
            }
            ASR::asr_t *v = ASR::make_CustomOperator_t(al, loc, current_scope,
                                generic_name, symbols.p, symbols.size(), ASR::Public);
            current_scope->add_symbol(proc.first, ASR::down_cast<ASR::symbol_t>(v));
        }
        defined_op_procs.clear();
    }

    void add_assignment_procedures() {
        if( assgn_proc_names.empty() ) {
            return ;
        }
        Location loc;
        loc.first = 1;
        loc.last = 1;
        std::string str_name = "~assign";
        Str s;
        s.from_str_view(str_name);
        char *generic_name = s.c_str(al);
        Vec<ASR::symbol_t*> symbols;
        symbols.reserve(al, assgn_proc_names.size());
        for (auto &pname : assgn_proc_names) {
            ASR::symbol_t *x;
            Str s;
            s.from_str_view(pname);
            char *name = s.c_str(al);
            x = resolve_symbol(loc, name);
            symbols.push_back(al, x);
        }
        ASR::asr_t *v = ASR::make_CustomOperator_t(al, loc, current_scope,
                            generic_name, symbols.p, symbols.size(), assgn[current_scope]);
        current_scope->add_symbol(str_name, ASR::down_cast<ASR::symbol_t>(v));
    }

    void add_generic_procedures() {
        for (auto &proc : generic_procedures) {
            // FIXME LOCATION
            Location loc;
            loc.first = 1;
            loc.last = 1;
            Vec<ASR::symbol_t*> symbols;
            symbols.reserve(al, proc.second.size());
            for (auto &pname : proc.second) {
                std::string correct_pname = pname;
                if( pname == proc.first ) {
                    correct_pname = pname + "~genericprocedure";
                }
                ASR::symbol_t *x;
                Str s;
                s.from_str_view(correct_pname);
                char *name = s.c_str(al);
                x = resolve_symbol(loc, name);
                symbols.push_back(al, x);
            }
            std::string sym_name_str = proc.first;
            if( current_scope->get_symbol(proc.first) != nullptr ) {
                ASR::symbol_t* der_type_name = current_scope->get_symbol(proc.first);
                if( der_type_name->type == ASR::symbolType::DerivedType ||
                    der_type_name->type == ASR::symbolType::Function ) {
                    sym_name_str = "~" + proc.first;
                }
            }
            Str s;
            s.from_str_view(sym_name_str);
            char *generic_name = s.c_str(al);
            ASR::asr_t *v = ASR::make_GenericProcedure_t(al, loc,
                current_scope,
                generic_name, symbols.p, symbols.size(), ASR::Public);
            current_scope->add_symbol(sym_name_str, ASR::down_cast<ASR::symbol_t>(v));
        }
    }

    void add_generic_class_procedures() {
        for (auto &proc : generic_class_procedures) {
            Location loc;
            loc.first = 1;
            loc.last = 1;
            ASR::DerivedType_t *clss = ASR::down_cast<ASR::DerivedType_t>(
                                            current_scope->get_symbol(proc.first));
            for (auto &pname : proc.second) {
                Vec<ASR::symbol_t*> cand_procs;
                cand_procs.reserve(al, pname.second.size());
                for( std::string &cand_proc: pname.second ) {
                    if( clss->m_symtab->get_symbol(cand_proc) != nullptr ) {
                        cand_procs.push_back(al, clss->m_symtab->get_symbol(cand_proc));
                    } else {
                        throw SemanticError(cand_proc + " doesn't exist inside " + proc.first + " type", loc);
                    }
                }
                Str s;
                s.from_str_view(pname.first);
                char *generic_name = s.c_str(al);
                ASR::asr_t *v = ASR::make_GenericProcedure_t(al, loc,
                    clss->m_symtab, generic_name, cand_procs.p, cand_procs.size(),
                    ASR::accessType::Public); // Update the access as per the input Fortran code
                ASR::symbol_t *cls_proc_sym = ASR::down_cast<ASR::symbol_t>(v);
                clss->m_symtab->add_symbol(pname.first, cls_proc_sym);
            }
        }
    }

    void add_class_procedures() {
        for (auto &proc : class_procedures) {
            // FIXME LOCATION
            Location loc;
            loc.first = 1;
            loc.last = 1;
            ASR::DerivedType_t *clss = ASR::down_cast<ASR::DerivedType_t>(
                current_scope->get_symbol(proc.first));
            for (auto &pname : proc.second) {
                ASR::symbol_t *proc_sym = current_scope->get_symbol(pname.second);
                Str s;
                s.from_str_view(pname.first);
                char *name = s.c_str(al);
                s.from_str_view(pname.second);
                char *proc_name = s.c_str(al);
                ASR::asr_t *v = ASR::make_ClassProcedure_t(al, loc,
                    clss->m_symtab, name, proc_name, proc_sym,
                    ASR::abiType::Source);
                ASR::symbol_t *cls_proc_sym = ASR::down_cast<ASR::symbol_t>(v);
                clss->m_symtab->add_symbol(pname.first, cls_proc_sym);
            }
        }
    }

    std::string import_all(const ASR::Module_t* m) {
        // Import all symbols from the module, e.g.:
        //     use a
        for (auto &item : m->m_symtab->get_scope()) {
            if( current_scope->get_symbol(item.first) != nullptr &&
                !in_submodule ) {
                continue;
            }
            // TODO: only import "public" symbols from the module
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *mfn = ASR::down_cast<ASR::Function_t>(item.second);
                ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                    al, mfn->base.base.loc,
                    /* a_symtab */ current_scope,
                    /* a_name */ mfn->m_name,
                    (ASR::symbol_t*)mfn,
                    m->m_name, nullptr, 0, mfn->m_name,
                    dflt_access
                    );
                std::string sym = to_lower(mfn->m_name);
                current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(fn));
            } else if (ASR::is_a<ASR::GenericProcedure_t>(*item.second)) {
                ASR::GenericProcedure_t *gp = ASR::down_cast<
                    ASR::GenericProcedure_t>(item.second);
                ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                    al, gp->base.base.loc,
                    current_scope,
                    /* a_name */ gp->m_name,
                    (ASR::symbol_t*)gp,
                    m->m_name, nullptr, 0, gp->m_name,
                    dflt_access
                    );
                std::string sym = to_lower(gp->m_name);
                current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(ep));
            }  else if (ASR::is_a<ASR::CustomOperator_t>(*item.second)) {
                ASR::CustomOperator_t *gp = ASR::down_cast<
                    ASR::CustomOperator_t>(item.second);
                ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                    al, gp->base.base.loc,
                    current_scope,
                    /* a_name */ gp->m_name,
                    (ASR::symbol_t*)gp,
                    m->m_name, nullptr, 0, gp->m_name,
                    dflt_access
                    );
                std::string sym = to_lower(gp->m_name);
                current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(ep));
            } else if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *mvar = ASR::down_cast<ASR::Variable_t>(item.second);
                ASR::asr_t *var = ASR::make_ExternalSymbol_t(
                    al, mvar->base.base.loc,
                    /* a_symtab */ current_scope,
                    /* a_name */ mvar->m_name,
                    (ASR::symbol_t*)mvar,
                    m->m_name, nullptr, 0, mvar->m_name,
                    dflt_access
                    );
                std::string sym = to_lower(mvar->m_name);
                current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(var));
            } else if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                // We have to "repack" the ExternalSymbol so that it lives in the
                // local symbol table
                ASR::ExternalSymbol_t *es0 = ASR::down_cast<ASR::ExternalSymbol_t>(item.second);
                std::string sym;
                if( in_submodule ) {
                    sym = item.first;
                } else {
                    sym = to_lower(es0->m_original_name);
                }
                ASR::asr_t *es = ASR::make_ExternalSymbol_t(
                    al, es0->base.base.loc,
                    /* a_symtab */ current_scope,
                    /* a_name */ s2c(al, sym),
                    es0->m_external,
                    es0->m_module_name, nullptr, 0,
                    es0->m_original_name,
                    dflt_access
                    );
                current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(es));
            } else if( ASR::is_a<ASR::DerivedType_t>(*item.second) ) {
                ASR::DerivedType_t *mv = ASR::down_cast<ASR::DerivedType_t>(item.second);
                // `mv` is the Variable in a module. Now we construct
                // an ExternalSymbol that points to it.
                Str name;
                name.from_str(al, item.first);
                char *cname = name.c_str(al);
                ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                    al, mv->base.base.loc,
                    /* a_symtab */ current_scope,
                    /* a_name */ cname,
                    (ASR::symbol_t*)mv,
                    m->m_name, nullptr, 0, mv->m_name,
                    dflt_access
                    );
                current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
            } else {
                return item.first;
            }
        }
        return "";
    }

    void import_symbols_util(ASR::Module_t *m, std::string& msym,
                             std::string& remote_sym, std::string& local_sym,
                             std::queue<std::pair<std::string, std::string>>& to_be_imported_later,
                             const Location& loc) {
        ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
        if (!t) {
            throw SemanticError("The symbol '" + remote_sym + "' not found in the module '" + msym + "'",
                loc);
        }
        if (ASR::is_a<ASR::Function_t>(*t) &&
            ASR::down_cast<ASR::Function_t>(t)->m_return_var == nullptr) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Subroutine already defined",
                    loc);
            }
            ASR::Function_t *msub = ASR::down_cast<ASR::Function_t>(t);
            // `msub` is the Subroutine in a module. Now we construct
            // an ExternalSymbol that points to
            // `msub` via the `external` field.
            Str name;
            name.from_str(al, local_sym);
            ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
                al, msub->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ name.c_str(al),
                (ASR::symbol_t*)msub,
                m->m_name, nullptr, 0, msub->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(sub));
        } else if (ASR::is_a<ASR::GenericProcedure_t>(*t)) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Symbol already defined",
                    loc);
            }
            ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(t);
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                al, t->base.loc,
                current_scope,
                /* a_name */ cname,
                t,
                m->m_name, nullptr, 0, gp->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
        } else if (ASR::is_a<ASR::CustomOperator_t>(*t)) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Symbol already defined",
                    loc);
            }
            ASR::CustomOperator_t *gp = ASR::down_cast<ASR::CustomOperator_t>(t);
            std::string gp_name = std::string(gp->m_name);
            for (size_t igp = 0; igp < gp->n_procs; igp++) {
                std::string proc_name = ASRUtils::symbol_name(gp->m_procs[igp]);
                std::string mangled_name = proc_name + "@" + gp_name;
                to_be_imported_later.push(std::make_pair(proc_name, mangled_name));
            }
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                al, t->base.loc,
                current_scope,
                /* a_name */ cname,
                t,
                m->m_name, nullptr, 0, gp->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
        } else if (ASR::is_a<ASR::ExternalSymbol_t>(*t)) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Symbol already defined", loc);
            }
            // Repack ExternalSymbol to point directly to the original symbol
            ASR::ExternalSymbol_t *es = ASR::down_cast<ASR::ExternalSymbol_t>(t);
            ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                al, es->base.base.loc,
                current_scope,
                /* a_name */ es->m_name,
                es->m_external,
                es->m_module_name, es->m_scope_names, es->n_scope_names, es->m_original_name,
                es->m_access
                );
            ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(t);
            if( ASR::is_a<ASR::CustomOperator_t>(*orig_sym) ) {
                ASR::CustomOperator_t *gp = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
                std::string gp_name = std::string(gp->m_name);
                for (size_t igp = 0; igp < gp->n_procs; igp++) {
                    std::string proc_name = ASRUtils::symbol_name(gp->m_procs[igp]);
                    std::string mangled_name = proc_name + "@" + gp_name;
                    ASR::symbol_t* proc_sym = m->m_symtab->resolve_symbol(proc_name);
                    ASR::symbol_t* mangled_sym = m->m_symtab->resolve_symbol(mangled_name);
                    std::string proc_remote_sym = "";
                    if( proc_sym ) {
                        proc_remote_sym = proc_name;
                    } else if( mangled_sym ) {
                        proc_remote_sym = mangled_name;
                    } else {
                        // Should never happen because if the user
                        // doesn't import a procedure for a custom operator
                        // the lfortran is supposed to do that with help
                        // of to_be_imported_later queue.
                        LFORTRAN_ASSERT(false);
                    }
                    to_be_imported_later.push(std::make_pair(proc_remote_sym, mangled_name));
                }
            }
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
        } else if (ASR::is_a<ASR::Function_t>(*t)) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Function already defined", loc);
            }
            ASR::Function_t *mfn = ASR::down_cast<ASR::Function_t>(t);
            // `mfn` is the Function in a module. Now we construct
            // an ExternalSymbol that points to it.
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                al, mfn->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)mfn,
                m->m_name, nullptr, 0, mfn->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(fn));
        } else if (ASR::is_a<ASR::Variable_t>(*t)) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Variable already defined", loc);
            }
            ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(t);
            // `mv` is the Variable in a module. Now we construct
            // an ExternalSymbol that points to it.
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, mv->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)mv,
                m->m_name, nullptr, 0, mv->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(v));
        } else if( ASR::is_a<ASR::DerivedType_t>(*t) ) {
            if (current_scope->get_symbol(local_sym) != nullptr) {
                throw SemanticError("Derived type already defined", loc);
            }
            ASR::DerivedType_t *mv = ASR::down_cast<ASR::DerivedType_t>(t);
            // `mv` is the Variable in a module. Now we construct
            // an ExternalSymbol that points to it.
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, mv->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)mv,
                m->m_name, nullptr, 0, mv->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(v));
        } else {
            throw LCompilersException("Only Subroutines, Functions, Variables and Derived supported in 'use'");
        }
    }

    void visit_Use(const AST::Use_t &x) {
        std::string msym = to_lower(x.m_module);
        Str msym_c; msym_c.from_str_view(msym);
        char *msym_cc = msym_c.c_str(al);
        if (!present(current_module_dependencies, msym_cc)) {
            current_module_dependencies.push_back(al, msym_cc);
        }
        ASR::symbol_t *t = current_scope->parent->resolve_symbol(msym);
        if (!t) {
            std::string rl_path = get_runtime_library_dir();
            t = (ASR::symbol_t*)(ASRUtils::load_module(al, current_scope->parent,
                msym, x.base.base.loc, false, rl_path, true,
                [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }
                ));
        }
        if (!ASR::is_a<ASR::Module_t>(*t)) {
            throw SemanticError("The symbol '" + msym + "' must be a module",
                x.base.base.loc);
        }
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(t);
        if (x.n_symbols == 0) {
            std::string unsupported_sym_name = import_all(m);
            if( !unsupported_sym_name.empty() ) {
                throw LCompilersException("'" + unsupported_sym_name + "' is not supported yet for declaring with use.");
            }
        } else {
            // Only import individual symbols from the module, e.g.:
            //     use a, only: x, y, z
            std::queue<std::pair<std::string, std::string>> to_be_imported_later;
            for (size_t i = 0; i < x.n_symbols; i++) {
                std::string remote_sym;
                switch (x.m_symbols[i]->type)
                {
                    case AST::use_symbolType::UseSymbol: {
                        remote_sym = to_lower(AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_remote_sym);
                        break;
                    }
                    case AST::use_symbolType::UseAssignment: {
                        remote_sym = "~assign";
                        break;
                    }
                    case AST::use_symbolType::IntrinsicOperator: {
                        AST::intrinsicopType op_type = AST::down_cast<AST::IntrinsicOperator_t>(x.m_symbols[i])->m_op;
                        remote_sym = intrinsic2str[op_type];
                        break;
                    }
                    default:
                        throw SemanticError("Symbol with use not supported yet", x.base.base.loc);
                }
                std::string local_sym;
                if (AST::is_a<AST::UseSymbol_t>(*x.m_symbols[i]) &&
                    AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_local_rename) {
                    local_sym = to_lower(AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_local_rename);
                } else {
                    local_sym = remote_sym;
                }
                import_symbols_util(m, msym, remote_sym, local_sym,
                                    to_be_imported_later, x.base.base.loc);
            }

            // Importing procedures defined for overloaded operators like assignment
            // after all the user imports are complete. This avoids
            // importing the same function twice i.e., if the user has already imported
            // the required procedures manually then importing later avoids polluting the
            // symbol table.
            while( !to_be_imported_later.empty() ) {
                std::string remote_sym = to_be_imported_later.front().first;
                std::string local_sym = to_be_imported_later.front().second;
                to_be_imported_later.pop();
                if( current_scope->resolve_symbol(local_sym) == nullptr ) {
                    import_symbols_util(m, msym, remote_sym, local_sym,
                                        to_be_imported_later, x.base.base.loc);
                }
            }
        }
    }

    void visit_GenericName(const AST::GenericName_t& x) {
        std::string generic_name = to_lower(std::string(x.m_name));
        for( size_t i = 0; i < x.n_names; i++ ) {
            std::string x_m_name = std::string(x.m_names[i]);
            generic_class_procedures[dt_name][generic_name].push_back(to_lower(x_m_name));
        }
    }

};

Result<ASR::asr_t*> symbol_table_visitor(Allocator &al, AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table, CompilerOptions &compiler_options)
{
    SymbolTableVisitor v(al, symbol_table, diagnostics, compiler_options);
    try {
        v.visit_TranslationUnit(ast);
    } catch (const SemanticError &e) {
        Error error;
        diagnostics.diagnostics.push_back(e.d);
        return error;
    } catch (const SemanticAbort &) {
        Error error;
        return error;
    }
    ASR::asr_t *unit = v.tmp;
    return unit;
}

} // namespace LFortran
