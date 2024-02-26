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
#include <libasr/utils.h>
#include <libasr/pass/instantiate_template.h>

namespace LCompilers::LFortran {

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
                } else if (to_lower(std::string(name->m_id)) == "js") {
                    abi_type=ASR::abiType::BindJS;
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
    struct ClassProcInfo {
        std::string name;
        Location loc;
    };
    SymbolTable *global_scope;
    std::map<std::string, std::vector<std::string>> generic_procedures;
    std::map<std::string, std::map<std::string, std::vector<std::string>>> generic_class_procedures;
    std::map<AST::intrinsicopType, std::vector<std::string>> overloaded_op_procs;
    std::map<std::string, std::vector<std::string>> defined_op_procs;
    std::map<std::string, std::map<std::string, std::map<std::string, ClassProcInfo>>> class_procedures;
    std::map<std::string, std::map<std::string, std::map<std::string, Location>>> class_deferred_procedures;
    std::vector<std::string> assgn_proc_names;
    std::vector<std::pair<std::string,Location>> simd_variables;
    std::map<std::string, std::vector<AST::arg_t>> entry_function_args;
    std::string dt_name;
    bool in_submodule = false;
    bool is_interface = false;
    std::string interface_name = "";
    ASR::symbol_t *current_module_sym;

    ASR::ttype_t *tmp_type;

    SymbolTableVisitor(Allocator &al, SymbolTable *symbol_table,
        diag::Diagnostics &diagnostics, CompilerOptions &compiler_options, std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash, std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint32_t, std::map<std::string, ASR::ttype_t*>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::vector<ASR::stmt_t*> &data_structure)
      : CommonVisitor(al, symbol_table, diagnostics, compiler_options, implicit_mapping, common_variables_hash, external_procedures_mapping,
                      instantiate_types, instantiate_symbols, entry_functions, entry_function_arguments_mapping, data_structure) {}

    void visit_TranslationUnit(const AST::TranslationUnit_t &x) {
        if (!current_scope) {
            current_scope = al.make_new<SymbolTable>(nullptr);
        }
        LCOMPILERS_ASSERT(current_scope != nullptr);
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
        if (pre_declared_array_dims.size() > 0) {
            std::string sym_name = "";
            for (auto &it: pre_declared_array_dims) {
                if (it.second == 2) continue;
                if (sym_name.empty()) {
                     sym_name += it.first;
                } else {
                     sym_name += ", " + it.first;
                }
            }
            if (!sym_name.empty()) {
                throw SemanticError(sym_name + " is/are used as dimensions but not declared", x.base.base.loc);
            }
        }
    }

    void visit_Private(const AST::Private_t&) {
        // To Be Implemented
    }

    void visit_FinalName(const AST::FinalName_t&) {
        // To Be Implemented
    }

    template <typename T>
    void fix_type_info(T* x) {
        current_module_dependencies.clear(al);
        std::map<ASR::asr_t*, SetChar> node2deps;
        for( TypeMissingData* data: type_info ) {
            if( data->sym_type == -1 ) {
                continue;
            }
            ASR::expr_t* expr = nullptr;
            if( data->sym_type == (int64_t) ASR::symbolType::Function ) {
                SymbolTable* current_scope_copy = current_scope;
                current_scope = data->scope;
                current_function_dependencies.clear(al);
                visit_expr(*data->expr);
                for( size_t i = 0; i < current_function_dependencies.size(); i++ ) {
                    char* itr = current_function_dependencies[i];
                    node2deps[current_scope->asr_owner].push_back(al, itr);
                }
                expr = ASRUtils::EXPR(tmp);
                current_scope = current_scope_copy;
            }
            if( expr ) {
                switch( data->type->type ) {
                    case ASR::ttypeType::Character: {
                        ASR::Character_t* char_type = ASR::down_cast<ASR::Character_t>(data->type);
                        char_type->m_len_expr = expr;
                        char_type->m_len = -3;
                        if( expr->type == ASR::exprType::FunctionCall ) {
                            ASR::FunctionCall_t *call = ASR::down_cast<ASR::FunctionCall_t>(expr);
                            for(size_t i = 0; i < call->n_args; i ++) {
                                if (ASR::is_a<ASR::Var_t>(*call->m_args[i].m_value)) {
                                    ASR::Variable_t *v = ASRUtils::EXPR2VAR(call->m_args[i].m_value);
                                    if (v->m_storage == ASR::storage_typeType::Parameter) {
                                        call->m_args[i].m_value = v->m_value;
                                    }
                                }
                            }
                            if( data->sym_type == (int64_t) ASR::symbolType::Function ) {
                                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
                                    ASR::down_cast<ASR::symbol_t>(data->scope->asr_owner));
                                ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(*func);
                                if( func_type->m_return_var_type ) {
                                    ASRUtils::ReplaceWithFunctionParamVisitor replacer(al, func->m_args, func->n_args);
                                    func_type->m_return_var_type = replacer.replace_args_with_FunctionParam(data->type, data->scope);
                                }
                            }
                        }
                        break;
                    }
                    default: {
                        throw SemanticError("Only Character type is supported as of now.", data->type->base.loc);
                    }
                }
            }

            ASR::symbol_t* sym = data->scope->get_symbol(data->sym_name);
            if( sym && ASR::is_a<ASR::Variable_t>(*sym) ) {
                ASR::Variable_t* sym_variable = ASR::down_cast<ASR::Variable_t>(sym);
                SetChar variable_dependencies_vec;
                variable_dependencies_vec.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, sym_variable->m_type,
                    sym_variable->m_symbolic_value, sym_variable->m_value);
                sym_variable->m_dependencies = variable_dependencies_vec.p;
                sym_variable->n_dependencies = variable_dependencies_vec.size();
            }
        }

        for( auto& itr: node2deps ) {
            if( ASR::is_a<ASR::symbol_t>(*itr.first) ) {
                ASR::symbol_t* asr_owner_sym = ASR::down_cast<ASR::symbol_t>(itr.first);
                if( ASR::is_a<ASR::Function_t>(*asr_owner_sym) ) {
                    SetChar func_deps;
                    ASR::Function_t* asr_owner_func = ASR::down_cast<ASR::Function_t>(asr_owner_sym);
                    func_deps.from_pointer_n_copy(al, asr_owner_func->m_dependencies,
                                                  asr_owner_func->n_dependencies);
                    for( size_t i = 0; i < itr.second.size(); i++ ) {
                        char* dep = itr.second[i];
                        func_deps.push_back(al, dep);
                    }
                    asr_owner_func->m_dependencies = func_deps.p;
                    asr_owner_func->n_dependencies = func_deps.size();
                }
            }
        }

        SetChar x_deps_vec;
        x_deps_vec.from_pointer_n_copy(al, x->m_dependencies, x->n_dependencies);
        for( size_t i = 0; i < current_module_dependencies.size(); i++ ) {
            x_deps_vec.push_back(al, current_module_dependencies[i]);
        }
        x->m_dependencies = x_deps_vec.p;
        x->n_dependencies = x_deps_vec.size();

        type_info.clear();
    }

    void fix_struct_type(SymbolTable* symtab) {
        for( auto& itr: symtab->get_scope() ) {
            ASR::symbol_t* sym = itr.second;
            if( !ASR::is_a<ASR::Variable_t>(*sym) &&
                !ASR::is_a<ASR::StructType_t>(*sym) ) {
                continue ;
            }

            if( ASR::is_a<ASR::StructType_t>(*sym) ) {
                fix_struct_type(ASR::down_cast<ASR::StructType_t>(sym)->m_symtab);
                continue ;
            }

            ASR::ttype_t* sym_type = ASRUtils::type_get_past_pointer(
                                        ASRUtils::symbol_type(sym));
            if( ASR::is_a<ASR::Struct_t>(*sym_type) ) {
                ASR::Struct_t* struct_t = ASR::down_cast<ASR::Struct_t>(sym_type);
                ASR::symbol_t* der_sym = struct_t->m_derived_type;
                if( ASR::is_a<ASR::ExternalSymbol_t>(*der_sym) &&
                    ASR::down_cast<ASR::ExternalSymbol_t>(der_sym)->m_external == nullptr &&
                    ASR::down_cast<ASR::ExternalSymbol_t>(der_sym)->m_module_name == nullptr ) {
                    std::string derived_type_name = ASR::down_cast<ASR::ExternalSymbol_t>(der_sym)->m_name;
                    ASR::symbol_t* sym_ = symtab->resolve_symbol(derived_type_name);
                    if( !sym_ ) {
                        throw SemanticError("Derived type '"
                                + derived_type_name + "' not declared", der_sym->base.loc);
                    }
                    struct_t->m_derived_type = sym_;
                }
            }
        }
    }


    void populate_implicit_dictionary(Location &a_loc, std::map<std::string, ASR::ttype_t*> &implicit_dictionary) {
        for (char ch='i'; ch<='n'; ch++) {
            implicit_dictionary[std::string(1, ch)] = ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, compiler_options.po.default_integer_kind));
        }

        for (char ch='o'; ch<='z'; ch++) {
            implicit_dictionary[std::string(1, ch)] = ASRUtils::TYPE(ASR::make_Real_t(al, a_loc, 4));
        }

        for (char ch='a'; ch<='h'; ch++) {
            implicit_dictionary[std::string(1, ch)] = ASRUtils::TYPE(ASR::make_Real_t(al, a_loc, 4));
        }
    }

    template <typename T>
    void process_implicit_statements(const T &x, std::map<std::string, ASR::ttype_t*> &implicit_dictionary) {
        if (implicit_stack.size() > 0 && x.n_implicit == 0) {
            // We are inside a module and visiting a function / subroutine with no implicit statement
            if (!is_interface) {
                implicit_dictionary = implicit_stack.back();
                return;
            }
        }
        //iterate over all implicit statements
        for (size_t i=0;i<x.n_implicit;i++) {
            //check if the implicit statement is of type "none"
            if (AST::is_a<AST::ImplicitNone_t>(*x.m_implicit[i])) {
                //if yes, clear the implicit dictionary i.e. set all characters to nullptr
                if (x.n_implicit != 1) {
                    throw SemanticError("No other implicit statement is allowed when 'implicit none' is used", x.m_implicit[i]->base.loc);
                }
                for (auto &it: implicit_dictionary) {
                    it.second = nullptr;
                }
            } else {
                //if no, then it is of type "implicit"
                //get the implicit statement
                AST::Implicit_t* implicit = AST::down_cast<AST::Implicit_t>(x.m_implicit[i]);
                AST::AttrType_t *attr_type = AST::down_cast<AST::AttrType_t>(implicit->m_type);
                AST::decl_typeType ast_type=attr_type->m_type;
                ASR::ttype_t *type = nullptr;
                //convert the ast_type to asr_type
                int i_kind = compiler_options.po.default_integer_kind;
                int a_kind = 4;
                int a_len = -10;
                if (attr_type->m_kind != nullptr) {
                    if (attr_type->n_kind == 1) {
                        visit_expr(*attr_type->m_kind->m_value);
                        ASR::expr_t* kind_expr = ASRUtils::EXPR(tmp);
                        if (attr_type->m_type == AST::decl_typeType::TypeCharacter) {
                            a_len = ASRUtils::extract_len<SemanticError>(kind_expr, x.base.base.loc);
                        } else {
                            a_kind = ASRUtils::extract_kind<SemanticError>(kind_expr, x.base.base.loc);
                            i_kind = a_kind;
                        }
                    } else {
                        throw SemanticError("Only one kind item supported for now", x.base.base.loc);
                    }
                }
                switch (ast_type) {
                    case (AST::decl_typeType::TypeInteger) : {
                        type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, i_kind));
                        break;
                    }
                    case (AST::decl_typeType::TypeReal) : {
                        type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, a_kind));
                        break;
                    }
                    case (AST::decl_typeType::TypeDoublePrecision) : {
                        type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 8));
                        break;
                    }
                    case (AST::decl_typeType::TypeComplex) : {
                        type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc, a_kind));
                        break;
                    }
                    case (AST::decl_typeType::TypeLogical) : {
                        type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                        break;
                    }
                    case (AST::decl_typeType::TypeCharacter) : {
                        type = ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc, 1, a_len, nullptr));
                        break;
                    }
                    default :
                        throw SemanticError("Return type not supported",
                                x.base.base.loc);
                }
                //iterate over all implicit rules
                for (size_t j=0;j<implicit->n_specs;j++) {
                    //cast x.m_specs[j] to AST::LetterSpec_t
                    AST::LetterSpec_t* letter_spec = AST::down_cast<AST::LetterSpec_t>(implicit->m_specs[j]);
                    char *start=letter_spec->m_start;
                    char *end=letter_spec->m_end;
                    if (!start) {
                        implicit_dictionary[to_lower(std::string(1, *end))] = type;
                    } else {
                        for(char ch=*start; ch<=*end; ch++){
                            implicit_dictionary[to_lower(std::string(1, ch))] = type;
                        }
                    }
                }
            }
        }
    }

    void print_implicit_dictionary(std::map<std::string, ASR::ttype_t*> &implicit_dictionary) {
        std::cout << "Implicit Dictionary: " << std::endl;
        for (auto it: implicit_dictionary) {
            if (it.second) {
                std::cout << it.first << " " << ASRUtils::type_to_str(it.second) << std::endl;
            } else {
                std::cout << it.first << " " << "NULL" << std::endl;
            }
        }
    }

    template <typename T, typename R>
    void visit_ModuleSubmoduleCommon(const T &x, std::string parent_name="") {
        assgn_proc_names.clear();
        class_procedures.clear();
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
            ASR::symbol_t* submod_parent = (ASR::symbol_t*)(ASRUtils::load_module(al, global_scope,
                                                parent_name, x.base.base.loc, false,
                                                compiler_options.po, true,
                                                [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }
                                                ));
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(submod_parent);
            std::string unsupported_sym_name = import_all(m, true);
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
            bool current_storage_save = default_storage_save;
            default_storage_save = false;
            visit_program_unit(*x.m_contains[i]);
            default_storage_save = current_storage_save;
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
        m->n_dependencies = current_module_dependencies.size();
        std::string sym_name = to_lower(x.m_name);
        if (parent_scope->get_symbol(sym_name) != nullptr) {
            throw SemanticError("Module already defined", tmp->loc);
        }
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        fix_type_info(m);
        fix_struct_type(m->m_symtab);
        dflt_access = ASR::Public;
    }

    void visit_Module(const AST::Module_t &x) {
        if (compiler_options.implicit_typing) {
            Location a_loc = x.base.base.loc;
            populate_implicit_dictionary(a_loc, implicit_dictionary);
            process_implicit_statements(x, implicit_dictionary);
            implicit_stack.push_back(implicit_dictionary);
        } else {
            for (size_t i=0;i<x.n_implicit;i++) {
                if (!AST::is_a<AST::ImplicitNone_t>(*x.m_implicit[i])) {
                    throw SemanticError("Implicit typing is not allowed, enable it by using --implicit-typing ", x.m_implicit[i]->base.loc);
                }
            }
        }
        in_module = true;
        visit_ModuleSubmoduleCommon<AST::Module_t, ASR::Module_t>(x);
        in_module = false;
        if (compiler_options.implicit_typing) {
            implicit_stack.pop_back();
        }
    }

    void visit_Submodule(const AST::Submodule_t &x) {
        in_submodule = true;
        visit_ModuleSubmoduleCommon<AST::Submodule_t, ASR::Module_t>(x, std::string(x.m_id));
        in_submodule = false;
    }

    void handle_save() {
        if (default_storage_save) {
            /*
                Iterate over all variables in the symbol table
                and set the storageType to Save
            */
            for (auto &it: current_scope->get_scope()) {
                ASR::symbol_t* sym = it.second;
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
                    var->m_storage = ASR::storage_typeType::Save;
                }
            }
            default_storage_save = false;
        }
    }

    void visit_Program(const AST::Program_t &x) {
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        current_module_dependencies.reserve(al, 4);
        if (compiler_options.implicit_typing) {
            Location a_loc = x.base.base.loc;
            populate_implicit_dictionary(a_loc, implicit_dictionary);
            process_implicit_statements(x, implicit_dictionary);
        } else {
            for (size_t i=0;i<x.n_implicit;i++) {
                if (!AST::is_a<AST::ImplicitNone_t>(*x.m_implicit[i])) {
                    throw SemanticError("Implicit typing is not allowed, enable it by using --implicit-typing ", x.m_implicit[i]->base.loc);
                }
            }
        }
        simd_variables.clear();
        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            visit_unit_decl2(*x.m_decl[i]);
        }
        process_simd_variables();
        for (size_t i=0; i<x.n_contains; i++) {
            bool current_storage_save = default_storage_save;
            default_storage_save = false;
            visit_program_unit(*x.m_contains[i]);
            default_storage_save = current_storage_save;
        }
        tmp = ASR::make_Program_t(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(x.m_name)),
            current_module_dependencies.p,
            current_module_dependencies.size(),
            /* a_body */ nullptr,
            /* n_body */ 0);
        std::string sym_name = to_lower(x.m_name);
        if (parent_scope->get_symbol(sym_name) != nullptr) {
            throw SemanticError("Program already defined", tmp->loc);
        }
        handle_save();
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;

        // print_implicit_dictionary(implicit_dictionary);
        // get hash of the function and add it to the implicit_mapping
        if (compiler_options.implicit_typing) {
            uint64_t hash = get_hash(tmp);

            implicit_mapping[hash] = implicit_dictionary;

            implicit_dictionary.clear();
        }

        // populate the external_procedures_mapping
        uint64_t hash = get_hash(tmp);
        external_procedures_mapping[hash] = external_procedures;

        fix_type_info(ASR::down_cast2<ASR::Program_t>(tmp));
        mark_common_blocks_as_declared();
    }

    bool subroutine_contains_entry_function(std::string subroutine_name, AST::stmt_t** body, size_t n_body) {
        bool contains_entry_function = false;
        for (size_t i=0; i<n_body; i++) {
            if (AST::is_a<AST::Entry_t>(*body[i])) {
                contains_entry_function = true;
                AST::Entry_t* entry = AST::down_cast<AST::Entry_t>(body[i]);
                std::string entry_name = to_lower(entry->m_name);
                entry_functions[subroutine_name][entry_name] = std::vector<AST::stmt_t*>();
                for(size_t i = 0; i < entry->n_args; i++) {
                    entry_function_args[entry_name].push_back(entry->m_args[i]);
                }
            } else {
                if (contains_entry_function) {
                    for(auto &it: entry_functions[subroutine_name]) {
                        it.second.push_back(body[i]);
                    }
                }
            }
        }
        return contains_entry_function;
    }

    void update_duplicated_nodes(Allocator &al, SymbolTable *current_scope) {
        class UpdateDuplicatedNodes : public PassUtils::PassVisitor<UpdateDuplicatedNodes>
        {
            public:
            SymbolTable* scope = current_scope;
            SymbolTable* correct_scope = nullptr;
            UpdateDuplicatedNodes(Allocator &al) : PassVisitor(al, nullptr) {}

            void visit_FunctionCall(const ASR::FunctionCall_t& x) {
                ASR::FunctionCall_t* func_call = (ASR::FunctionCall_t*)(&x);
                if (scope->counter == correct_scope->counter) {
                    std::string func_call_name = ASRUtils::symbol_name(func_call->m_name);
                    ASR::symbol_t* sym = correct_scope->resolve_symbol(func_call_name);
                    if (sym) {
                        func_call->m_name = correct_scope->resolve_symbol(func_call_name);
                    }
                    std::string func_call_origin_name = ASRUtils::symbol_name(func_call->m_original_name);
                    sym = correct_scope->resolve_symbol(func_call_origin_name);
                    if (sym) {
                        func_call->m_original_name = correct_scope->resolve_symbol(func_call_origin_name);
                    }
                }
                for (size_t i=0; i<x.n_args; i++) {
                    this->visit_call_arg(x.m_args[i]);
                }
                this->visit_ttype(*x.m_type);
                if (x.m_value) {
                    this->visit_expr(*x.m_value);
                }
                if (x.m_dt) {
                    this->visit_expr(*x.m_dt);
                }
            }

            void visit_Var(const ASR::Var_t& x) {
                if (scope && scope->counter == correct_scope->counter) {
                    ASR::Var_t* var = (ASR::Var_t*)(&x);
                    ASR::symbol_t* sym = var->m_v;
                    std::string sym_name = ASRUtils::symbol_name(sym);

                    ASR::symbol_t* sym_in_scope = scope->resolve_symbol(sym_name);
                    var->m_v = sym_in_scope;
                }
            }

            void visit_Function(const ASR::Function_t& x) {
                ASR::Function_t* func = (ASR::Function_t*)(&x);
                SymbolTable* parent_scope = scope;
                scope = func->m_symtab;
                if (func->m_symtab->counter == correct_scope->counter) {
                    for (size_t i = 0; i < func->n_body; i++) {
                        this->visit_stmt(*func->m_body[i]);
                    }
                    if (func->m_return_var) {
                        this->visit_expr(*func->m_return_var);
                    }
                }
                scope = parent_scope;
                for (auto &item: func->m_symtab->get_scope()) {
                    this->visit_symbol(*item.second);
                }
            }
        };

        UpdateDuplicatedNodes v(al);
        v.correct_scope = current_scope;
        SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);
        ASR::asr_t* asr_ = tu_symtab->asr_owner;
        ASR::TranslationUnit_t* tu = ASR::down_cast2<ASR::TranslationUnit_t>(asr_);
        v.visit_TranslationUnit(*tu);
    }

    void create_template_entry_function(const Location &loc, std::string function_name, std::vector<AST::arg_t> &vector_args,
                                        bool is_master = false, bool is_function = false, std::string parent_function_name = "") {
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);

        ASR::accessType s_access = dflt_access;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;

        SymbolTable *old_scope = current_scope;
        SymbolTable *parent_scope = current_scope->parent;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        ASRUtils::SymbolDuplicator symbol_duplicator(al);
        std::vector<std::string> copy_external_procedure = external_procedures;
        external_procedures.clear();
        std::vector<std::string> symbols_to_erase;
        for( auto item: old_scope->get_scope() ) {
            symbol_duplicator.duplicate_symbol(item.second, current_scope);
            bool is_external = check_is_external(item.first, old_scope);
            if (is_external) {
                external_procedures.push_back(item.first);
                // remove it from old_scope
                symbols_to_erase.push_back(item.first);
            }
        }

        for (auto it: symbols_to_erase) {
            old_scope->erase_symbol(it);
        }

        if (is_master) {
            // Create integer variable "entry__lcompilers"
            ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
            ASR::symbol_t* entry_lcompilers_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al,
                                                loc, current_scope, s2c(al, "entry__lcompilers"), nullptr, 0,
                                                ASR::intentType::In, nullptr, nullptr, ASR::storage_typeType::Default,
                                                int_type, nullptr, ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required,
                                                false));
            current_scope->add_symbol("entry__lcompilers", entry_lcompilers_sym);
        }

        for (auto it: vector_args) {
            char *arg = it.m_arg;
            if (arg) {
                current_procedure_args.push_back(to_lower(arg));
            } else {
                throw SemanticError("Alternate returns are not implemented yet",
                    it.loc);
            }
        }

        Vec<ASR::expr_t*> args;
        args.reserve(al, vector_args.size());
        for (auto it: vector_args) {
            char *arg = it.m_arg;
            std::string arg_s = to_lower(arg);
            if (current_scope->get_symbol(arg_s) == nullptr) {
                if (compiler_options.implicit_typing) {
                    ASR::ttype_t *t = implicit_dictionary[std::string(1, arg_s[0])];
                    if (t == nullptr) {
                        throw SemanticError("Dummy argument '" + arg_s + "' not defined", it.loc);
                    }
                    declare_implicit_variable2(it.loc, arg_s,
                        ASRUtils::intent_unspecified, t);
                } else {
                    throw SemanticError("Dummy argument '" + arg_s + "' not defined", it.loc);
                }
            }
            ASR::symbol_t *var = current_scope->get_symbol(arg_s);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, it.loc,
                var)));
        }

        current_procedure_abi_type = ASR::abiType::Source;

        SetChar func_deps;
        func_deps.reserve(al, current_function_dependencies.size());
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }

        ASR::symbol_t* return_var = current_scope->resolve_symbol(function_name);
        ASR::expr_t* return_var_expr = nullptr;
        if (return_var) {
            return_var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_var));
        }
        if (!return_var && is_function) {
            // create return variable, with same type as parent function
            ASR::symbol_t* parent_func_sym = current_scope->resolve_symbol(parent_function_name);
            ASR::ttype_t* return_type = ASRUtils::symbol_type(parent_func_sym);
            ASR::symbol_t* return_var_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al,
                                                loc, current_scope, s2c(al, function_name), nullptr, 0,
                                                ASR::intentType::ReturnVar, nullptr, nullptr, ASR::storage_typeType::Default,
                                                return_type, nullptr, ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required,
                                                false));
            current_scope->add_symbol(function_name, return_var_sym);
            return_var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_var_sym));
        }

        ASR::asr_t* tmp_ = ASRUtils::make_Function_t_util(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(function_name)),
            func_deps.p, func_deps.size(),
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ nullptr,
            /* n_body */ 0,
            return_var_expr,
            current_procedure_abi_type,
            s_access, deftype, nullptr,
            false, false, false, false, false,
            nullptr, 0,
            is_requirement, false, false);
        parent_scope->add_symbol(function_name, ASR::down_cast<ASR::symbol_t>(tmp_));

        for (auto &item: current_scope->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(current_scope->resolve_symbol(item.first));
                update_duplicated_nodes(al, func->m_symtab);
            } else if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(current_scope->resolve_symbol(item.first));
                ASR::ttype_t* var_type = var->m_type;
                if (var_type->type == ASR::ttypeType::Array) {
                    ASR::Array_t* arr_type = ASR::down_cast<ASR::Array_t>(var_type);
                    for (size_t i = 0; i < arr_type->n_dims; i++) {
                        ASR::dimension_t dim = arr_type->m_dims[i];
                        ASR::expr_t* dim_length = dim.m_length;
                        if (dim_length && ASR::is_a<ASR::Var_t>(*dim_length)) {
                            ASR::Var_t* dim_length_var = ASR::down_cast<ASR::Var_t>(dim_length);
                            ASR::symbol_t* dim_length_sym = dim_length_var->m_v;
                            std::string dim_length_sym_name = ASRUtils::symbol_name(dim_length_sym);
                            ASR::symbol_t* dim_length_sym_in_scope = current_scope->resolve_symbol(dim_length_sym_name);
                            if (dim_length_sym_in_scope) {
                                dim_length_var->m_v = dim_length_sym_in_scope;
                            }
                        }
                    }
                }
                // check if variable is in current current_procedure_args
                if (std::find(current_procedure_args.begin(), current_procedure_args.end(), item.first) != current_procedure_args.end()) {
                    // if yes, then make var->m_intent = ASR::intentType::Unspecified
                    var->m_intent = ASR::intentType::Unspecified;
                }
            }
        }

        // populate the external_procedures_mapping
        uint64_t hash = get_hash(tmp_);
        external_procedures_mapping[hash] = external_procedures;

        current_scope = old_scope;
        current_function_dependencies = current_function_dependencies_copy;
        external_procedures = copy_external_procedure;
        current_procedure_args.clear();
    }

    template <typename T>
    std::vector<AST::arg_t> perform_argument_mapping(const T& x, std::string sym_name) {
        // create master function
        std::vector<std::string> arg_names;
        for(size_t i = 0; i < x.n_args; i++) {
            arg_names.push_back(x.m_args[i].m_arg);
        }
        for(auto &it: entry_functions[sym_name]) {
            for(auto &arg: entry_function_args[it.first]) {
                arg_names.push_back(arg.m_arg);
            }
        }
        std::set<std::string> s(arg_names.begin(), arg_names.end());
        std::vector<std::string> arg_names_unique(s.begin(), s.end());arg_names_unique.insert(arg_names_unique.begin(), "entry__lcompilers");

        for (size_t i = 0; i < x.n_args; i++){
            AST::arg_t arg = x.m_args[i];
            // find index of arg in arg_names_unique
            auto it = std::find(arg_names_unique.begin(), arg_names_unique.end(), arg.m_arg);
            if (it != arg_names_unique.end()) {
                int index = std::distance(arg_names_unique.begin(), it);
                if (entry_function_arguments_mapping.find(sym_name) == entry_function_arguments_mapping.end()) {
                    entry_function_arguments_mapping[sym_name] = std::vector<int>();
                }
                entry_function_arguments_mapping[sym_name].push_back(index);
            }
        }
        for(auto &it: entry_functions[sym_name]) {
            for(auto &arg: entry_function_args[it.first]) {
                // find index of arg in arg_names_unique
                auto it2 = std::find(arg_names_unique.begin(), arg_names_unique.end(), arg.m_arg);
                if (it2 != arg_names_unique.end()) {
                    int index = std::distance(arg_names_unique.begin(), it2);
                    if (entry_function_arguments_mapping.find(it.first) == entry_function_arguments_mapping.end()) {
                        entry_function_arguments_mapping[it.first] = std::vector<int>();
                    }
                    entry_function_arguments_mapping[it.first].push_back(index);
                }
            }
        }
        std::vector<AST::arg_t> master_args;
        for (auto &arg: arg_names_unique) {
            AST::arg_t a; a.loc = x.base.base.loc; a.m_arg = s2c(al, arg); master_args.push_back(a);
        }

        return master_args;
    }

    void visit_Subroutine(const AST::Subroutine_t &x) {
        in_Subroutine = true;
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);
        if (compiler_options.implicit_typing) {
            Location a_loc = x.base.base.loc;
            populate_implicit_dictionary(a_loc, implicit_dictionary);
            process_implicit_statements(x, implicit_dictionary);
        } else {
            for (size_t i=0;i<x.n_implicit;i++) {
                if (!AST::is_a<AST::ImplicitNone_t>(*x.m_implicit[i])) {
                    throw SemanticError("Implicit typing is not allowed, enable it by using --implicit-typing ", x.m_implicit[i]->base.loc);
                }
            }
        }
        simd_variables.clear();
        ASR::accessType s_access = dflt_access;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;
        std::string sym_name = to_lower(x.m_name);

        SymbolTable *grandparent_scope = current_scope;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        // Handle templated subroutines
        if (x.n_temp_args > 0) {
            is_template = true;

            SetChar temp_args;
            temp_args.reserve(al, x.n_temp_args);
            for (size_t i=0; i < x.n_temp_args; i++) {
                current_procedure_args.push_back(to_lower(x.m_temp_args[i]));
                temp_args.push_back(al, s2c(al, to_lower(x.m_temp_args[i])));
            }

            Vec<ASR::require_instantiation_t*> reqs;
            reqs.reserve(al, x.n_decl);
            for (size_t i=0; i < x.n_decl; i++) {
                if (AST::is_a<AST::Require_t>(*x.m_decl[i])) {
                    AST::Require_t *r = AST::down_cast<AST::Require_t>(x.m_decl[i]);
                    for (size_t i=0; i<r->n_reqs; i++) {
                        visit_unit_require(*r->m_reqs[i]);
                        reqs.push_back(al, ASR::down_cast<ASR::require_instantiation_t>(tmp));
                        tmp = nullptr;
                    }
                }

                if (AST::is_a<AST::DerivedType_t>(*x.m_decl[i])) {
                    AST::DerivedType_t *dt = AST::down_cast<AST::DerivedType_t>(x.m_decl[i]);
                    if (std::find(current_procedure_args.begin(),
                                  current_procedure_args.end(),
                                  to_lower(dt->m_name)) != current_procedure_args.end()) {
                        visit_unit_decl2(*x.m_decl[i]);
                    }
                }
            }

            ASR::asr_t *temp = ASR::make_Template_t(al, x.base.base.loc,
                current_scope, s2c(al, sym_name), temp_args.p, temp_args.size(), reqs.p, reqs.size());

            parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(temp));
            parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            current_procedure_args.clear();
        }

        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            if (arg) {
                current_procedure_args.push_back(to_lower(arg));
            } else {
                throw SemanticError("Alternate returns are not implemented yet",
                    x.m_args[i].loc);
            }
        }
        current_procedure_abi_type = ASR::abiType::Source;
        char *bindc_name=nullptr;
        extract_bind(x, current_procedure_abi_type, bindc_name);

        for (size_t i=0; i<x.n_use; i++) {
            visit_unit_decl1(*x.m_use[i]);
        }
        for (size_t i=0; i<x.n_decl; i++) {
            is_Function = true;
            if (!AST::is_a<AST::Require_t>(*x.m_decl[i])) {
                visit_unit_decl2(*x.m_decl[i]);
            }
            is_Function = false;
        }
        process_simd_variables();
        for (size_t i=0; i<x.n_contains; i++) {
            bool current_storage_save = default_storage_save;
            default_storage_save = false;
            visit_program_unit(*x.m_contains[i]);
            default_storage_save = current_storage_save;
        }
        Vec<ASR::expr_t*> args;
        args.reserve(al, x.n_args);
        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            std::string arg_s = to_lower(arg);
            if (current_scope->get_symbol(arg_s) == nullptr) {
                if (compiler_options.implicit_typing) {
                    ASR::ttype_t *t = implicit_dictionary[std::string(1, arg_s[0])];
                    if (t == nullptr) {
                        throw SemanticError("Dummy argument '" + arg_s + "' not defined", x.base.base.loc);
                    }
                    declare_implicit_variable2(x.base.base.loc, arg_s,
                        ASRUtils::intent_unspecified, t);
                } else {
                    throw SemanticError("Dummy argument '" + arg_s + "' not defined", x.base.base.loc);
                }
            }
            ASR::symbol_t *var = current_scope->get_symbol(arg_s);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                var)));
        }
        if (assgnd_access.count(sym_name)) {
            s_access = assgnd_access[sym_name];
        }
        if (is_interface){
            deftype = ASR::deftypeType::Interface;
        }
        bool is_pure = false, is_module = false, is_elemental = false;
        for( size_t i = 0; i < x.n_attributes; i++ ) {
            switch( x.m_attributes[i]->type ) {
                case AST::decl_attributeType::SimpleAttribute: {
                    AST::SimpleAttribute_t* simple_attr = AST::down_cast<AST::SimpleAttribute_t>(x.m_attributes[i]);
                    if( simple_attr->m_attr == AST::simple_attributeType::AttrPure ) {
                        is_pure = true;
                    } else if( simple_attr->m_attr == AST::simple_attributeType::AttrModule ) {
                        is_module = true;
                    } else if( simple_attr->m_attr == AST::simple_attributeType::AttrElemental ) {
                        is_elemental = true;
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

        bool update_gp = false;
        int gp_index_to_be_updated = -1;
        ASR::symbol_t* f1_ = nullptr;
        if (parent_scope->get_symbol(sym_name) != nullptr) {
            f1_ = parent_scope->get_symbol(sym_name);
            ASR::symbol_t *f1 = ASRUtils::symbol_get_past_external(f1_);
            if (ASR::is_a<ASR::Function_t>(*f1)) {
                ASR::Function_t* f2 = ASR::down_cast<ASR::Function_t>(f1);
                if (ASRUtils::get_FunctionType(f2)->m_abi == ASR::abiType::Interactive ||
                    ASRUtils::get_FunctionType(f2)->m_deftype == ASR::deftypeType::Interface) {
                    // Previous declaration will be shadowed
                    parent_scope->erase_symbol(sym_name);
                } else {
                    throw SemanticError("Subroutine already defined " + sym_name, tmp->loc);
                }
            } else if( ASR::is_a<ASR::GenericProcedure_t>(*f1) ) {
                ASR::GenericProcedure_t* gp = ASR::down_cast<ASR::GenericProcedure_t>(f1);
                if( sym_name == gp->m_name ) {
                    sym_name = sym_name + "~genericprocedure";
                }

                if( !ASR::is_a<ASR::GenericProcedure_t>(*f1_) ) {
                    update_gp = true;
                    Vec<ASR::symbol_t*> gp_procs;
                    gp_procs.from_pointer_n_copy(al, gp->m_procs, gp->n_procs);
                    f1_ = ASR::down_cast<ASR::symbol_t>(ASR::make_GenericProcedure_t(al, f1->base.loc,
                        parent_scope, gp->m_name, gp_procs.p, gp_procs.size(), gp->m_access));
                    parent_scope->overwrite_symbol(gp->m_name, f1_);
                }

                for( size_t igp = 0; igp < gp->n_procs; igp++ ) {
                    if( ASRUtils::symbol_get_past_external(gp->m_procs[igp]) ==
                        ASRUtils::symbol_get_past_external(parent_scope->resolve_symbol(sym_name)) ) {
                        gp_index_to_be_updated = igp;
                        break;
                    }
                }

                // Any import from parent module will be shadowed
                parent_scope->erase_symbol(sym_name);
            } else if (compiler_options.implicit_typing && ASR::is_a<ASR::Variable_t>(*f1)) {
                // function previously added as variable due to implicit typing
                parent_scope->erase_symbol(sym_name);
            } else {
                throw SemanticError("Subroutine already defined " + sym_name, tmp->loc);
            }
        }
        if( sym_name == interface_name ) {
            sym_name = sym_name + "~genericprocedure";
        }

        SetChar func_deps;
        func_deps.reserve(al, current_function_dependencies.size());
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        tmp = ASRUtils::make_Function_t_util(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(sym_name)),
            func_deps.p, func_deps.size(),
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ nullptr,
            /* n_body */ 0,
            nullptr,
            current_procedure_abi_type,
            s_access, deftype, bindc_name,
            is_elemental, is_pure, is_module, false, false,
            nullptr, 0,
            is_requirement, false, false);
        handle_save();
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        if( update_gp ) {
            LCOMPILERS_ASSERT(gp_index_to_be_updated >= 0);
            ASR::GenericProcedure_t* f1_gp = ASR::down_cast<ASR::GenericProcedure_t>(f1_);
            f1_gp->m_procs[gp_index_to_be_updated] = ASR::down_cast<ASR::symbol_t>(tmp);
        }
        // populate the external_procedures_mapping
        uint64_t hash = get_hash(tmp);
        external_procedures_mapping[hash] = external_procedures;
        if (subroutine_contains_entry_function(sym_name, x.m_body, x.n_body)) {
            /*
                This subroutine contains an entry function, create
                template function for each entry and a master function
            */
            for (auto &entry_function: entry_functions[sym_name]) {
                create_template_entry_function(x.base.base.loc, entry_function.first, entry_function_args[entry_function.first], false, false, sym_name);
            }
            std::vector<AST::arg_t> master_args = perform_argument_mapping(x, sym_name);

            create_template_entry_function(x.base.base.loc, sym_name+"_main__lcompilers", master_args, true, false, sym_name);
        }
        entry_function_args.clear();
        if (x.n_temp_args > 0) {
            current_scope = grandparent_scope;
        } else {
            current_scope = parent_scope;
        }
        /* FIXME: This can become incorrect/get cleared prematurely, perhaps
           in nested functions, and also in callback.f90 test, but it may not
           matter since we would have already checked the intent */
        current_procedure_args.clear();
        current_procedure_abi_type = ASR::abiType::Source;

        // print_implicit_dictionary(implicit_dictionary);
        // get hash of the function and add it to the implicit_mapping
        if (compiler_options.implicit_typing) {
            uint64_t hash = get_hash(tmp);

            implicit_mapping[hash] = implicit_dictionary;

            implicit_dictionary.clear();
        }

        current_function_dependencies = current_function_dependencies_copy;
        in_Subroutine = false;
        is_template = false;
        mark_common_blocks_as_declared();
    }

    AST::AttrType_t* find_return_type(AST::decl_attribute_t** attributes,
            size_t n, const Location &loc, std::string &return_var_name) {
        AST::AttrType_t* r = nullptr;
        bool found = false;
        if (n == 0 && compiler_options.implicit_interface && compiler_options.implicit_typing) {
            std::string first_letter = to_lower(std::string(1,return_var_name[0]));
            ASR::ttype_t* t = implicit_dictionary[first_letter];
            AST::decl_typeType ttype;
            if (t == nullptr) {
                throw SemanticError("No implicit return type available for `" + return_var_name +"`", loc);
            }
            switch( t->type ) {
                case ASR::ttypeType::Integer: {
                    ttype = AST::decl_typeType::TypeInteger;
                    break;
                }
                case ASR::ttypeType::Real: {
                    // check if it is a double precision
                    int a_kind = ASR::down_cast<ASR::Real_t>(t)->m_kind;
                    if (a_kind == 8) {
                        ttype = AST::decl_typeType::TypeDoublePrecision;
                        break;
                    } else {
                        ttype = AST::decl_typeType::TypeReal;
                        break;
                    }
                }
                case ASR::ttypeType::Complex: {
                    ttype = AST::decl_typeType::TypeComplex;
                    break;
                }
                case ASR::ttypeType::Logical: {
                    ttype = AST::decl_typeType::TypeLogical;
                    break;
                }
                case ASR::ttypeType::Character: {
                    ttype = AST::decl_typeType::TypeCharacter;
                    break;
                }
                default: {
                    throw SemanticError("Implicit return type not supported yet", loc);
                }
            }
            AST::ast_t* r_ast = AST::make_AttrType_t(al, loc, ttype, nullptr, 0, nullptr, nullptr, AST::symbolType::None);
            AST::decl_attribute_t* r_attr = AST::down_cast<AST::decl_attribute_t>(r_ast);
            r = AST::down_cast<AST::AttrType_t>(r_attr);
        }
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
        in_Subroutine = true;
        SetChar current_function_dependencies_copy = current_function_dependencies;
        current_function_dependencies.clear(al);
        if (compiler_options.implicit_typing) {
            Location a_loc = x.base.base.loc;
            populate_implicit_dictionary(a_loc, implicit_dictionary);
            process_implicit_statements(x, implicit_dictionary);
        } else {
            for (size_t i=0;i<x.n_implicit;i++) {
                if (!AST::is_a<AST::ImplicitNone_t>(*x.m_implicit[i])) {
                    throw SemanticError("Implicit typing is not allowed, enable it by using --implicit-typing ", x.m_implicit[i]->base.loc);
                }
            }
        }
        simd_variables.clear();
        // Extract local (including dummy) variables first
        current_symbol = (int64_t) ASR::symbolType::Function;
        ASR::accessType s_access = dflt_access;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;
        std::string sym_name = to_lower(x.m_name);

        SymbolTable *grandparent_scope = current_scope;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        // Handle templated functions
        std::map<AST::intrinsicopType, std::vector<std::string>> ext_overloaded_op_procs;

        if (x.n_temp_args > 0) {
            is_template = true;

            SetChar temp_args;
            temp_args.reserve(al, x.n_temp_args);
            for (size_t i=0; i < x.n_temp_args; i++) {
                current_procedure_args.push_back(to_lower(x.m_temp_args[i]));
                temp_args.push_back(al, s2c(al, to_lower(x.m_temp_args[i])));
            }
            for (auto &proc: overloaded_op_procs) {
                ext_overloaded_op_procs[proc.first] = proc.second;
            }
            overloaded_op_procs.clear();

            Vec<ASR::require_instantiation_t*> reqs;
            reqs.reserve(al, x.n_decl);
            for (size_t i=0; i < x.n_decl; i++) {
                if (AST::is_a<AST::Require_t>(*x.m_decl[i])) {
                    AST::Require_t *r = AST::down_cast<AST::Require_t>(x.m_decl[i]);
                    for (size_t i=0; i<r->n_reqs; i++) {
                        visit_unit_require(*r->m_reqs[i]);
                        reqs.push_back(al, ASR::down_cast<ASR::require_instantiation_t>(tmp));
                        tmp = nullptr;
                    }
                }
                if (AST::is_a<AST::DerivedType_t>(*x.m_decl[i])) {
                    AST::DerivedType_t *dt = AST::down_cast<AST::DerivedType_t>(x.m_decl[i]);
                    if (std::find(current_procedure_args.begin(),
                                  current_procedure_args.end(),
                                  to_lower(dt->m_name)) != current_procedure_args.end()) {
                        visit_unit_decl2(*x.m_decl[i]);
                    }
                }
            }

            ASR::asr_t *temp = ASR::make_Template_t(al, x.base.base.loc,
                current_scope, s2c(al, sym_name), temp_args.p, temp_args.size(), reqs.p, reqs.size());
            parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(temp));
            parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            current_procedure_args.clear();
        }

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
            is_Function = true;
            if (!AST::is_a<AST::Require_t>(*x.m_decl[i])) {
                visit_unit_decl2(*x.m_decl[i]);
            }
            is_Function = false;
        }
        process_simd_variables();
        for (size_t i=0; i<x.n_contains; i++) {
            bool current_storage_save = default_storage_save;
            default_storage_save = false;
            visit_program_unit(*x.m_contains[i]);
            default_storage_save = current_storage_save;
        }
        // Convert and check arguments
        Vec<ASR::expr_t*> args;
        args.reserve(al, x.n_args);
        for (size_t i=0; i<x.n_args; i++) {
            char *arg=x.m_args[i].m_arg;
            std::string arg_s = to_lower(arg);
            if (current_scope->get_symbol(arg_s) == nullptr) {
                if (compiler_options.implicit_typing) {
                    ASR::ttype_t *t = implicit_dictionary[std::string(1, arg_s[0])];
                    declare_implicit_variable2(x.base.base.loc, arg_s,
                        ASRUtils::intent_unspecified, t);
                } else {
                    throw SemanticError("Dummy argument '" + arg_s + "' not defined", x.base.base.loc);
                }
            }
            ASR::symbol_t *var = current_scope->get_symbol(arg_s);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
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
            x.n_attributes, x.base.base.loc, return_var_name);
        if (current_scope->get_symbol(return_var_name) == nullptr) {
            // The variable is not defined among local variables, extract the
            // type from "integer function f()" and add the variable.
            if (!return_type) {
                throw SemanticError("Return type not specified",
                        x.base.base.loc);
            }
            ASR::ttype_t *type;
            int i_kind = compiler_options.po.default_integer_kind;
            int a_kind = 4;
            int a_len = -10;
            if (return_type->m_kind != nullptr) {
                if (return_type->n_kind == 1) {
                    visit_expr(*return_type->m_kind->m_value);
                    ASR::expr_t* kind_expr = ASRUtils::EXPR(tmp);
                    if (return_type->m_type == AST::decl_typeType::TypeCharacter) {
                        a_len = ASRUtils::extract_len<SemanticError>(kind_expr, x.base.base.loc);
                    } else {
                        a_kind = ASRUtils::extract_kind<SemanticError>(kind_expr, x.base.base.loc);
                        i_kind = a_kind;
                    }
                } else {
                    throw SemanticError("Only one kind item supported for now", x.base.base.loc);
                }
            }
            switch (return_type->m_type) {
                case (AST::decl_typeType::TypeInteger) : {
                    type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, i_kind));
                    break;
                }
                case (AST::decl_typeType::TypeReal) : {
                    type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, a_kind));
                    break;
                }
                case (AST::decl_typeType::TypeDoublePrecision) : {
                    type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 8));
                    break;
                }
                case (AST::decl_typeType::TypeComplex) : {
                    type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc, a_kind));
                    break;
                }
                case (AST::decl_typeType::TypeDoubleComplex) : {
                    type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc, 8));
                    break;
                }
                case (AST::decl_typeType::TypeLogical) : {
                    type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                    break;
                }
                case (AST::decl_typeType::TypeCharacter) : {
                    type = ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc, 1, a_len, nullptr));
                    break;
                }
                case (AST::decl_typeType::TypeType) : {
                    LCOMPILERS_ASSERT(return_type->m_name);
                    std::string derived_type_name = to_lower(return_type->m_name);
                    ASR::symbol_t *v = current_scope->resolve_symbol(derived_type_name);
                    if (!v) {
                        throw SemanticError("Derived type '"
                            + derived_type_name + "' not declared", x.base.base.loc);

                    }
                    type = ASRUtils::TYPE(ASR::make_Struct_t(al, x.base.base.loc, v));
                    break;
                }
                default :
                    throw SemanticError("Return type not supported",
                            x.base.base.loc);
            }
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
            // Add it as a local variable:
            return_var = ASR::make_Variable_t(al, x.base.base.loc,
                current_scope, s2c(al, return_var_name), variable_dependencies_vec.p,
                variable_dependencies_vec.size(), ASRUtils::intent_return_var,
                nullptr, nullptr, ASR::storage_typeType::Default, type, nullptr,
                current_procedure_abi_type, ASR::Public, ASR::presenceType::Required,
                false);
            current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
        } else {
            if (return_type && !(x.n_attributes == 0 && compiler_options.implicit_typing && compiler_options.implicit_interface)) {
                throw SemanticError("Cannot specify the return type twice",
                    x.base.base.loc);
            }
            // Extract the variable from the local scope
            return_var = (ASR::asr_t*) current_scope->get_symbol(return_var_name);
            ASR::Variable_t* return_variable = ASR::down_cast2<ASR::Variable_t>(return_var);
            return_variable->m_intent = ASRUtils::intent_return_var;
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, return_variable->m_type,
                                                    return_variable->m_symbolic_value, return_variable->m_value);
            return_variable->m_dependencies = variable_dependencies_vec.p;
            return_variable->n_dependencies = variable_dependencies_vec.size();
        }

        ASR::asr_t *return_var_ref = ASR::make_Var_t(al, x.base.base.loc,
            ASR::down_cast<ASR::symbol_t>(return_var));

        // Create and register the function
        if (assgnd_access.count(sym_name)) {
            s_access = assgnd_access[sym_name];
        }

        if (is_interface) {
            deftype = ASR::deftypeType::Interface;
        }

        if( generic_procedures.find(sym_name) != generic_procedures.end() ) {
            sym_name = sym_name + "~genericprocedure";
        }

        if (parent_scope->get_symbol(sym_name) != nullptr) {
            ASR::symbol_t *f1 = parent_scope->get_symbol(sym_name);
            if (ASR::is_a<ASR::ExternalSymbol_t>(*f1) && in_submodule) {
                parent_scope->erase_symbol(sym_name);
            } else if (ASR::is_a<ASR::Function_t>(*f1)) {
                ASR::Function_t* f2 = ASR::down_cast<ASR::Function_t>(f1);
                if (ASRUtils::get_FunctionType(f2)->m_abi == ASR::abiType::Interactive) {
                    // Previous declaration will be shadowed
                    parent_scope->erase_symbol(sym_name);
                } else {
                    throw SemanticError("Function already defined", tmp->loc);
                }
            } else if (compiler_options.implicit_typing && ASR::is_a<ASR::Variable_t>(*f1)) {
                // function previously added as variable due to implicit typing
                parent_scope->erase_symbol(sym_name);
            } else {
                throw SemanticError("Function already defined", tmp->loc);
            }
        }

        bool is_elemental = false;
        bool is_pure = false;
        for(size_t i = 0; i < x.n_attributes; i++) {
            switch( x.m_attributes[i]->type ) {
                case AST::decl_attributeType::SimpleAttribute: {
                    AST::SimpleAttribute_t* simple_attr = AST::down_cast<AST::SimpleAttribute_t>(
                        x.m_attributes[i]);
                    if( simple_attr->m_attr == AST::simple_attributeType::AttrPure ) {
                        is_pure = true;
                    } else if( simple_attr->m_attr == AST::simple_attributeType::AttrElemental ) {
                        is_elemental = true;
                    }
                    break;
                }
                default: {
                    // pass
                    break;
                }
            }
        }

        SetChar func_deps;
        func_deps.reserve(al, current_function_dependencies.size());
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }
        tmp = ASRUtils::make_Function_t_util(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(sym_name)),
            func_deps.p, func_deps.size(),
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ nullptr,
            /* n_body */ 0,
            /* a_return_var */ ASRUtils::EXPR(return_var_ref),
            current_procedure_abi_type, s_access, deftype,
            bindc_name, is_elemental, is_pure, false, false, false,
            nullptr, 0, is_requirement, false, false);
        handle_save();
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        // populate the external_procedures_mapping
        uint64_t hash = get_hash(tmp);
        external_procedures_mapping[hash] = external_procedures;
        if (subroutine_contains_entry_function(sym_name, x.m_body, x.n_body)) {
            /*
                This subroutine contains an entry function, create
                template function for each entry and a master function
            */
            for (auto &entry_function: entry_functions[sym_name]) {
                create_template_entry_function(x.base.base.loc, entry_function.first, entry_function_args[entry_function.first], false, true, sym_name);
            }
            std::vector<AST::arg_t> master_args = perform_argument_mapping(x, sym_name);

            create_template_entry_function(x.base.base.loc, sym_name+"_main__lcompilers", master_args, true, true, sym_name);
        }
        if (x.n_temp_args > 0) {
            add_overloaded_procedures();
            for (auto &proc: ext_overloaded_op_procs) {
                overloaded_op_procs[proc.first] = proc.second;
            }
            for (size_t i=0; i<x.n_temp_args; i++) {
                ASR::symbol_t *s = parent_scope->get_symbol(to_lower(x.m_temp_args[i]));
                if (!s) {
                    throw SemanticError("Template argument " + std::string(x.m_temp_args[i])
                        + " has not been declared in templated function specification.",
                        x.base.base.loc);
                }
            }
            current_scope = grandparent_scope;
        } else {
            current_scope = parent_scope;
        }
        current_procedure_args.clear();
        current_procedure_abi_type = ASR::abiType::Source;
        current_symbol = -1;
        // print_implicit_dictionary(implicit_dictionary);
        // get hash of the function and add it to the implicit_mapping
        if (compiler_options.implicit_typing) {
            uint64_t hash = get_hash(tmp);

            implicit_mapping[hash] = implicit_dictionary;

            implicit_dictionary.clear();
        }
        current_function_dependencies = current_function_dependencies_copy;
        in_Subroutine = false;
        mark_common_blocks_as_declared();
    }

    void visit_Declaration(const AST::Declaration_t& x) {
        visit_DeclarationUtil(x);
    }

    void visit_DeclarationPragma(const AST::DeclarationPragma_t &x) {
        if (compiler_options.ignore_pragma) {
            return;
        }
        if (x.m_type == AST::LFortranPragma) {
            std::string t = x.m_text;
            if (startswith(t, "attributes ")) {
                t = t.substr(11);
                if (startswith(t, "simd :: ")) {
                    t = t.substr(8);
                    // !LF$ attributes simd :: X, Y, Z
                    for (auto &var : string_split(t, ", ")) {
                        simd_variables.push_back(std::pair(to_lower(var), x.base.base.loc));
                    }
                } else {
                    throw SemanticError("Only `simd` attribute supported",
                        x.base.base.loc);
                }
            } else {
                throw SemanticError("Unsupported LFortran pragma type",
                    x.base.base.loc);
            }

        } else {
            throw SemanticError("The pragma type not supported yet",
                x.base.base.loc);
        }
    }

    void process_simd_variables() {
        for (auto &var : simd_variables) {
            ASR::symbol_t *s = current_scope->get_symbol(var.first);
            if (s) {
                ASR::ttype_t *t = ASRUtils::symbol_type(s);
                if (ASR::is_a<ASR::Array_t>(*t)) {
                    ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(t);
                    a->m_physical_type = ASR::array_physical_typeType::SIMDArray;
                    // TODO: check all the SIMD requirements here:
                    // * 1D array
                    // * the right, compile time, size, compatible type
                    // * Not allocatable, or pointer
                } else {
                    throw SemanticError("The SIMD variable `" + var.first + "` must be an array",
                        t->base.loc);
                }
            } else {
                throw SemanticError("The SIMD variable `" + var.first + "` not declared",
                    var.second);
            }

        }
        simd_variables.clear();
    }

    void visit_DerivedType(const AST::DerivedType_t &x) {
        dt_name = to_lower(x.m_name);
        bool is_abstract = false;
        bool is_deferred = false;
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
                case AST::decl_attributeType::SimpleAttribute: {
                    AST::SimpleAttribute_t* simple_attr =
                        AST::down_cast<AST::SimpleAttribute_t>(x.m_attrtype[i]);
                    is_abstract = simple_attr->m_attr == AST::simple_attributeType::AttrAbstract;
                    is_deferred = simple_attr->m_attr == AST::simple_attributeType::AttrDeferred;
                }
                default:
                    break;
            }
        }
        if ((is_requirement || is_template) && is_deferred) {
            ASR::asr_t *tp = ASR::make_TypeParameter_t(al, x.base.base.loc, s2c(al, dt_name));
            tmp = ASR::make_Variable_t(al, x.base.base.loc, current_scope, s2c(al, dt_name),
                nullptr, 0, ASRUtils::intent_in, nullptr, nullptr, ASR::storage_typeType::Default,
                ASRUtils::TYPE(tp), nullptr, ASR::abiType::Source, dflt_access, ASR::presenceType::Required, false);
            current_scope->add_symbol(dt_name, ASR::down_cast<ASR::symbol_t>(tmp));
            return;
        }
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        data_member_names.reserve(al, 0);
        is_derived_type = true;
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
        SetChar struct_dependencies;
        struct_dependencies.reserve(al, 1);
        for( auto& item: current_scope->get_scope() ) {
            // ExternalSymbol means that current module/program
            // already depends on the module of ExternalSymbol
            // present inside StructType's scope. So the order
            // is already established and hence no need to store
            // this ExternalSymbol as a dependency.
            if( ASR::is_a<ASR::ExternalSymbol_t>(*item.second) ) {
                continue;
            }
            ASR::ttype_t* var_type = ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(item.second));
            char* aggregate_type_name = nullptr;
            if( ASR::is_a<ASR::Struct_t>(*var_type) ) {
                ASR::symbol_t* sym = ASR::down_cast<ASR::Struct_t>(var_type)->m_derived_type;
                aggregate_type_name = ASRUtils::symbol_name(sym);
            } else if( ASR::is_a<ASR::Class_t>(*var_type) ) {
                ASR::symbol_t* sym = ASR::down_cast<ASR::Class_t>(var_type)->m_class_type;
                aggregate_type_name = ASRUtils::symbol_name(sym);
            }
            if( aggregate_type_name ) {
                struct_dependencies.push_back(al, aggregate_type_name);
            }
        }
        tmp = ASR::make_StructType_t(al, x.base.base.loc, current_scope,
            s2c(al, to_lower(x.m_name)), struct_dependencies.p, struct_dependencies.size(),
            data_member_names.p, data_member_names.size(),
            ASR::abiType::Source, dflt_access, false, is_abstract, nullptr, 0, nullptr, parent_sym);
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
        is_derived_type = false;
    }

    void visit_InterfaceProc(const AST::InterfaceProc_t &x) {
        bool old_is_interface = is_interface;
        is_interface = true;
        visit_program_unit(*x.m_proc);
        is_interface = old_is_interface;
        return;
    }

    void visit_DerivedTypeProc(const AST::DerivedTypeProc_t &x) {
        for (size_t i = 0; i < x.n_symbols; i++) {
            AST::UseSymbol_t *use_sym = AST::down_cast<AST::UseSymbol_t>(
                x.m_symbols[i]);
            ClassProcInfo remote_sym_str;
            remote_sym_str.loc = x.base.base.loc;
            if( x.m_name ) {
                remote_sym_str.name = to_lower(x.m_name);
            } else {
                remote_sym_str.name = to_lower(use_sym->m_remote_sym);
            }
            std::string use_sym_name = "";
            if (use_sym->m_local_rename) {
                use_sym_name = to_lower(use_sym->m_local_rename);
            } else {
                use_sym_name = to_lower(use_sym->m_remote_sym);
            }
            class_procedures[dt_name][use_sym_name]["procedure"] = remote_sym_str;
            for( size_t i = 0; i < x.n_attr; i++ ) {
                switch( x.m_attr[i]->type ) {
                    case AST::decl_attributeType::AttrPass: {
                        AST::AttrPass_t* attr_pass = AST::down_cast<AST::AttrPass_t>(x.m_attr[i]);
                        LCOMPILERS_ASSERT(class_procedures[dt_name][use_sym_name].find("pass") == class_procedures[dt_name][use_sym_name].end());
                        class_procedures[dt_name][use_sym_name]["pass"].name = (attr_pass->m_name) ? std::string(attr_pass->m_name) : "";
                        class_procedures[dt_name][use_sym_name]["pass"].loc =  attr_pass->base.base.loc;
                        break ;
                    }
                    case AST::decl_attributeType::SimpleAttribute: {
                        auto &cdf = class_deferred_procedures;
                        AST::SimpleAttribute_t* attr = AST::down_cast<AST::SimpleAttribute_t>(x.m_attr[i]);
                        if( attr->m_attr == AST::simple_attributeType::AttrDeferred ) {
                            LCOMPILERS_ASSERT(cdf[dt_name][use_sym_name].find("deferred") == cdf[dt_name][use_sym_name].end());
                            cdf[dt_name][use_sym_name]["deferred"] = attr->base.base.loc;
                        } else if (attr->m_attr == AST::simple_attributeType::AttrNoPass) {
                            LCOMPILERS_ASSERT(cdf[dt_name][use_sym_name].find("nopass") == cdf[dt_name][use_sym_name].end());
                            cdf[dt_name][use_sym_name]["nopass"] = attr->base.base.loc;
                        }
                        break;
                    }
                    default: {
                        break ;
                    }
                }
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
                        LCOMPILERS_ASSERT(false);
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
            if( generic_procedures.find(generic_name) != generic_procedures.end() ) {
                generic_procedures[generic_name].insert(generic_procedures[generic_name].end(),
                    proc_names.begin(), proc_names.end());
            } else {
                generic_procedures[generic_name] = proc_names;
            }
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
                x = resolve_symbol(loc, to_lower(name));
                symbols.push_back(al, x);
            }
            LCOMPILERS_ASSERT(strlen(generic_name) > 0);
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
            LCOMPILERS_ASSERT(strlen(generic_name) > 0);
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
                // lower case the name
                name = s2c(al, to_lower(name));
                x = resolve_symbol(loc, name);
                symbols.push_back(al, x);
            }
            std::string sym_name_str = proc.first;
            if( current_scope->get_symbol(proc.first) != nullptr ) {
                ASR::symbol_t* der_type_name = current_scope->get_symbol(proc.first);
                if( der_type_name->type == ASR::symbolType::StructType ||
                    der_type_name->type == ASR::symbolType::Function ) {
                    sym_name_str = "~" + proc.first;
                }
            }
            Str s;
            s.from_str_view(sym_name_str);
            char *generic_name = s.c_str(al);
            if (current_scope->resolve_symbol(generic_name)) {
                // Check for ExternalSymbol (GenericProcedure)
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    current_scope->resolve_symbol(generic_name));
                if(ASR::is_a<ASR::GenericProcedure_t>(*sym)) {
                    ASR::GenericProcedure_t *gp
                        = ASR::down_cast<ASR::GenericProcedure_t>(sym);
                    for (size_t i=0; i < gp->n_procs; i++) {
                        ASR::symbol_t *s = current_scope->get_symbol(
                            ASRUtils::symbol_name(gp->m_procs[i]));
                        if (s != nullptr) {
                            // Append all the module procedure's in the scope
                            symbols.push_back(al, s);
                        } else {
                            // If not available, import it from the module
                            // Create an ExternalSymbol using it
                            ASR::Module_t *m = ASRUtils::get_sym_module(sym);
                            s = m->m_symtab->get_symbol(
                                ASRUtils::symbol_name(gp->m_procs[i]));
                            if (ASR::is_a<ASR::Function_t>(*s)) {
                                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                                ASR::symbol_t *ep_s = (ASR::symbol_t *)
                                    ASR::make_ExternalSymbol_t(
                                        al, fn->base.base.loc, current_scope,
                                        fn->m_name, s, m->m_name, nullptr, 0,
                                        fn->m_name, dflt_access);
                                current_scope->add_symbol(fn->m_name, ep_s);
                                // Append the ExternalSymbol
                                symbols.push_back(al, ep_s);
                            }
                        }
                    }
                }
            }
            ASR::asr_t *v = ASR::make_GenericProcedure_t(al, loc,
                current_scope, generic_name,
                symbols.p, symbols.size(), ASR::Public);
            current_scope->add_or_overwrite_symbol(sym_name_str, ASR::down_cast<ASR::symbol_t>(v));
        }
    }

    void add_generic_class_procedures() {
        for (auto &proc : generic_class_procedures) {
            Location loc;
            loc.first = 1;
            loc.last = 1;
            ASR::StructType_t *clss = ASR::down_cast<ASR::StructType_t>(
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
                ASR::asr_t *v = nullptr;

                // Check for GenericOperator
                bool operator_found = false;
                for (auto &[key, value]: intrinsic2str) {
                    if (value == pname.first && pname.first.size() > 0) {
                        operator_found  = true;
                    }
                }
                if ( operator_found || startswith(pname.first, "~def_op~") ) {
                    // GenericOperator and GenericDefinedOperator
                    LCOMPILERS_ASSERT(strlen(generic_name) > 0);
                    v = ASR::make_CustomOperator_t(al, loc, clss->m_symtab,
                        generic_name, cand_procs.p, cand_procs.size(),
                        ASR::accessType::Public);
                } else if( pname.first == "~assign" ) {
                    LCOMPILERS_ASSERT(strlen(generic_name) > 0);
                    v = ASR::make_CustomOperator_t(al, loc, clss->m_symtab,
                        generic_name, cand_procs.p, cand_procs.size(),
                        ASR::accessType::Public);
                } else {
                    LCOMPILERS_ASSERT(strlen(generic_name) > 0);
                    v = ASR::make_GenericProcedure_t(al, loc,
                        clss->m_symtab, generic_name,
                        cand_procs.p, cand_procs.size(), ASR::accessType::Public); // Update the access as per the input Fortran code
                }
                ASR::symbol_t *cls_proc_sym = ASR::down_cast<ASR::symbol_t>(v);
                clss->m_symtab->add_symbol(pname.first, cls_proc_sym);
            }
        }
    }

    bool arg_type_equal_to_class(ASR::ttype_t* var_type, ASR::symbol_t* clss_sym) {
        if (ASR::is_a<ASR::Class_t>(*var_type)) {
            ASR::Class_t* var_type_clss = ASR::down_cast<ASR::Class_t>(var_type);
            ASR::symbol_t* var_type_clss_sym = var_type_clss->m_class_type;
            while (var_type_clss_sym) {
                if (var_type_clss_sym == clss_sym) {
                    return true;
                }
                var_type_clss_sym = ASR::down_cast<ASR::StructType_t>(var_type_clss_sym)->m_parent;
            }
        }
        return false;
    }

    void ensure_matching_types_for_pass_obj_dum_arg(ASR::Function_t* func, char* pass_arg_name, ASR::symbol_t* clss_sym, Location &loc) {
        if (pass_arg_name == nullptr) {
            ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(*func);
            if (func_type->n_arg_types == 0 ||
                !arg_type_equal_to_class(func_type->m_arg_types[0], clss_sym)) {
                throw SemanticError("Passed object dummy argument does not match function argument", loc);
            }
        } else {
            bool is_pass_arg_name_found = false;
            for (size_t i = 0; i < func->n_args && !is_pass_arg_name_found; i++) {
                ASR::Variable_t* v = ASRUtils::EXPR2VAR(func->m_args[i]);
                if (strcmp(v->m_name, pass_arg_name) == 0) {
                    if (!arg_type_equal_to_class(v->m_type, clss_sym)) {
                        throw SemanticError("Passed object dummy argument " + std::string(pass_arg_name)
                            + " type does not match function argument", loc);
                    }
                    is_pass_arg_name_found = true;
                }
            }
            if (!is_pass_arg_name_found) {
                throw SemanticError("Passed object dummy argument " + std::string(pass_arg_name)
                    + " not found in function arguments", loc);
            }
        }
    }

    bool check_is_deferred(const std::string& pname, ASR::StructType_t* clss) {
        auto& cdf = class_deferred_procedures;
        while( true ) {
            std::string proc = clss->m_name;
            if(cdf.count(proc) && cdf[proc].count(pname) && cdf[proc][pname].count("deferred")) {
                return true;
            }
            ASR::symbol_t* clss_sym = ASRUtils::symbol_get_past_external(clss->m_parent);
            if( !clss_sym ) {
                break;
            }
            LCOMPILERS_ASSERT(ASR::is_a<ASR::StructType_t>(*clss_sym));
            clss = ASR::down_cast<ASR::StructType_t>(clss_sym);
        }
        return false;
    }

    void add_class_procedures() {
        for (auto &proc : class_procedures) {
            ASR::symbol_t* clss_sym = ASRUtils::symbol_get_past_external(
                current_scope->resolve_symbol(proc.first));
            ASR::StructType_t *clss = ASR::down_cast<ASR::StructType_t>(clss_sym);
            SymbolTable* proc_scope = ASRUtils::symbol_parent_symtab(clss_sym);
            for (auto &pname : proc.second) {
                auto &loc = pname.second["procedure"].loc;
                auto& cdf = class_deferred_procedures;
                bool is_pass = pname.second.count("pass");
                bool is_deferred = check_is_deferred(pname.first, clss);
                bool is_nopass = (cdf.count(proc.first) && cdf[proc.first].count(pname.first) && cdf[proc.first][pname.first].count("nopass"));
                if (is_pass && is_nopass) {
                    throw SemanticError(diag::Diagnostic("Pass and NoPass attributes cannot be provided together",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("pass specified here", { pname.second["pass"].loc} ),
                            diag::Label("nopass specified here", { cdf[proc.first][pname.first]["nopass"] })
                        }));
                }

                ASR::symbol_t *proc_sym = proc_scope->resolve_symbol(pname.second["procedure"].name);
                if (proc_sym == nullptr) {
                    if (is_deferred) {
                        throw SemanticError("Interface must be specified for DEFERRED binding", cdf[proc.first][pname.first]["deferred"]);
                    } else {
                        throw SemanticError("'" + pname.second["procedure"].name + "' must be a module procedure"
                            " or an external procedure with an explicit interface", loc);
                    }
                }
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc_sym);
                if (!is_deferred &&
                    ASRUtils::get_FunctionType(*func)->m_deftype == ASR::deftypeType::Interface) {
                    throw SemanticError("PROCEDURE(interface) should be declared DEFERRED", loc);
                }
                Str s;
                s.from_str_view(pname.first);
                char *name = s.c_str(al);
                s.from_str_view(pname.second["procedure"].name);
                char *proc_name = s.c_str(al);
                char* pass_arg_name = nullptr;
                if( is_pass && pname.second["pass"].name.length() > 0) {
                    pass_arg_name = s2c(al, pname.second["pass"].name);
                }
                if (!is_nopass) {
                    ensure_matching_types_for_pass_obj_dum_arg(func, pass_arg_name, clss_sym, loc);
                }
                ASR::asr_t *v = ASR::make_ClassProcedure_t(al, loc,
                    clss->m_symtab, name, pass_arg_name,
                    proc_name, proc_sym, ASR::abiType::Source,
                    is_deferred, is_nopass);
                ASR::symbol_t *cls_proc_sym = ASR::down_cast<ASR::symbol_t>(v);
                clss->m_symtab->add_symbol(pname.first, cls_proc_sym);
            }
        }
    }

    std::string import_all(const ASR::Module_t* m, bool to_submodule=false) {
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
                // check if m_access of mvar is public
                if ( mvar->m_access == ASR::accessType::Public || to_submodule ) {
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
                }
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
                current_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(es));
            } else if( ASR::is_a<ASR::StructType_t>(*item.second) ) {
                ASR::StructType_t *mv = ASR::down_cast<ASR::StructType_t>(item.second);
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
            } else if (ASR::is_a<ASR::Requirement_t>(*item.second)) {
                ASR::Requirement_t *req = ASR::down_cast<ASR::Requirement_t>(item.second);
                Str name;
                name.from_str(al, item.first);
                char *cname = name.c_str(al);
                ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                    al, req->base.base.loc,
                    current_scope,
                    cname,
                    (ASR::symbol_t*)req,
                    m->m_name, nullptr, 0, req->m_name,
                    dflt_access
                );
                current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
            } else if (ASR::is_a<ASR::Template_t>(*item.second)) {
                ASR::Template_t *temp = ASR::down_cast<ASR::Template_t>(item.second);
                Str name;
                name.from_str(al, item.first);
                char *cname = name.c_str(al);
                ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                    al, temp->base.base.loc,
                    current_scope,
                    cname,
                    (ASR::symbol_t*)temp,
                    m->m_name, nullptr, 0, temp->m_name,
                    dflt_access
                );
                current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
            } else {
                return item.first;
            }
        }
        return "";
    }

    template <typename T>
    void process_generic_proc_custom_op(std::string& local_sym, ASR::symbol_t *t,
        std::queue<std::pair<std::string, std::string>>& to_be_imported_later,
        const Location& loc, ASR::Module_t *m,
        ASR::asr_t* (*constructor) (Allocator&, const Location&, SymbolTable*,
        char*, ASR::symbol_t**, size_t, ASR::accessType), T* /*ptr*/) {
        if (current_scope->get_symbol(local_sym) != nullptr) {
            ASR::symbol_t* gp_sym = current_scope->get_symbol(local_sym);
            if( ASR::is_a<ASR::ExternalSymbol_t>(*gp_sym) ) {
                gp_sym = ASRUtils::symbol_get_past_external(gp_sym);
                LCOMPILERS_ASSERT(ASR::is_a<T>(*gp_sym));
                T* gp = ASR::down_cast<T>(gp_sym);
                T* gp_ext = ASR::down_cast<T>(t);
                Vec<ASR::symbol_t*> gp_procs;
                gp_procs.reserve(al, gp->n_procs + gp_ext->n_procs);
                for( size_t i = 0; i < gp->n_procs; i++ ) {
                    std::string gp_proc_name = ASRUtils::symbol_name(gp->m_procs[i]);
                    ASR::symbol_t* m_proc = current_scope->resolve_symbol(
                        gp_proc_name);
                    if( m_proc == nullptr ) {
                        std::string local_sym_ = gp_proc_name + "@" + local_sym;
                        m_proc = current_scope->resolve_symbol(local_sym_);
                        if( m_proc == nullptr ) {
                            ASR::Module_t* m_ = ASRUtils::get_sym_module(gp->m_procs[i]);
                            std::string m__name = std::string(m_->m_name);
                            import_symbols_util(m_, m__name, gp_proc_name, local_sym_,
                                                to_be_imported_later, loc);
                            m_proc = current_scope->resolve_symbol(local_sym_);
                        }
                    }
                    LCOMPILERS_ASSERT(m_proc != nullptr);
                    if( !ASRUtils::present(gp_procs, m_proc) ) {
                        gp_procs.push_back(al, m_proc);
                    }
                }
                for( size_t i = 0; i < gp_ext->n_procs; i++ ) {
                    std::string gp_ext_proc_name = ASRUtils::symbol_name(gp_ext->m_procs[i]);
                    ASR::symbol_t* m_proc = current_scope->resolve_symbol(
                        gp_ext_proc_name);
                    if( m_proc == nullptr ) {
                        std::string local_sym_ = gp_ext_proc_name + "@" + local_sym;
                        m_proc = current_scope->resolve_symbol(local_sym_);
                        if( m_proc == nullptr ) {
                            ASR::Module_t* m_ = ASRUtils::get_sym_module(gp_ext->m_procs[i]);
                            std::string m__name = std::string(m_->m_name);
                            import_symbols_util(m_, m__name, gp_ext_proc_name,
                                                local_sym_, to_be_imported_later, loc);
                            m_proc = current_scope->resolve_symbol(local_sym_);
                        }
                    }
                    LCOMPILERS_ASSERT(m_proc != nullptr);
                    if( !ASRUtils::present(gp_procs, m_proc) ) {
                        gp_procs.push_back(al, m_proc);
                    }
                }
                ASR::asr_t *ep = constructor(
                    al, t->base.loc, current_scope, s2c(al, local_sym),
                    gp_procs.p, gp_procs.size(), dflt_access);
                current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
            } else {
                LCOMPILERS_ASSERT(ASR::is_a<T>(*gp_sym));
                T* gp = ASR::down_cast<T>(gp_sym);
                T* gp_ext = ASR::down_cast<T>(t);
                Vec<ASR::symbol_t*> gp_procs;
                gp_procs.reserve(al, gp->n_procs + gp_ext->n_procs);
                for( size_t i = 0; i < gp->n_procs; i++ ) {
                    gp_procs.push_back(al, gp->m_procs[i]);
                }
                for( size_t i = 0; i < gp_ext->n_procs; i++ ) {
                    std::string gp_ext_proc_name = ASRUtils::symbol_name(gp_ext->m_procs[i]);
                    ASR::symbol_t* m_proc = current_scope->resolve_symbol(
                        gp_ext_proc_name);
                    if( m_proc == nullptr ) {
                        std::string local_sym_ = "@" + gp_ext_proc_name + "@";
                        m_proc = current_scope->resolve_symbol(local_sym_);
                        if( m_proc == nullptr ) {
                            ASR::Module_t* m_ = ASRUtils::get_sym_module(gp_ext->m_procs[i]);
                            std::string m__name = std::string(m_->m_name);
                            import_symbols_util(m_, m__name, gp_ext_proc_name,
                                                local_sym_, to_be_imported_later, loc);
                            m_proc = current_scope->resolve_symbol(local_sym_);
                        }
                    }
                    LCOMPILERS_ASSERT(m_proc != nullptr);
                    if( !ASRUtils::present(gp_procs, m_proc) ) {
                        gp_procs.push_back(al, m_proc);
                    }
                    gp_procs.push_back(al, m_proc);
                }
                gp->m_procs = gp_procs.p;
                gp->n_procs = gp_procs.size();
            }
        } else {
            T* gp_ext = ASR::down_cast<T>(t);
            Vec<ASR::symbol_t*> gp_procs;
            gp_procs.reserve(al, gp_ext->n_procs);
            bool are_all_present = true;
            for( size_t i = 0; i < gp_ext->n_procs; i++ ) {
                ASR::symbol_t* m_proc = current_scope->resolve_symbol(
                    ASRUtils::symbol_name(gp_ext->m_procs[i]));
                if( m_proc == nullptr ) {
                    are_all_present = false;
                    std::string proc_name = ASRUtils::symbol_name(gp_ext->m_procs[i]);
                    to_be_imported_later.push(std::make_pair(proc_name, proc_name + "@" + local_sym));
                }
                gp_procs.push_back(al, m_proc);
            }
            ASR::asr_t *ep = nullptr;
            if( are_all_present ) {
                ep = constructor(
                    al, t->base.loc, current_scope, s2c(al, local_sym),
                    gp_procs.p, gp_procs.size(), dflt_access);
            } else {
                ep = ASR::make_ExternalSymbol_t(al, t->base.loc,
                    current_scope, s2c(al, local_sym), t,
                    m->m_name, nullptr, 0, gp_ext->m_name, dflt_access);
            }
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
        }
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
                throw SemanticError("Subroutine already defined " + local_sym,
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
            process_generic_proc_custom_op<ASR::GenericProcedure_t>(local_sym, t,
                to_be_imported_later, loc, m, &ASR::make_GenericProcedure_t, nullptr);
        } else if (ASR::is_a<ASR::CustomOperator_t>(*t)) {
            process_generic_proc_custom_op<ASR::CustomOperator_t>(local_sym, t,
                to_be_imported_later, loc, m, &ASR::make_CustomOperator_t, nullptr);
        } else if (ASR::is_a<ASR::Function_t>(*t)) {
            bool is_already_defined = false;
            ASR::symbol_t* imported_func_sym = current_scope->get_symbol(local_sym);
            if (imported_func_sym != nullptr) {
                ASR::ExternalSymbol_t* ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(imported_func_sym);
                if( ext_sym->m_external != t ) {
                    is_already_defined = true;
                }
            }
            if( is_already_defined ) {
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
            current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(fn));
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
            if (mv->m_access == ASR::accessType::Private) {
                throw SemanticError("Private variable `" + local_sym + "` cannot be imported", loc);
            }
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, mv->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)mv,
                m->m_name, nullptr, 0, mv->m_name,
                dflt_access
                );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(v));
        } else if( ASR::is_a<ASR::StructType_t>(*t) ) {
            ASR::symbol_t* imported_struct_type = current_scope->get_symbol(local_sym);
            ASR::StructType_t *mv = ASR::down_cast<ASR::StructType_t>(t);
            if (imported_struct_type != nullptr) {
                imported_struct_type = ASRUtils::symbol_get_past_external(imported_struct_type);
                if( imported_struct_type == t ) {
                    return ;
                }
                throw SemanticError("Derived type " + local_sym + " already defined.", loc);
            }
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
        } else if (ASR::is_a<ASR::Requirement_t>(*t)) {
            ASR::Requirement_t *mreq = ASR::down_cast<ASR::Requirement_t>(t);
            ASR::asr_t *req = ASR::make_ExternalSymbol_t(
                al, mreq->base.base.loc,
                current_scope,
                s2c(al, local_sym),
                (ASR::symbol_t*) mreq,
                m->m_name, nullptr, 0, mreq->m_name,
                dflt_access);
            current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(req));
        } else if (ASR::is_a<ASR::Template_t>(*t)) {
            ASR::Template_t *mtemp = ASR::down_cast<ASR::Template_t>(t);
            ASR::asr_t *temp = ASR::make_ExternalSymbol_t(
                al, mtemp->base.base.loc,
                current_scope,
                s2c(al, local_sym),
                (ASR::symbol_t*) mtemp,
                m->m_name, nullptr, 0, mtemp->m_name,
                dflt_access);
            current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(temp));
        } else if (ASR::is_a<ASR::ExternalSymbol_t>(*t)) {
            ASR::ExternalSymbol_t* ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(t);
            ASR::asr_t* temp = ASR::make_ExternalSymbol_t(
                al, ext_sym->base.base.loc,
                current_scope,
                s2c(al, local_sym),
                ext_sym->m_external,
                ext_sym->m_module_name,
                nullptr, 0, ext_sym->m_original_name,
                dflt_access);
            current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(temp));
            if( ASR::is_a<ASR::GenericProcedure_t>(*ext_sym->m_external) ) {
                process_generic_proc_custom_op<ASR::GenericProcedure_t>(local_sym,
                    ext_sym->m_external, to_be_imported_later, loc, m,
                    &ASR::make_GenericProcedure_t, nullptr);
            } else if( ASR::is_a<ASR::CustomOperator_t>(*ext_sym->m_external) ) {
                process_generic_proc_custom_op<ASR::CustomOperator_t>(local_sym,
                    ext_sym->m_external, to_be_imported_later, loc, m,
                    &ASR::make_CustomOperator_t, nullptr);
            }
        } else {
            throw LCompilersException("Only Subroutines, Functions, Variables and Derived supported in 'use', found: " +
                std::to_string(t->type) + ", name is: " + std::string(ASRUtils::symbol_name(t)));
        }
    }

    void visit_Use(const AST::Use_t &x) {
        std::string msym = to_lower(x.m_module);
        if (msym == "ieee_arithmetic") {
            msym = "lfortran_intrinsic_" + msym;
        }
        Str msym_c; msym_c.from_str_view(msym);
        char *msym_cc = msym_c.c_str(al);
        current_module_dependencies.push_back(al, msym_cc);

        ASR::symbol_t *t = current_scope->resolve_symbol(msym);
        if (!t) {
            SymbolTable *tu_symtab = current_scope;
            while (tu_symtab->parent != nullptr) {
                tu_symtab = tu_symtab->parent;
            }
            t = (ASR::symbol_t*)(ASRUtils::load_module(al, tu_symtab,
                msym, x.base.base.loc, false, compiler_options.po, true,
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
                    case AST::use_symbolType::DefinedOperator: {
                        remote_sym = AST::down_cast<AST::DefinedOperator_t>(
                            x.m_symbols[i])->m_opName;
                        break;
                    }
                    case AST::use_symbolType::UseWrite: {
                        remote_sym = AST::down_cast<AST::UseWrite_t>(
                            x.m_symbols[i])->m_id;
                        break;
                    }
                    case AST::use_symbolType::UseRead: {
                        remote_sym = AST::down_cast<AST::UseRead_t>(
                            x.m_symbols[i])->m_id;
                        break;
                    }
                    default:
                        throw SemanticError("Symbol with use not supported yet " + std::to_string(x.m_symbols[i]->type), x.base.base.loc);
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

    void visit_GenericAssignment(const AST::GenericAssignment_t& x) {
        std::string generic_name = "~assign";
        for( size_t i = 0; i < x.n_names; i++ ) {
            std::string x_m_name = std::string(x.m_names[i]);
            generic_class_procedures[dt_name][generic_name].push_back(to_lower(x_m_name));
        }
    }

    void visit_GenericOperator(const AST::GenericOperator_t &x) {
        std::string generic_name = intrinsic2str[x.m_op];
        for( size_t i = 0; i < x.n_names; i++ ) {
            std::string x_m_name = std::string(x.m_names[i]);
            generic_class_procedures[dt_name][generic_name].push_back(
                to_lower(x_m_name));
        }
    }

    void visit_GenericDefinedOperator(const AST::GenericDefinedOperator_t &x) {
        std::string generic_name = "~def_op~" + std::string(x.m_optype);
        for( size_t i = 0; i < x.n_names; i++ ) {
            std::string x_m_name = std::string(x.m_names[i]);
            generic_class_procedures[dt_name][generic_name].push_back(
                to_lower(x_m_name));
        }
    }

    void visit_Requirement(const AST::Requirement_t &x) {
        is_requirement = true;

        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        SetChar args;
        args.reserve(al, x.n_namelist);
        for (size_t i=0; i<x.n_namelist; i++) {
            std::string arg = to_lower(x.m_namelist[i]);
            args.push_back(al, s2c(al, arg));
            current_procedure_args.push_back(arg);
        }

        std::map<AST::intrinsicopType, std::vector<std::string>> requirement_op_procs;
        for (auto &proc: overloaded_op_procs) {
            requirement_op_procs[proc.first] = proc.second;
        }
        overloaded_op_procs.clear();

        Vec<ASR::require_instantiation_t*> reqs;
        reqs.reserve(al, x.n_decl);
        for (size_t i=0; i<x.n_decl; i++) {
            if (AST::is_a<AST::Require_t>(*x.m_decl[i])) {
                AST::Require_t *r = AST::down_cast<AST::Require_t>(x.m_decl[i]);
                for (size_t i=0; i<r->n_reqs; i++) {
                    visit_unit_require(*r->m_reqs[i]);
                    reqs.push_back(al, ASR::down_cast<ASR::require_instantiation_t>(tmp));
                    tmp = nullptr;
                }
            } else {
                this->visit_unit_decl2(*x.m_decl[i]);
            }
        }
        for (size_t i=0; i<x.n_funcs; i++) {
            this->visit_program_unit(*x.m_funcs[i]);
        }

        for (size_t i=0; i<x.n_namelist; i++) {
            std::string arg = to_lower(x.m_namelist[i]);
            if (!current_scope->get_symbol(arg)) {
                diag.add(Diagnostic(
                    "Parameter " + arg + " is unused in " + x.m_name,
                    Level::Warning, Stage::Semantic, {
                        Label("", {x.base.base.loc})
                    }
                ));
            }
            current_procedure_args.push_back(arg);
        }

        for (auto &item: current_scope->get_scope()) {
            bool defined = false;
            std::string sym = item.first;
            for (size_t i=0; i<x.n_namelist; i++) {
                std::string arg = to_lower(x.m_namelist[i]);
                if (sym.compare(arg) == 0) {
                    defined = true;
                }
            }
            if (!defined) {
                throw SemanticError("Symbol " + sym + " is not declared in "
                    + to_lower(x.m_name) + "'s parameters", x.base.base.loc);
            }
        }

        add_overloaded_procedures();
        for (auto &proc: requirement_op_procs) {
            overloaded_op_procs[proc.first] = proc.second;
        }

        ASR::asr_t *req = ASR::make_Requirement_t(al, x.base.base.loc,
            current_scope, s2c(al, to_lower(x.m_name)), args.p, args.size(),
            reqs.p, reqs.size());

        parent_scope->add_symbol(to_lower(x.m_name), ASR::down_cast<ASR::symbol_t>(req));

        current_scope = parent_scope;
        current_procedure_args.clear();
        is_requirement = false;
    }

    void visit_Require(const AST::Require_t &x) {
        for (size_t i=0; i<x.n_reqs; i++) {
            visit_unit_require(*x.m_reqs[i]);
        }
    }

    void visit_UnitRequire(const AST::UnitRequire_t &x) {
        std::string require_name = to_lower(x.m_name);
        ASR::symbol_t *req0 = ASRUtils::symbol_get_past_external(
            current_scope->resolve_symbol(require_name));

        if (!req0 || !ASR::is_a<ASR::Requirement_t>(*req0)) {
            throw SemanticError("No requirement '" + require_name+ "' is defined",
                x.base.base.loc);
        }

        ASR::Requirement_t *req = ASR::down_cast<ASR::Requirement_t>(req0);

        if (x.n_namelist != req->n_args) {
            throw SemanticError("The number of parameters passed to '" +
                require_name + "' is not correct", x.base.base.loc);
        }

        std::map<std::string, ASR::ttype_t*> type_subs;

        SetChar args;
        args.reserve(al, x.n_namelist);

        for (size_t i=0; i<x.n_namelist; i++) {
            AST::decl_attribute_t *attr = x.m_namelist[i];

            std::string req_param = req->m_args[i];
            std::string req_arg = "";

            if (AST::is_a<AST::AttrNamelist_t>(*attr)) {
                AST::AttrNamelist_t *attr_name = AST::down_cast<AST::AttrNamelist_t>(attr);
                req_arg = to_lower(attr_name->m_name);
                if (std::find(current_procedure_args.begin(),
                        current_procedure_args.end(),
                        req_arg) == current_procedure_args.end()
                        && !current_scope->get_symbol(req_arg)) {
                    throw SemanticError("Parameter '" + req_arg + "' was not declared", x.base.base.loc);
                }
            } else if (AST::is_a<AST::AttrType_t>(*attr)) {
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 0);
                ASR::symbol_t *type_declaration;
                ASR::ttype_t *ttype = determine_type(attr->base.loc, req_param,
                    attr, false, false, dims, type_declaration, current_procedure_abi_type);

                req_arg = ASRUtils::type_to_str(ttype);
                type_subs[req_param] = ttype;
            } else {
                throw SemanticError("Unsupported decl_attribute for require statements.", x.m_namelist[i]->base.loc);
            }

            ASR::symbol_t *param_sym = (req->m_symtab)->get_symbol(req_param);
            rename_symbol(al, type_subs, current_scope, req_arg, param_sym);
            context_map[req_param] = req_arg;
            args.push_back(al, s2c(al, req_arg));
        }

        // adding custom operators
        for (auto &item: req->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::CustomOperator_t>(*item.second)) {
                ASR::CustomOperator_t *c_op = ASR::down_cast<ASR::CustomOperator_t>(item.second);

                // may not need to add new custom operators if another requires already got an interface
                Vec<ASR::symbol_t*> symbols;
                symbols.reserve(al, c_op->n_procs);
                for (size_t i=0; i<c_op->n_procs; i++) {
                    ASR::symbol_t *proc = c_op->m_procs[i];
                    std::string new_proc_name = context_map[ASRUtils::symbol_name(proc)];
                    proc = current_scope->resolve_symbol(new_proc_name);
                    symbols.push_back(al, proc);
                }

                ASR::symbol_t *new_c_op = ASR::down_cast<ASR::symbol_t>(ASR::make_CustomOperator_t(
                    al, c_op->base.base.loc, current_scope,
                    s2c(al, c_op->m_name), symbols.p, symbols.size(), c_op->m_access));
                current_scope->add_symbol(c_op->m_name, new_c_op);
            }
        }

        tmp = ASR::make_Require_t(al, x.base.base.loc, s2c(al, require_name),
            args.p, args.size());

        context_map.clear();
    }

    void visit_Template(const AST::Template_t &x){
        is_template = true;

        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        for (size_t i=0; i<x.n_namelist; i++) {
            current_procedure_args.push_back(to_lower(x.m_namelist[i]));
        }

        std::map<AST::intrinsicopType, std::vector<std::string>> ext_overloaded_op_procs;
        for (auto &proc: overloaded_op_procs) {
            ext_overloaded_op_procs[proc.first] = proc.second;
        }
        overloaded_op_procs.clear();

        Vec<ASR::require_instantiation_t*> reqs;
        reqs.reserve(al, x.n_decl);
        // For interface and type parameters (derived type)
        for (size_t i=0; i<x.n_decl; i++) {
            if (AST::is_a<AST::Require_t>(*x.m_decl[i])) {
                AST::Require_t *r = AST::down_cast<AST::Require_t>(x.m_decl[i]);
                for (size_t i=0; i<r->n_reqs; i++) {
                    visit_unit_require(*r->m_reqs[i]);
                    reqs.push_back(al, ASR::down_cast<ASR::require_instantiation_t>(tmp));
                    tmp = nullptr;
                }
            } else {
                this->visit_unit_decl2(*x.m_decl[i]);
            }
        }

        for (size_t i=0; i<x.n_contains; i++) {
            this->visit_program_unit(*x.m_contains[i]);
        }

        SetChar args;
        args.reserve(al, x.n_namelist);
        for (size_t i=0; i<x.n_namelist; i++) {
            std::string arg = to_lower(x.m_namelist[i]);
            args.push_back(al, s2c(al, arg));
            ASR::symbol_t *s = current_scope->get_symbol(arg);
            if (!s) {
                throw SemanticError("Template argument " + arg  + " has not been"
                    " declared in template specification.", x.base.base.loc);
            }
        }

        add_overloaded_procedures();
        add_class_procedures();

        for (auto &proc: ext_overloaded_op_procs) {
            overloaded_op_procs[proc.first] = proc.second;
        }

        ASR::asr_t *temp = ASR::make_Template_t(al, x.base.base.loc,
            current_scope, x.m_name, args.p, args.size(), reqs.p, reqs.size());

        parent_scope->add_symbol(x.m_name, ASR::down_cast<ASR::symbol_t>(temp));
        current_scope = parent_scope;

        // needs to rebuild the context prior to visiting template
        class_procedures.clear();

        is_template = false;
    }

    void visit_Instantiate(const AST::Instantiate_t &x) {
        std::string template_name = x.m_name;

        // check if the template exists
        ASR::symbol_t *sym0 = ASRUtils::symbol_get_past_external(
            current_scope->resolve_symbol(template_name));
        if (!sym0) {
            throw SemanticError("Use of an unspecified template '" + template_name
                + "'", x.base.base.loc);
        }

        // check if the symbol is a template
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(sym0);
        if (!ASR::is_a<ASR::Template_t>(*sym)) {
            throw SemanticError("Cannot instantiate a non-template '" + template_name
                + "'", x.base.base.loc);
        }

        ASR::Template_t* temp = ASR::down_cast<ASR::Template_t>(sym);

        // check for number of template arguments
        if (temp->n_args != x.n_args) {
            throw SemanticError("Number of template arguments don't match", x.base.base.loc);
        }

        std::map<std::string, ASR::ttype_t*> type_subs;
        std::map<std::string, ASR::symbol_t*> symbol_subs;

        for (size_t i=0; i<x.n_args; i++) {
            std::string param = temp->m_args[i];
            ASR::symbol_t *param_sym = temp->m_symtab->get_symbol(param);
            if (AST::is_a<AST::AttrType_t>(*x.m_args[i])) {
                // Handling types as instantiate's arguments
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 0);
                ASR::symbol_t *type_declaration;
                ASR::ttype_t *arg_type = determine_type(x.m_args[i]->base.loc, param,
                    x.m_args[i], false, false, dims, type_declaration, current_procedure_abi_type);
                ASR::ttype_t *param_type = ASRUtils::symbol_type(param_sym);
                if (!ASRUtils::is_type_parameter(*param_type)) {
                    throw SemanticError("The type " + ASRUtils::type_to_str(arg_type) +
                        " cannot be applied to non-type parameter " + param, x.m_args[i]->base.loc);
                }
                type_subs[param] = arg_type;
            } else if (AST::is_a<AST::AttrNamelist_t>(*x.m_args[i])) {
                AST::AttrNamelist_t *attr_name = AST::down_cast<AST::AttrNamelist_t>(x.m_args[i]);
                std::string arg = to_lower(attr_name->m_name);
                if (ASR::is_a<ASR::Function_t>(*param_sym)) {
                    // Handling functions passed as instantiate's arguments
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(param_sym);
                    ASR::symbol_t *f_arg0 = current_scope->resolve_symbol(arg);
                    if (!f_arg0) {
                        throw SemanticError("The function argument " + arg + " is not found",
                            x.m_args[i]->base.loc);
                    }
                    ASR::symbol_t *f_arg = ASRUtils::symbol_get_past_external(f_arg0);
                    if (!ASR::is_a<ASR::Function_t>(*f_arg)) {
                        throw SemanticError(
                            "The argument for " + param + " must be a function",
                            x.m_args[i]->base.loc);
                    }
                    check_restriction(type_subs,
                        symbol_subs, f, f_arg0, x.m_args[i]->base.loc, diag,
                        []() { throw SemanticAbort(); });
                } else {
                    ASR::ttype_t *param_type = ASRUtils::symbol_type(param_sym);
                    if (ASRUtils::is_type_parameter(*param_type)) {
                        // Handling types passed as instantiate's arguments
                        ASR::symbol_t *arg_sym0 = current_scope->resolve_symbol(arg);
                        ASR::symbol_t *arg_sym = ASRUtils::symbol_get_past_external(arg_sym0);
                        ASR::ttype_t *arg_type = nullptr;
                        if (ASR::is_a<ASR::StructType_t>(*arg_sym)) {
                            arg_type = ASRUtils::TYPE(ASR::make_Struct_t(al, x.m_args[i]->base.loc, arg_sym0));
                        } else {
                            arg_type = ASRUtils::symbol_type(arg_sym);
                        }
                        type_subs[param] = ASRUtils::duplicate_type(al, arg_type);
                    } else {
                        // Handling local variables passed as instantiate's arguments
                        ASR::symbol_t *arg_sym = current_scope->resolve_symbol(arg);
                        ASR::ttype_t *arg_type = ASRUtils::symbol_type(arg_sym);
                        if (!ASRUtils::check_equal_type(arg_type, param_type)) {
                            throw SemanticError("The type of " + arg + " does not match the type of " + param,
                                x.m_args[i]->base.loc);
                        }
                        symbol_subs[param] = arg_sym;
                    }
                }
            } else if (AST::is_a<AST::AttrIntrinsicOperator_t>(*x.m_args[i])) {
                AST::AttrIntrinsicOperator_t *intrinsic_op
                    = AST::down_cast<AST::AttrIntrinsicOperator_t>(x.m_args[i]);
                ASR::binopType binop = ASR::Add;
                ASR::cmpopType cmpop = ASR::Eq;
                bool is_binop = false, is_cmpop = false;
                std::string op_name;
                switch (intrinsic_op->m_op) {
                    case (AST::PLUS):
                        is_binop = true; binop = ASR::Add; op_name = "~add";
                        break;
                    case (AST::MINUS):
                        is_binop = true; binop = ASR::Sub; op_name = "~sub";
                        break;
                    case (AST::STAR):
                        is_binop = true; binop = ASR::Mul; op_name = "~mul";
                        break;
                    case (AST::DIV):
                        is_binop = true; binop = ASR::Div; op_name = "~div";
                        break;
                    case (AST::POW):
                        is_binop = true; binop = ASR::Pow; op_name = "~pow";
                        break;
                    case (AST::EQ):
                        is_cmpop = true; cmpop = ASR::Eq; op_name = "~eq";
                        break;
                    case (AST::NOTEQ):
                        is_cmpop = true; cmpop = ASR::NotEq; op_name = "~neq";
                        break;
                    case (AST::LT):
                        is_cmpop = true; cmpop = ASR::Lt; op_name = "~lt";
                        break;
                    case (AST::LTE):
                        is_cmpop = true; cmpop = ASR::LtE; op_name = "~lte";
                        break;
                    case (AST::GT):
                        is_cmpop = true; cmpop = ASR::Gt; op_name = "~gt";
                        break;
                    case (AST::GTE):
                        is_cmpop = true; cmpop = ASR::GtE; op_name = "~gte";
                        break;
                    default:
                        throw SemanticError("Unsupported binary operator", x.m_args[i]->base.loc);
                }

                bool is_overloaded;
                if (is_binop) {
                    is_overloaded = ASRUtils::is_op_overloaded(binop, op_name, current_scope, nullptr);
                } else if (is_cmpop) {
                    is_overloaded = ASRUtils::is_op_overloaded(cmpop, op_name, current_scope, nullptr);
                } else {
                    throw SemanticError("Must be binop or cmop", x.m_args[i]->base.loc);
                }

                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(param_sym);
                std::string f_name = f->m_name;
                bool found = false;
                // check if an alias is defined for the operator
                if (is_overloaded) {
                    ASR::symbol_t* sym = current_scope->resolve_symbol(op_name);
                    ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
                    ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
                    for (size_t i = 0; i < gen_proc->n_procs && !found; i++) {
                        ASR::symbol_t* proc = gen_proc->m_procs[i];
                        found = check_restriction(type_subs,
                            symbol_subs, f, proc, x.m_args[i]->base.loc, diag,
                            []() { throw SemanticAbort(); }, false);
                    }
                }

                // if not found, then try to build a function for intrinsic operator
                if (!found) {
                    if (f->n_args != 2) {
                        throw SemanticError("The restriction " + f_name
                            + " does not have 2 parameters", x.base.base.loc);
                    }

                    ASR::ttype_t *left_type = ASRUtils::subs_expr_type(type_subs, f->m_args[0]);
                    ASR::ttype_t *right_type = ASRUtils::subs_expr_type(type_subs, f->m_args[1]);
                    ASR::ttype_t *ftype = ASRUtils::subs_expr_type(type_subs, f->m_return_var);

                    SymbolTable *parent_scope = current_scope;
                    current_scope = al.make_new<SymbolTable>(parent_scope);
                    Vec<ASR::expr_t*> args;
                    args.reserve(al, 2);
                    for (size_t i=0; i<2; i++) {
                        std::string var_name = "arg" + std::to_string(i);
                        ASR::asr_t *v = ASR::make_Variable_t(al, x.base.base.loc, current_scope,
                            s2c(al, var_name), nullptr, 0, ASR::intentType::In, nullptr,
                            nullptr, ASR::storage_typeType::Default,
                            (i == 0 ? ASRUtils::duplicate_type(al, left_type)
                                : ASRUtils::duplicate_type(al, right_type)),
                            nullptr, ASR::abiType::Source, ASR::accessType::Private,
                            ASR::presenceType::Required, false);
                        current_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(v));
                        ASR::symbol_t *var = current_scope->get_symbol(var_name);
                        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, var)));
                    }

                    //std::string func_name = op_name + "_intrinsic_" + ASRUtils::type_to_str(ltype);
                    std::string func_name = parent_scope->get_unique_name(op_name + "_intrinsic");

                    ASR::ttype_t *return_type = nullptr;
                    ASR::expr_t *value = nullptr;
                    ASR::expr_t *left = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                        current_scope->get_symbol("arg0")));
                    ASR::expr_t *right = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                        current_scope->get_symbol("arg1")));

                    ASR::expr_t **conversion_cand = &left;
                    ASR::ttype_t *source_type = left_type;
                    ASR::ttype_t *dest_type = right_type;

                    if (is_binop) {
                        ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                                     right_type, conversion_cand,
                                                                     &source_type, &dest_type);
                        ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                                               source_type, dest_type);
                        return_type = ASRUtils::duplicate_type(al, ftype);
                        value = ASRUtils::EXPR(ASRUtils::make_Binop_util(al, x.base.base.loc, binop, left, right, dest_type));
                        if (!ASRUtils::check_equal_type(dest_type, return_type)) {
                            throw SemanticError("Unapplicable types for intrinsic operator " + op_name,
                                x.base.base.loc);
                        }
                    } else {
                        return_type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                        value = ASRUtils::EXPR(ASRUtils::make_Cmpop_util(al, x.base.base.loc, cmpop, left, right, left_type));
                    }

                    ASR::asr_t *return_v = ASR::make_Variable_t(al, x.base.base.loc,
                        current_scope, s2c(al, "ret"), nullptr, 0,
                        ASR::intentType::ReturnVar, nullptr, nullptr, ASR::storage_typeType::Default,
                        return_type, nullptr, ASR::abiType::Source,
                        ASR::accessType::Private, ASR::presenceType::Required, false);
                    current_scope->add_symbol("ret", ASR::down_cast<ASR::symbol_t>(return_v));
                    ASR::expr_t *return_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                        current_scope->get_symbol("ret")));

                    Vec<ASR::stmt_t*> body;
                    body.reserve(al, 1);
                    ASR::symbol_t *return_sym = current_scope->get_symbol("ret");
                    ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, return_sym));
                    ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, target, value);
                    ASR::stmt_t *assignment = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc,
                        target, value, nullptr));
                    body.push_back(al, assignment);

                    ASR::FunctionType_t *req_type = ASR::down_cast<ASR::FunctionType_t>(f->m_function_signature);

                    ASR::asr_t *op_function = ASRUtils::make_Function_t_util(
                        al, x.base.base.loc, current_scope, s2c(al, func_name),
                        nullptr, 0, args.p, 2, body.p, 1, return_expr,
                        ASR::abiType::Source, ASR::accessType::Public,
                        ASR::deftypeType::Implementation, nullptr, req_type->m_elemental,
                        req_type->m_pure, req_type->m_module, req_type->m_inline,
                        req_type->m_static, nullptr, 0, f->m_deterministic,
                        f->m_side_effect_free, true);
                    ASR::symbol_t *op_sym = ASR::down_cast<ASR::symbol_t>(op_function);
                    parent_scope->add_symbol(func_name, op_sym);

                    Vec<ASR::symbol_t*> symbols;
                    if (parent_scope->get_symbol(op_name) != nullptr) {
                        ASR::CustomOperator_t *old_c = ASR::down_cast<ASR::CustomOperator_t>(
                            parent_scope->get_symbol(op_name));
                        symbols.reserve(al, old_c->n_procs + 1);
                        for (size_t i=0; i<old_c->n_procs; i++) {
                            symbols.push_back(al, old_c->m_procs[i]);
                        }
                    } else {
                        symbols.reserve(al, 1);
                    }
                    symbols.push_back(al, ASR::down_cast<ASR::symbol_t>(op_function));
                    ASR::asr_t *c = ASR::make_CustomOperator_t(al, x.base.base.loc,
                        parent_scope, s2c(al, op_name), symbols.p, symbols.size(), ASR::Public);
                    parent_scope->add_or_overwrite_symbol(op_name, ASR::down_cast<ASR::symbol_t>(c));

                    current_scope = parent_scope;
                    symbol_subs[f->m_name] = op_sym;
                }
            } else {
                throw SemanticError("Unsupported template argument", x.m_args[i]->base.loc);
            }
        }

        if (x.n_symbols == 0) {
            for (auto const &sym_pair: temp->m_symtab->get_scope()) {
                ASR::symbol_t *s = sym_pair.second;
                std::string s_name = ASRUtils::symbol_name(s);
                if (ASR::is_a<ASR::Function_t>(*s) && !ASRUtils::is_template_arg(sym, s_name)) {
                    instantiate_symbol(al, current_scope, type_subs, symbol_subs, s_name, s);
                }
            }
        } else {
            for (size_t i = 0; i < x.n_symbols; i++){
                AST::UseSymbol_t* use_symbol = AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i]);
                std::string generic_name = to_lower(use_symbol->m_remote_sym);
                ASR::symbol_t *s = temp->m_symtab->get_symbol(generic_name);
                if (s == nullptr) {
                    throw SemanticError("Symbol " + generic_name + " was not found", x.base.base.loc);
                }
                std::string new_sym_name = generic_name;
                if (use_symbol->m_local_rename) {
                    new_sym_name = to_lower(use_symbol->m_local_rename);
                }
                ASR::symbol_t* new_sym = instantiate_symbol(al, current_scope, type_subs, symbol_subs, new_sym_name, s);
                symbol_subs[generic_name] = new_sym;
            }
        }

        instantiate_types[x.base.base.loc.first] = type_subs;
        instantiate_symbols[x.base.base.loc.first] = symbol_subs;
    }

    // TODO: give proper location to each symbol
    ASR::symbol_t* replace_symbol(ASR::symbol_t* s, std::string name) {
        switch (s->type) {
            case ASR::symbolType::Variable: {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(s);
                ASR::ttype_t* t = ASRUtils::duplicate_type(al, v->m_type);
                ASR::dimension_t* tp_m_dims = nullptr;
                int tp_n_dims = ASRUtils::extract_dimensions_from_ttype(t, tp_m_dims);
                t = ASRUtils::type_get_past_array(t);
                if (ASR::is_a<ASR::TypeParameter_t>(*t)) {
                    ASR::TypeParameter_t* tp = ASR::down_cast<ASR::TypeParameter_t>(t);
                    context_map[tp->m_param] = name;
                    if (name.compare("integer") == 0) {
                        t = ASRUtils::TYPE(ASR::make_Integer_t(al,
                            tp->base.base.loc, compiler_options.po.default_integer_kind));
                    } else {
                        t = ASRUtils::TYPE(ASR::make_TypeParameter_t(al,
                            tp->base.base.loc, s2c(al, name)));
                    }
                    t = ASRUtils::make_Array_t_util(al, tp->base.base.loc,
                        t, tp_m_dims, tp_n_dims);
                }
                ASR::asr_t* new_v = ASR::make_Variable_t(al, v->base.base.loc,
                    current_scope, s2c(al, name), v->m_dependencies,
                    v->n_dependencies, v->m_intent,
                    v->m_symbolic_value, v->m_value, v->m_storage, t,
                    v->m_type_declaration, v->m_abi, v->m_access,
                    v->m_presence, v->m_value_attr);
                return ASR::down_cast<ASR::symbol_t>(new_v);
            }
            case ASR::symbolType::Function: {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(s);
                ASR::FunctionType_t* ftype = ASR::down_cast<ASR::FunctionType_t>(
                    f->m_function_signature);
                SymbolTable* new_scope = al.make_new<SymbolTable>(current_scope);

                Vec<ASR::expr_t*> args;
                args.reserve(al, f->n_args);
                for (size_t i=0; i<f->n_args; i++) {
                    ASR::Variable_t* param_var = ASR::down_cast<ASR::Variable_t>(
                        (ASR::down_cast<ASR::Var_t>(f->m_args[i]))->m_v);
                    ASR::ttype_t* param_type = ASRUtils::expr_type(f->m_args[i]);
                    ASR::dimension_t* tp_m_dims = nullptr;
                    int tp_n_dims = ASRUtils::extract_dimensions_from_ttype(param_type, tp_m_dims);
                    param_type = ASRUtils::type_get_past_array(param_type);
                    if (ASR::is_a<ASR::TypeParameter_t>(*param_type)) {
                        ASR::TypeParameter_t* tp = ASR::down_cast<ASR::TypeParameter_t>(param_type);
                        if (context_map.find(tp->m_param) != context_map.end()) {
                            std::string pt = context_map[tp->m_param];
                            if (pt.compare("integer") == 0) {
                                param_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
                                    tp->base.base.loc, compiler_options.po.default_integer_kind));
                            } else {
                                param_type = ASRUtils::TYPE(ASR::make_TypeParameter_t(
                                    al, tp->base.base.loc, s2c(al, context_map[tp->m_param])));
                            }
                            if( tp_n_dims > 0 ) {
                                param_type = ASRUtils::make_Array_t_util(al, tp->base.base.loc,
                                    param_type, tp_m_dims, tp_n_dims);
                            }
                        }
                    }

                    Location loc = param_var->base.base.loc;
                    std::string var_name = param_var->m_name;
                    ASR::intentType s_intent = param_var->m_intent;
                    ASR::expr_t *init_expr = nullptr;
                    ASR::expr_t *value = nullptr;
                    ASR::storage_typeType storage_type = param_var->m_storage;
                    ASR::abiType abi_type = param_var->m_abi;
                    ASR::symbol_t *type_decl = nullptr;
                    ASR::accessType s_access = param_var->m_access;
                    ASR::presenceType s_presence = param_var->m_presence;
                    bool value_attr = param_var->m_value_attr;

                    SetChar variable_dependencies_vec;
                    variable_dependencies_vec.reserve(al, 1);
                    ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, param_type);
                    ASR::asr_t *v = ASR::make_Variable_t(al, loc, new_scope,
                        s2c(al, var_name), variable_dependencies_vec.p,
                        variable_dependencies_vec.size(),
                        s_intent, init_expr, value, storage_type, param_type,
                        type_decl, abi_type, s_access, s_presence, value_attr);

                    new_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(v));
                    ASR::symbol_t* var = new_scope->get_symbol(var_name);
                    args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, f->base.base.loc, var)));
                }

                ASR::expr_t *new_return_var_ref = nullptr;
                if (f->m_return_var != nullptr) {
                    ASR::Variable_t *return_var = ASR::down_cast<ASR::Variable_t>(
                        (ASR::down_cast<ASR::Var_t>(f->m_return_var))->m_v);
                    std::string return_var_name = return_var->m_name;
                    ASR::ttype_t *return_type = ASRUtils::expr_type(f->m_return_var);
                    ASR::dimension_t* tp_m_dims = nullptr;
                    int tp_n_dims = ASRUtils::extract_dimensions_from_ttype(return_type, tp_m_dims);
                    return_type = ASRUtils::type_get_past_array(return_type);
                    if (ASR::is_a<ASR::TypeParameter_t>(*return_type)) {
                        ASR::TypeParameter_t* tp = ASR::down_cast<ASR::TypeParameter_t>(return_type);
                        if (context_map.find(tp->m_param) != context_map.end()) {
                            std::string pt = context_map[tp->m_param];
                            if (pt.compare("integer") == 0) {
                                return_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
                                    tp->base.base.loc, compiler_options.po.default_integer_kind));
                            } else {
                                return_type = ASRUtils::TYPE(ASR::make_TypeParameter_t(
                                    al, tp->base.base.loc, s2c(al, context_map[tp->m_param])));
                            }
                            if( tp_n_dims > 0 ) {
                                return_type = ASRUtils::make_Array_t_util(al, tp->base.base.loc,
                                    return_type, tp_m_dims, tp_n_dims);
                            }
                        }
                    }
                    SetChar variable_dependencies_vec;
                    variable_dependencies_vec.reserve(al, 1);
                    ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, return_type);
                    ASR::asr_t *new_return_var = ASR::make_Variable_t(al, return_var->base.base.loc,
                        new_scope, s2c(al, return_var_name),
                        variable_dependencies_vec.p,
                        variable_dependencies_vec.size(),
                        return_var->m_intent, nullptr, nullptr,
                        return_var->m_storage, return_type,
                        return_var->m_type_declaration, return_var->m_abi,
                        return_var->m_access, return_var->m_presence,
                        return_var->m_value_attr);
                    new_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(new_return_var));
                    new_return_var_ref = ASRUtils::EXPR(ASR::make_Var_t(al, f->base.base.loc,
                        new_scope->get_symbol(return_var_name)));
                }

                ASR::asr_t* new_f = ASRUtils::make_Function_t_util(al, f->base.base.loc,
                    new_scope, s2c(al, name), f->m_dependencies, f->n_dependencies, args.p,
                    args.size(), nullptr, 0, new_return_var_ref, ftype->m_abi, f->m_access,
                    ftype->m_deftype, ftype->m_bindc_name, ftype->m_elemental, ftype->m_pure,
                    ftype->m_module, ftype->m_inline, ftype->m_static,
                    ftype->m_restrictions, ftype->n_restrictions,
                    ftype->m_is_restriction, f->m_deterministic, f->m_side_effect_free);
                return ASR::down_cast<ASR::symbol_t>(new_f);
            }
            default : {
                std::string sym_name = ASRUtils::symbol_name(s);
                throw SemanticError("Symbol not found " + sym_name, s->base.loc);
            }
        }
    }

    void visit_Enum(const AST::Enum_t &x) {
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        std::string sym_name = "_nameless_enum";
        {
            int i = 1;
            while (parent_scope->get_symbol(std::to_string(i) +
                    sym_name) != nullptr) {
                i++;
            }
            sym_name = std::to_string(i) + sym_name;
        }
        Vec<char *> m_members;
        m_members.reserve(al, 4);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al,
            x.base.base.loc, compiler_options.po.default_integer_kind));

        ASR::abiType abi_type = ASR::abiType::BindC;
        if ( x.n_attr == 1 ) {
            if ( AST::is_a<AST::AttrBind_t>(*x.m_attr[0]) ) {
                AST::Bind_t *bind = AST::down_cast<AST::Bind_t>(
                    AST::down_cast<AST::AttrBind_t>(x.m_attr[0])->m_bind);
                if (bind->n_args == 1 && AST::is_a<AST::Name_t>(*bind->m_args[0])) {
                    AST::Name_t *name = AST::down_cast<AST::Name_t>(
                        bind->m_args[0]);
                    if (to_lower(std::string(name->m_id)) != "c") {
                        throw SemanticError("Unsupported language in bind()",
                            x.base.base.loc);
                    }
                } else {
                    throw SemanticError("Language name must be specified in "
                        "bind() as a plain text", x.base.base.loc);
                }
            } else {
                throw SemanticError("Unsupported attribute type in enum, "
                    "only bind() is allowed", x.base.base.loc);
            }
        } else {
            throw SemanticError("Only one attribute is allowed in enum",
                x.base.base.loc);
        }

        for ( size_t i = 0; i < x.n_items; i++ ) {
            this->visit_unit_decl2(*x.m_items[i]);
        }

        for( auto sym: current_scope->get_scope() ) {
            ASR::Variable_t* member_var = ASR::down_cast<
                ASR::Variable_t>(sym.second);
            m_members.push_back(al, member_var->m_name);
        }

        ASR::enumtypeType enum_value_type = ASR::enumtypeType::IntegerConsecutiveFromZero;
        ASRUtils::set_enum_value_type(enum_value_type, current_scope);

        tmp = ASR::make_EnumType_t(al, x.base.base.loc, current_scope,
            s2c(al, sym_name), nullptr, 0, m_members.p, m_members.n, abi_type,
            dflt_access, enum_value_type, type, nullptr);
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
    }

};

Result<ASR::asr_t*> symbol_table_visitor(Allocator &al, AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table, CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint32_t, std::map<std::string, ASR::ttype_t*>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::vector<ASR::stmt_t*> &data_structure)
{
    SymbolTableVisitor v(al, symbol_table, diagnostics, compiler_options, implicit_mapping, common_variables_hash, external_procedures_mapping,
                         instantiate_types, instantiate_symbols, entry_functions, entry_function_arguments_mapping, data_structure);
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

} // namespace LCompilers::LFortran
