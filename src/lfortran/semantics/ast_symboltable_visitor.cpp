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
    std::string dt_name;
    bool in_submodule = false;
    bool is_interface = false;
    std::string interface_name = "";
    ASR::symbol_t *current_module_sym;

    ASR::ttype_t *tmp_type;

    std::map<AST::intrinsicopType, std::string> intrinsic2str = {
        {AST::intrinsicopType::STAR, "~mul"},
        {AST::intrinsicopType::PLUS, "~add"},
        {AST::intrinsicopType::EQ, "~eq"},
        {AST::intrinsicopType::NOTEQ, "~noteq"},
        {AST::intrinsicopType::LT, "~lt"},
        {AST::intrinsicopType::LTE, "~lte"},
        {AST::intrinsicopType::GT, "~gt"},
        {AST::intrinsicopType::GTE, "~gte"},
        {AST::intrinsicopType::MINUS, "~sub"}
    };

    SymbolTableVisitor(Allocator &al, SymbolTable *symbol_table,
        diag::Diagnostics &diagnostics, CompilerOptions &compiler_options, std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash, std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint32_t, std::map<std::string, ASR::ttype_t*>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols)
      : CommonVisitor(al, symbol_table, diagnostics, compiler_options, implicit_mapping, common_variables_hash, external_procedures_mapping,
                      instantiate_types, instantiate_symbols) {}

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
                            if( data->sym_type == (int64_t) ASR::symbolType::Function ) {
                                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
                                    ASR::down_cast<ASR::symbol_t>(data->scope->asr_owner));
                                ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(*func);
                                if( func_type->m_return_var_type ) {
                                    ASRUtils::ReplaceWithFunctionParamVisitor replacer(al, func->m_args, func->n_args);
                                    func_type->m_return_var_type = replacer.replace_args_with_FunctionParam(data->type);
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
            implicit_dictionary[std::string(1, ch)] = ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4));
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
                        }
                    } else {
                        throw SemanticError("Only one kind item supported for now", x.base.base.loc);
                    }
                }
                switch (ast_type) {
                    case (AST::decl_typeType::TypeInteger) : {
                        type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, a_kind));
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
                        type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4));
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
            LCompilers::PassOptions pass_options;
            pass_options.runtime_library_dir = compiler_options.runtime_library_dir;
            pass_options.mod_files_dir = compiler_options.mod_files_dir;
            pass_options.include_dirs = compiler_options.include_dirs;

            ASR::symbol_t* submod_parent = (ASR::symbol_t*)(ASRUtils::load_module(al, global_scope,
                                                parent_name, x.base.base.loc, false,
                                                pass_options, true,
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

        ASR::accessType s_access = dflt_access;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
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
            visit_unit_decl2(*x.m_decl[i]);
            is_Function = false;
        }
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
            ASR::symbol_t *f1 = ASRUtils::symbol_get_past_external(
                parent_scope->get_symbol(sym_name));
            ASR::Function_t *f2 = nullptr;
            if( ASR::is_a<ASR::Function_t>(*f1) ) {
                f2 = ASR::down_cast<ASR::Function_t>(f1);
            }
            if ((f1->type == ASR::symbolType::ExternalSymbol && in_submodule) ||
                ASRUtils::get_FunctionType(f2)->m_abi == ASR::abiType::Interactive ||
                ASRUtils::get_FunctionType(f2)->m_deftype == ASR::deftypeType::Interface) {
                // Previous declaration will be shadowed
                parent_scope->erase_symbol(sym_name);
            } else {
                throw SemanticError("Subroutine already defined", tmp->loc);
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
            is_pure, is_module, false, false, false,
            nullptr, 0,
            is_requirement, false, false);
        handle_save();
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
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

        // populate the external_procedures_mapping
        uint64_t hash = get_hash(tmp);
        external_procedures_mapping[hash] = external_procedures;

        current_function_dependencies = current_function_dependencies_copy;
        in_Subroutine = false;
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
            is_Function = true;
            visit_unit_decl2(*x.m_decl[i]);
            is_Function = false;
        }
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
                    }
                } else {
                    throw SemanticError("Only one kind item supported for now", x.base.base.loc);
                }
            }
            switch (return_type->m_type) {
                case (AST::decl_typeType::TypeInteger) : {
                    type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, a_kind));
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
                    type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4));
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
        std::string sym_name = to_lower(x.m_name);
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
            ASR::Function_t *f2 = nullptr;
            if( f1->type == ASR::symbolType::Function ) {
                f2 = ASR::down_cast<ASR::Function_t>(f1);
            }
            if ((f1->type == ASR::symbolType::ExternalSymbol && in_submodule) ||
                ASRUtils::get_FunctionType(f2)->m_abi == ASR::abiType::Interactive) {
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
            bindc_name, is_elemental, false, false, false, false,
            nullptr, 0, is_requirement, false, false);
        handle_save();
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
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

        // populate the external_procedures_mapping
        uint64_t hash = get_hash(tmp);
        external_procedures_mapping[hash] = external_procedures;

        current_function_dependencies = current_function_dependencies_copy;
        in_Subroutine = false;
        mark_common_blocks_as_declared();
    }

    void visit_Declaration(const AST::Declaration_t& x) {
        visit_DeclarationUtil(x);
    }

    void visit_DerivedType(const AST::DerivedType_t &x) {
        if (is_requirement) {
            ASR::asr_t *tp = ASR::make_TypeParameter_t(
                al, x.base.base.loc, s2c(al, to_lower(x.m_name)));
            tmp = ASR::make_Variable_t(al, x.base.base.loc, current_scope,
                s2c(al, to_lower(x.m_name)), nullptr, 0, ASRUtils::intent_in,
                nullptr, nullptr, ASR::storage_typeType::Default,
                ASRUtils::TYPE(tp), nullptr, ASR::abiType::Source,
                dflt_access, ASR::presenceType::Required, false);
            current_scope->add_symbol(to_lower(x.m_name), ASR::down_cast<ASR::symbol_t>(tmp));
            return;
        }
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        data_member_names.reserve(al, 0);
        is_derived_type = true;
        bool is_abstract = false;
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
                case AST::decl_attributeType::SimpleAttribute: {
                    AST::SimpleAttribute_t* simple_attr =
                        AST::down_cast<AST::SimpleAttribute_t>(x.m_attrtype[i]);
                    is_abstract = simple_attr->m_attr == AST::simple_attributeType::AttrAbstract;
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
                bool is_deferred = (cdf.count(proc.first) && cdf[proc.first].count(pname.first) && cdf[proc.first][pname.first].count("deferred"));
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
                    is_deferred);
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
                // check if m_access of mvar is public
                if ( mvar->m_access == ASR::accessType::Public ) {
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
                ASR::symbol_t* gp_sym = current_scope->get_symbol(local_sym);
                if( ASR::is_a<ASR::ExternalSymbol_t>(*gp_sym) ) {
                    gp_sym = ASRUtils::symbol_get_past_external(gp_sym);
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*gp_sym));
                    ASR::GenericProcedure_t* gp = ASR::down_cast<ASR::GenericProcedure_t>(gp_sym);
                    ASR::GenericProcedure_t* gp_ext = ASR::down_cast<ASR::GenericProcedure_t>(t);
                    Vec<ASR::symbol_t*> gp_procs;
                    gp_procs.reserve(al, gp->n_procs + gp_ext->n_procs);
                    for( size_t i = 0; i < gp->n_procs; i++ ) {
                        std::string gp_proc_name = ASRUtils::symbol_name(gp->m_procs[i]);
                        ASR::symbol_t* m_proc = current_scope->resolve_symbol(
                            gp_proc_name);
                        if( m_proc == nullptr ) {
                            std::string local_sym_ = "@" + gp_proc_name + "@";
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
                    }
                    ASR::asr_t *ep = ASR::make_GenericProcedure_t(
                        al, t->base.loc, current_scope, s2c(al, local_sym),
                        gp_procs.p, gp_procs.size(), dflt_access);
                    current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*gp_sym));
                    ASR::GenericProcedure_t* gp = ASR::down_cast<ASR::GenericProcedure_t>(gp_sym);
                    ASR::GenericProcedure_t* gp_ext = ASR::down_cast<ASR::GenericProcedure_t>(t);
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
                ASR::GenericProcedure_t *gp_ext = ASR::down_cast<ASR::GenericProcedure_t>(t);
                Vec<ASR::symbol_t*> gp_procs;
                gp_procs.reserve(al, gp_ext->n_procs);
                bool are_all_present = true;
                for( size_t i = 0; i < gp_ext->n_procs; i++ ) {
                    ASR::symbol_t* m_proc = current_scope->resolve_symbol(
                        ASRUtils::symbol_name(gp_ext->m_procs[i]));
                    if( m_proc == nullptr ) {
                        are_all_present = false;
                        break;
                    }
                    gp_procs.push_back(al, m_proc);
                }
                ASR::asr_t *ep = nullptr;
                if( are_all_present ) {
                    ep = ASR::make_GenericProcedure_t(
                        al, t->base.loc, current_scope, s2c(al, local_sym),
                        gp_procs.p, gp_procs.size(), dflt_access);
                } else {
                    ep = ASR::make_ExternalSymbol_t(al, t->base.loc,
                        current_scope, s2c(al, local_sym), t,
                        m->m_name, nullptr, 0, gp_ext->m_name, dflt_access);
                }
                current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
            }
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
                throw SemanticError("Symbol " + local_sym +" already defined", loc);
            }
            // Repack ExternalSymbol to point directly to the original symbol
            ASR::ExternalSymbol_t *es = ASR::down_cast<ASR::ExternalSymbol_t>(t);
            ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                al, es->base.base.loc,
                current_scope,
                /* a_name */ s2c(al, local_sym),
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
                        LCOMPILERS_ASSERT(false);
                    }
                    to_be_imported_later.push(std::make_pair(proc_remote_sym, mangled_name));
                }
            }
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(ep));
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
        } else {
            throw LCompilersException("Only Subroutines, Functions, Variables and Derived supported in 'use'");
        }
    }

    void visit_Use(const AST::Use_t &x) {
        std::string msym = to_lower(x.m_module);
        Str msym_c; msym_c.from_str_view(msym);
        char *msym_cc = msym_c.c_str(al);
        current_module_dependencies.push_back(al, msym_cc);

        ASR::symbol_t *t = current_scope->resolve_symbol(msym);
        if (!t) {
            LCompilers::PassOptions pass_options;
            pass_options.runtime_library_dir = compiler_options.runtime_library_dir;
            pass_options.mod_files_dir = compiler_options.mod_files_dir;
            pass_options.include_dirs = compiler_options.include_dirs;

            SymbolTable *tu_symtab = current_scope;
            while (tu_symtab->parent != nullptr) {
                tu_symtab = tu_symtab->parent;
            }
            t = (ASR::symbol_t*)(ASRUtils::load_module(al, tu_symtab,
                msym, x.base.base.loc, false, pass_options, true,
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
            this->visit_unit_decl2(*x.m_decl[i]);
            if (tmp && ASR::is_a<ASR::require_instantiation_t>(*tmp)) {
                reqs.push_back(al, ASR::down_cast<ASR::require_instantiation_t>(tmp));
                tmp = nullptr;    
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
            for (size_t i=0; i<current_procedure_args.size(); i++) {
                std::string arg = current_procedure_args[i];
                if (sym.compare(arg) == 0) {
                    defined = true;
                }
            }
            if (!defined) {
                throw SemanticError("Symbol " + sym + " is not declared in " + to_lower(x.m_name) + "'s parameters",
                                    x.base.base.loc);
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

    void visit_Requires(const AST::Requires_t &x) {
        std::map<std::string,std::string> parameter_map;
        std::string require_name = to_lower(x.m_name);
        ASR::symbol_t *req0 = current_scope->resolve_symbol(require_name);

        if (!req0 || !ASR::is_a<ASR::Requirement_t>(*req0)) {
            throw SemanticError("No requirement '" + require_name + "' is defined",
                x.base.base.loc);
        }

        ASR::Requirement_t *req = ASR::down_cast<ASR::Requirement_t>(req0);

        if (x.n_namelist != req->n_args) {
            throw SemanticError("The number of parameters passed to '" +
                require_name + "' is not correct", x.base.base.loc);
        }

        SetChar args;
        args.reserve(al, x.n_namelist);
        for (size_t i=0; i<x.n_namelist; i++) {
            std::string temp_arg = to_lower(x.m_namelist[i]);
            args.push_back(al, s2c(al, temp_arg));
            if (std::find(current_procedure_args.begin(),
                          current_procedure_args.end(),
                          temp_arg) == current_procedure_args.end()) {
                throw SemanticError("Parameter '" + std::string(x.m_namelist[i])
                    + "' was not declared", x.base.base.loc);
            }
            ASR::symbol_t *temp_arg_sym = current_scope->resolve_symbol(temp_arg);
            if (!temp_arg_sym) {
                std::string req_arg = req->m_args[i];
                parameter_map[req_arg] = temp_arg;
                ASR::symbol_t *req_arg_sym = (req->m_symtab)->get_symbol(req_arg);
                temp_arg_sym = replace_symbol(req_arg_sym, temp_arg);
                current_scope->add_symbol(temp_arg, temp_arg_sym);
            }
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
                    std::string new_proc_name = parameter_map[ASRUtils::symbol_name(proc)];
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
    }

    void visit_Template(const AST::Template_t &x){
        is_template = true;

        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        for (size_t i=0; i<x.n_namelist; i++) {
            current_procedure_args.push_back(to_lower(x.m_namelist[i]));
        }

        Vec<ASR::require_instantiation_t*> reqs;
        reqs.reserve(al, x.n_decl);
        // For interface and type parameters (derived type)
        for (size_t i=0; i<x.n_decl; i++) {
            this->visit_unit_decl2(*x.m_decl[i]);
            if (tmp && ASR::is_a<ASR::require_instantiation_t>(*tmp)) {
                reqs.push_back(al, ASR::down_cast<ASR::require_instantiation_t>(tmp));
                tmp = nullptr;
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
        }

        ASR::asr_t *temp = ASR::make_Template_t(al, x.base.base.loc,
            current_scope, x.m_name, args.p, args.size(), reqs.p, reqs.size());

        parent_scope->add_symbol(x.m_name, ASR::down_cast<ASR::symbol_t>(temp));

        current_scope = parent_scope;
        current_procedure_args.clear();
        context_map.clear();
        is_template = false;
    }

    void visit_Instantiate(const AST::Instantiate_t &x) {
        std::string template_name = x.m_name;

        // check if the template exists
        ASR::symbol_t *sym0 = current_scope->resolve_symbol(template_name);
        if (!sym0) {
            throw SemanticError("Use of an unspecified template '" + template_name
                + "'", x.base.base.loc);
        }

        // check if the symbol is a template
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(sym0);
        if (!ASR::is_a<ASR::Template_t>(*sym)) {
            throw SemanticError("Cannot instantiate a non-tempalte '" + template_name
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
            if (AST::is_a<AST::UseSymbol_t>(*x.m_args[i])) {
                AST::UseSymbol_t* arg_symbol = AST::down_cast<AST::UseSymbol_t>(x.m_args[i]);
                std::string arg = to_lower(arg_symbol->m_remote_sym);
                if (ASR::is_a<ASR::Variable_t>(*param_sym)) {
                    ASR::ttype_t *t = ASRUtils::symbol_type(param_sym);
                    ASR::ttype_t* s;
                    if (ASRUtils::is_type_parameter(*t)) {
                        ASR::TypeParameter_t *tp = ASR::down_cast<ASR::TypeParameter_t>(t);
                        if (arg.compare("real") == 0) {
                            s = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 4));
                        } else if (arg.compare("integer") == 0) {
                            s = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4));
                        } else if (arg.compare("complex") == 0) {
                            s = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc, 4));
                        } else if (arg.compare("character") == 0) {
                            s = ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc, 1, 0, nullptr));
                        } else if (arg.compare("logical") == 0) {
                            s = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4));
                        } else {
                            throw SemanticError(
                                "The type " + arg + " is not yet handled for template instantiation",
                                x.base.base.loc);
                        }
                        type_subs[tp->m_param] = s;
                    } else {
                        // type checking non-type parameter template arguments
                        ASR::symbol_t *arg_sym = current_scope->resolve_symbol(arg);
                        s = ASRUtils::symbol_type(arg_sym);
                        if (!ASRUtils::check_equal_type(s, t)) {
                            throw SemanticError(
                                "The type of " + arg + " does not match the type of " + param,
                                x.base.base.loc);
                        }
                        symbol_subs[param] = arg_sym;
                    }
                } else if (ASR::is_a<ASR::Function_t>(*param_sym)) {
                    ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(param_sym);
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
                    check_restriction(type_subs, symbol_subs, f, f_arg0, x.base.base.loc);
                } else {
                    throw SemanticError("Unsupported symbol argument", x.m_args[i]->base.loc);
                }
            } else if (AST::is_a<AST::IntrinsicOperator_t>(*x.m_args[i])) {
                AST::IntrinsicOperator_t *intrinsic_op = AST::down_cast<AST::IntrinsicOperator_t>(x.m_args[i]);
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
                    throw LCompilersException("ICE: must be binop or cmop");
                }

                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(param_sym);
                std::string f_name = f->m_name;
                bool found = false;
                // check if an alias is defined for the operator
                if (is_overloaded) {
                    ASR::symbol_t* sym = current_scope->resolve_symbol(op_name);
                    ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
                    ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
                    for (size_t i = 0; i < gen_proc->n_procs; i++) {
                        ASR::symbol_t* proc = gen_proc->m_procs[i];
                        try {
                            check_restriction(type_subs, symbol_subs, f, proc, x.base.base.loc);
                            found = true;
                            break;
                        } catch (const SemanticError &e) {
                            // ignore restriction error so that the next alias can be checked
                        }
                    }
                }

                // if not found, then try to build a function for intrinsic operator
                if (!found) {
                    if (f->n_args != 2) {
                        throw SemanticError("The restriction " + f_name
                            + " does not have 2 parameters", x.base.base.loc);
                    }
                    ASR::ttype_t *ltype = ASRUtils::subs_expr_type(type_subs, f->m_args[0]);
                    ASR::ttype_t *rtype = ASRUtils::subs_expr_type(type_subs, f->m_args[1]);
                    ASR::ttype_t *ftype = ASRUtils::subs_expr_type(type_subs, f->m_return_var);
                    if (is_binop) {
                        if (!ASRUtils::check_equal_type(ltype, rtype) || !ASRUtils::check_equal_type(rtype, ftype)) {
                            throw SemanticError("Intrinsic operator "+ op_name + " does not apply to "
                                "arguments of different types", x.base.base.loc);
                        }
                    } else if (is_cmpop) {
                        if (!ASRUtils::check_equal_type(ltype, rtype) || !ASRUtils::is_logical(*ftype)) {
                            throw SemanticError("Intrinsic operator " + op_name + 
                                " requires same-typed arguments and a logical return type", x.base.base.loc);
                        }
                    }

                    SymbolTable *parent_scope = current_scope;
                    current_scope = al.make_new<SymbolTable>(parent_scope);
                    Vec<ASR::expr_t*> args;
                    args.reserve(al, 2);
                    for (size_t i=0; i<2; i++) {
                        std::string var_name = "arg" + std::to_string(i);
                        ASR::asr_t *v = ASR::make_Variable_t(al, x.base.base.loc, current_scope,
                            s2c(al, var_name), nullptr, 0, ASR::intentType::In, nullptr, nullptr,
                            ASR::storage_typeType::Default, ASRUtils::duplicate_type(al, ltype),
                            nullptr,
                            ASR::abiType::Source, ASR::accessType::Private,
                            ASR::presenceType::Required, false);
                        current_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(v));
                        ASR::symbol_t *var = current_scope->get_symbol(var_name);
                        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, var)));
                    }

                    std::string func_name = op_name + "_intrinsic_" + ASRUtils::type_to_str(ltype);
                    ASR::ttype_t *return_type = nullptr;
                    ASR::expr_t *value = nullptr;
                    ASR::expr_t *lexpr = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                        current_scope->get_symbol("arg0")));
                    ASR::expr_t *rexpr = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                        current_scope->get_symbol("arg1")));
                        
                    if (is_binop) {
                        value = ASRUtils::EXPR(ASRUtils::make_Binop_util(al, x.base.base.loc, binop, lexpr, rexpr, ltype));
                        return_type = ASRUtils::duplicate_type(al, ltype);
                    } else {
                        value = ASRUtils::EXPR(ASRUtils::make_Cmpop_util(al, x.base.base.loc, cmpop, lexpr, rexpr, ltype));
                        return_type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4));
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
                    ASR::stmt_t *assignment = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc,
                        target, value, nullptr));
                    body.push_back(al, assignment);

                    ASR::asr_t *op_function = ASRUtils::make_Function_t_util(
                        al, x.base.base.loc, current_scope, s2c(al, func_name),
                        nullptr, 0, args.p, 2, body.p, 1, return_expr,
                        ASR::abiType::Source, ASR::accessType::Public,
                        ASR::deftypeType::Implementation, nullptr, false, true,
                        false, false, false, nullptr, 0, false, false, true);

                    ASR::symbol_t *op_sym = ASR::down_cast<ASR::symbol_t>(op_function);
                    parent_scope->add_symbol(func_name, op_sym);
                    current_scope = parent_scope;
                    symbol_subs[f->m_name] = op_sym;
                }
            } else {
                throw SemanticError("Unsupported template argument", x.m_args[i]->base.loc);
            }
        }

        for (size_t i = 0; i < x.n_symbols; i++){
            AST::UseSymbol_t* use_symbol = AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i]);
            std::string generic_name = to_lower(use_symbol->m_remote_sym);
            ASR::symbol_t *s = temp->m_symtab->resolve_symbol(generic_name);
            if (!s) {
                throw SemanticError("Symbol " + generic_name + " was not found",
                                    x.base.base.loc);
            }
            std::string new_sym_name = to_lower(use_symbol->m_local_rename);
            pass_instantiate_symbol(al, context_map, type_subs, symbol_subs,
                current_scope, temp->m_symtab, new_sym_name, s);
            context_map[generic_name] = new_sym_name;
        }

        if (x.n_symbols == 0) {
            for (auto const &sym_pair: temp->m_symtab->get_scope()) {
                ASR::symbol_t *s = sym_pair.second;
                std::string s_name = ASRUtils::symbol_name(s);
                if (ASR::is_a<ASR::Function_t>(*s) && !ASRUtils::is_arg(sym, s_name)) {
                    pass_instantiate_symbol(al, context_map, type_subs, symbol_subs,
                        current_scope, temp->m_symtab, s_name, s);
                    context_map[s_name] = s_name;
                }
            }
        }

        instantiate_types[x.base.base.loc.first] = type_subs;
        instantiate_symbols[x.base.base.loc.first] = symbol_subs;
    }

    void check_restriction(std::map<std::string, ASR::ttype_t*> type_subs,
            std::map<std::string, ASR::symbol_t*> &symbol_subs,
            ASR::Function_t *f, ASR::symbol_t *sym_arg, const Location& loc) {
        std::string f_name = f->m_name;
        ASR::Function_t *arg = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(sym_arg));
        std::string arg_name = arg->m_name;
        if (f->n_args != arg->n_args) {
            std::string f_narg = std::to_string(f->n_args);
            std::string arg_narg = std::to_string(arg->n_args);
            diag.add(Diagnostic(
                "Number of arguments mismatch, restriction expects a function with " + f_narg
                    + " parameters, but a function with " + arg_narg + " parameters is provided",
                    Level::Error, Stage::Semantic, {
                        Label(arg_name + " has " + arg_narg + " parameters",
                                {loc, arg->base.base.loc}),
                        Label(f_name + " has " + f_narg + " parameters",
                                {f->base.base.loc})
                    }
            ));
            throw SemanticAbort();
        }
        for (size_t i = 0; i < f->n_args; i++) {
            ASR::ttype_t *f_param = ASRUtils::expr_type(f->m_args[i]);
            ASR::ttype_t *arg_param = ASRUtils::expr_type(arg->m_args[i]);
            if (ASR::is_a<ASR::TypeParameter_t>(*f_param)) {
                ASR::TypeParameter_t *f_tp
                    = ASR::down_cast<ASR::TypeParameter_t>(f_param);
                if (!ASRUtils::check_equal_type(type_subs[f_tp->m_param],
                                                arg_param)) {
                    std::string rtype = ASRUtils::type_to_str(type_subs[f_tp->m_param]);
                    std::string rvar = ASRUtils::symbol_name(
                                        ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
                    std::string atype = ASRUtils::type_to_str(arg_param);
                    std::string avar = ASRUtils::symbol_name(
                                        ASR::down_cast<ASR::Var_t>(arg->m_args[i])->m_v);
                    diag.add(Diagnostic(
                        "Restriction type mismatch with provided function argument",
                        Level::Error, Stage::Semantic, {
                            Label("", {loc}),
                            Label("Restriction's parameter " + rvar + " of type " + rtype,
                                    {f->m_args[i]->base.loc}),
                            Label("Function's parameter " + avar + " of type " + atype,
                                    {arg->m_args[i]->base.loc})
                        }
                    ));
                    throw SemanticAbort();
                }
            }
        }
        if (f->m_return_var) {
            if (!arg->m_return_var) {
                std::string msg = "The restriction argument " + arg_name
                    + " should have a return value";
                throw SemanticError(msg, loc);
            }
            ASR::ttype_t *f_ret = ASRUtils::expr_type(f->m_return_var);
            ASR::ttype_t *arg_ret = ASRUtils::expr_type(arg->m_return_var);
            if (ASR::is_a<ASR::TypeParameter_t>(*f_ret)) {
                ASR::TypeParameter_t *return_tp
                    = ASR::down_cast<ASR::TypeParameter_t>(f_ret);
                if (!ASRUtils::check_equal_type(type_subs[return_tp->m_param], arg_ret)) {
                    std::string rtype = ASRUtils::type_to_str(type_subs[return_tp->m_param]);
                    std::string atype = ASRUtils::type_to_str(arg_ret);
                    diag.add(Diagnostic(
                       "Restriction type mismatch with provided function argument",
                       Level::Error, Stage::Semantic, {
                            Label("", {loc}),
                            Label("Restriction's return type " + rtype,
                                {f->m_return_var->base.loc}),
                            Label("Function's return type " + atype,
                                {arg->m_return_var->base.loc})
                       }
                    ));
                    throw SemanticAbort();
                }
            }
        } else {
            if (arg->m_return_var) {
                std::string msg = "The restriction argument " + arg_name
                    + " should not have a return value";
                throw SemanticError(msg, loc);
            }
        }
        symbol_subs[f_name] = sym_arg;
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
                    t = ASRUtils::TYPE(ASR::make_TypeParameter_t(al,
                        tp->base.base.loc, s2c(al, name)));
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
                            param_type = ASRUtils::TYPE(ASR::make_TypeParameter_t(
                                al, tp->base.base.loc, s2c(al, context_map[tp->m_param])));
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
                            return_type = ASRUtils::TYPE(ASR::make_TypeParameter_t(
                                al, tp->base.base.loc, s2c(al, context_map[tp->m_param])));
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
            default : throw LCompilersException("Not implemented " + std::to_string(s->type));
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
            x.base.base.loc, 4));

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
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols)
{
    SymbolTableVisitor v(al, symbol_table, diagnostics, compiler_options, implicit_mapping, common_variables_hash, external_procedures_mapping,
                         instantiate_types, instantiate_symbols);
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
