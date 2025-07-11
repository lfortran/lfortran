#ifndef LFORTRAN_ASR_TO_C_CPP_H
#define LFORTRAN_ASR_TO_C_CPP_H

/*
 * Common code to be used in both of:
 *
 * * asr_to_cpp.cpp
 * * asr_to_c.cpp
 *
 * In particular, a common base class visitor with visitors that are identical
 * for both C and C++ code generation.
 */

#include <memory>
#include <set>

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_c.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/pass/unused_functions.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_subroutine_registry.h>


#include <map>

#define CHECK_FAST_C_CPP(compiler_options, x)                   \
        if (compiler_options.po.fast && x.m_value != nullptr) { \
            self().visit_expr(*x.m_value);                      \
            return;                                             \
        }                                                       \


namespace LCompilers {


// Platform dependent fast unique hash:
static inline uint64_t get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

struct SymbolInfo
{
    bool needs_declaration = true;
    bool intrinsic_function = false;
};

struct DeclarationOptions {
};

struct CDeclarationOptions: public DeclarationOptions {
    bool pre_initialise_derived_type;
    bool use_ptr_for_derived_type;
    bool use_static;
    bool force_declare;
    std::string force_declare_name;
    bool declare_as_constant;
    std::string const_name;
    bool do_not_initialize;

    CDeclarationOptions() :
    pre_initialise_derived_type{true},
    use_ptr_for_derived_type{true},
    use_static{true},
    force_declare{false},
    force_declare_name{""},
    declare_as_constant{false},
    const_name{""},
    do_not_initialize{false} {
    }
};

struct CPPDeclarationOptions: public DeclarationOptions {
    bool use_static;
    bool use_templates_for_arrays;

    CPPDeclarationOptions() :
    use_static{true},
    use_templates_for_arrays{false} {
    }
};

template <class StructType>
class BaseCCPPVisitor : public ASR::BaseVisitor<StructType>
{
private:
    StructType& self() { return static_cast<StructType&>(*this); }
public:
    diag::Diagnostics &diag;
    Platform platform;
    // `src` acts as a buffer that accumulates the generated C/C++ source code
    // as the visitor traverses all the ASR nodes of a program. Each visitor method
    // uses `src` to return the result, and the caller visitor uses `src` as the
    // value of the callee visitors it calls. The C/C++ complete source code
    // is then recursively constructed using `src`.
    std::string src;
    std::string current_body;
    CompilerOptions &compiler_options;
    int indentation_level;
    int indentation_spaces;
    // The precedence of the last expression, using the table:
    // https://en.cppreference.com/w/cpp/language/operator_precedence
    int last_expr_precedence;
    bool intrinsic_module = false;
    const ASR::Function_t *current_function = nullptr;
    std::map<uint64_t, SymbolInfo> sym_info;
    std::map<uint64_t, std::string> const_var_names;
    std::map<int32_t, std::string> gotoid2name;
    std::map<std::string, std::string> emit_headers;
    std::string array_types_decls;
    std::string forward_decl_functions;

    // Output configuration:
    // Use std::string or char*
    bool gen_stdstring;
    // Use std::complex<float/double> or float/double complex
    bool gen_stdcomplex;
    bool is_c;
    std::set<std::string> headers, user_headers, user_defines;
    std::vector<std::string> tmp_buffer_src;

    SymbolTable* global_scope;
    int64_t lower_bound;

    std::string template_for_Kokkos;
    size_t template_number;
    std::string from_std_vector_helper;

    std::unique_ptr<CCPPDSUtils> c_ds_api;
    std::unique_ptr<CUtils::CUtilFunctions> c_utils_functions;
    std::unique_ptr<BindPyUtils::BindPyUtilFunctions> bind_py_utils_functions;
    std::string const_name;
    size_t const_vars_count;
    size_t loop_end_count;

    // This is used to track if during the codegeneration whether or not
    // the source is inside any bracket. bracket_open is always >= 0. We
    // increment when we come-across a open bracket and decrement when we
    // come-across a closing bracket.
    // This helps in putting the extra code-generation (mainly of Constants)
    // in the right place and avoid producing syntax errors.
    // For example:
    // In FunctionCall node: we do `some_fun(` -> bracket_open++
    // and when we close the bracket `...)` -> bracket_open--

    int bracket_open;

    SymbolTable* current_scope;
    bool is_string_concat_present;

    BaseCCPPVisitor(diag::Diagnostics &diag, Platform &platform,
            CompilerOptions &_compiler_options, bool gen_stdstring, bool gen_stdcomplex, bool is_c,
            int64_t default_lower_bound) : diag{diag},
            platform{platform}, compiler_options{_compiler_options}, array_types_decls{std::string("")},
        gen_stdstring{gen_stdstring}, gen_stdcomplex{gen_stdcomplex},
        is_c{is_c}, global_scope{nullptr}, lower_bound{default_lower_bound},
        template_number{0}, c_ds_api{std::make_unique<CCPPDSUtils>(is_c, platform)},
        c_utils_functions{std::make_unique<CUtils::CUtilFunctions>()},
        bind_py_utils_functions{std::make_unique<BindPyUtils::BindPyUtilFunctions>()},
        const_name{"constname"},
        const_vars_count{0}, loop_end_count{0}, bracket_open{0},
        is_string_concat_present{false} {
        }

    std::string get_final_combined_src(std::string head, std::string unit_src) {
        std::string to_include = "";
        for (auto &s: user_defines) {
            to_include += "#define " + s + "\n";
        }
        for (auto &s: headers) {
            to_include += "#include <" + s + ">\n";
        }
        for (auto &s: user_headers) {
            to_include += "#include \"" + s + "\"\n";
        }
        if( c_ds_api->get_func_decls().size() > 0 ) {
            array_types_decls += "\n" + c_ds_api->get_func_decls() + "\n";
        }
        if( c_utils_functions->get_util_func_decls().size() > 0 ) {
            array_types_decls += "\n" + c_utils_functions->get_util_func_decls() + "\n";
        }
        std::string ds_funcs_defined = "";
        if( c_ds_api->get_generated_code().size() > 0 ) {
            ds_funcs_defined =  "\n" + c_ds_api->get_generated_code() + "\n";
        }
        std::string util_funcs_defined = "";
        if( c_utils_functions->get_generated_code().size() > 0 ) {
            util_funcs_defined =  "\n" + c_utils_functions->get_generated_code() + "\n";
        }
        if( bind_py_utils_functions->get_util_func_decls().size() > 0 ) {
            array_types_decls += "\n" + bind_py_utils_functions->get_util_func_decls() + "\n";
        }
        if( bind_py_utils_functions->get_generated_code().size() > 0 ) {
            util_funcs_defined =  "\n" + bind_py_utils_functions->get_generated_code() + "\n";
        }
        if( is_string_concat_present ) {
            std::string strcat_def = "";
            strcat_def += "    char* " + global_scope->get_unique_name("strcat_", false) + "(char* x, char* y) {\n";
            strcat_def += "        char* str_tmp = (char*) malloc((strlen(x) + strlen(y) + 2) * sizeof(char));\n";
            strcat_def += "        strcpy(str_tmp, x);\n";
            strcat_def += "        return strcat(str_tmp, y);\n";
            strcat_def += "    }\n\n";
            head += strcat_def;
        }

        // Include dimension_descriptor definition that is used by array types
        if (array_types_decls.size() != 0) {
            array_types_decls = "\nstruct dimension_descriptor\n"
                "{\n    int32_t lower_bound, length, stride;\n};\n" + array_types_decls;
        }

        return to_include + head + array_types_decls + forward_decl_functions + unit_src +
              ds_funcs_defined + util_funcs_defined;
    }
    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        global_scope = x.m_symtab;
        // All loose statements must be converted to a function, so the items
        // must be empty:
        LCOMPILERS_ASSERT(x.n_items == 0);
        std::string unit_src = "";
        indentation_level = 0;
        indentation_spaces = 4;
        c_ds_api->set_indentation(indentation_level + 1, indentation_spaces);
        c_ds_api->set_global_scope(global_scope);

        std::string headers =
R"(#include <stdio.h>
#include <assert.h>
#include <complex.h>
#include <lfortran_intrinsics.h>
)";
        unit_src += headers;

        {
            // Process intrinsic modules in the right order
            std::vector<std::string> build_order
                = ASRUtils::determine_module_dependencies(x);
            for (auto &item : build_order) {
                LCOMPILERS_ASSERT(x.m_symtab->get_scope().find(item)
                    != x.m_symtab->get_scope().end());
                if (startswith(item, "lfortran_intrinsic")) {
                    ASR::symbol_t *mod = x.m_symtab->get_symbol(item);
                    self().visit_symbol(*mod);
                    unit_src += src;
                }
            }
        }

        // Process procedures first:
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                self().visit_symbol(*item.second);
                unit_src += src;
            }
        }

        // Then do all the modules in the right order
        std::vector<std::string> build_order
            = ASRUtils::determine_module_dependencies(x);
        for (auto &item : build_order) {
            LCOMPILERS_ASSERT(x.m_symtab->get_scope().find(item)
                != x.m_symtab->get_scope().end());
            if (!startswith(item, "lfortran_intrinsic")) {
                ASR::symbol_t *mod = x.m_symtab->get_symbol(item);
                self().visit_symbol(*mod);
                unit_src += src;
            }
        }

        // Then the main program:
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*item.second)) {
                self().visit_symbol(*item.second);
                unit_src += src;
            }
        }

        src = unit_src;
    }

    std::string check_tmp_buffer() {
        std::string ret = "";
        if (bracket_open == 0 && !tmp_buffer_src.empty()) {
            for (auto &s: tmp_buffer_src) ret += s;
            tmp_buffer_src.clear();
        }
        return ret;
    }

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_intrinsic) {
            intrinsic_module = true;
        } else {
            intrinsic_module = false;
        }

        std::string contains;

        // Declare the global variables that are imported from the module
        std::vector<std::string> var_order = ASRUtils::determine_variable_declaration_order(x.m_symtab);
        for (auto &item : var_order) {
            ASR::symbol_t* var_sym = x.m_symtab->get_symbol(item);
            if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(var_sym);
                std::string decl = self().convert_variable_decl(*v);
                decl = check_tmp_buffer() + decl;
                bool used_define_for_const = (v->m_storage == ASR::storage_typeType::Parameter &&
                        v->m_intent == ASRUtils::intent_local);
                if (used_define_for_const) {
                    contains += decl + "\n";
                    continue;
                }
                if (v->m_value) {
                    self().visit_expr(*v->m_value);
                    decl += " = " + src;
                }
                decl += ";\n\n";
                contains += decl;
            }
        }

        // Topologically sort all module functions
        // and then define them in the right order
        std::vector<std::string> func_order = ASRUtils::determine_function_definition_order(x.m_symtab);

        // Generate the bodies of subroutines
        for (auto &item : func_order) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(item);
            if( !sym ) {
                continue ;
            }
            ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(sym);
            self().visit_Function(*s);
            contains += src;
        }

        src = contains;
        intrinsic_module = false;
    }

    void visit_Program(const ASR::Program_t &x) {
        // Generate code for nested subroutines and functions first:
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        std::string contains;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*s);
                contains += src;
            }
        }

        // Generate code for the main program
        indentation_level += 1;
        std::string indent1(indentation_level*indentation_spaces, ' ');
        indentation_level += 1;
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string decl;
        std::vector<std::string> var_order = ASRUtils::determine_variable_declaration_order(x.m_symtab);
        for (auto &item : var_order) {
            ASR::symbol_t* var_sym = x.m_symtab->get_symbol(item);
            if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(var_sym);
                std::string d = self().convert_variable_decl(*v) + ";\n";
                decl += check_tmp_buffer() + d;
            }
        }

        std::string body;
        for (size_t i=0; i<x.n_body; i++) {
            self().visit_stmt(*x.m_body[i]);
            body += src;
        }

        src = contains
                + "int main(int argc, char* argv[])\n{\n"
                + decl + body
                + indent1 + "return 0;\n}\n";
        indentation_level -= 2;
        current_scope = current_scope_copy;
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Block_t>(*x.m_m));
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(x.m_m);
        std::string decl, body;
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string open_paranthesis = indent + "{\n";
        std::string close_paranthesis = indent + "}\n";
        if (x.m_label != -1) {
            std::string b_name;
            if (gotoid2name.find(x.m_label) != gotoid2name.end()) {
                b_name = gotoid2name[x.m_label];
            } else {
                b_name = "__" +std::to_string(x.m_label);
            }
            open_paranthesis = indent + b_name + ": {\n";
        }
        indent += std::string(indentation_spaces, ' ');
        indentation_level += 1;
        SymbolTable* current_scope_copy = current_scope;
        current_scope = block->m_symtab;
        std::vector<std::string> var_order = ASRUtils::determine_variable_declaration_order(block->m_symtab);
        for (auto &item : var_order) {
            ASR::symbol_t* var_sym = block->m_symtab->get_symbol(item);
            if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(var_sym);
                std::string d = indent + self().convert_variable_decl(*v) + ";\n";
                decl += check_tmp_buffer() + d;
            }
        }
        for (size_t i=0; i<block->n_body; i++) {
            self().visit_stmt(*block->m_body[i]);
            body += src;
        }
        decl += check_tmp_buffer();
        src = open_paranthesis + decl + body + close_paranthesis;
        indentation_level -= 1;
        current_scope = current_scope_copy;
    }

    std::string get_return_var_type(ASR::Variable_t* return_var) {
        std::string sub;
        bool is_array = ASRUtils::is_array(return_var->m_type);
        if (ASRUtils::is_integer(*return_var->m_type)) {
            int kind = ASRUtils::extract_kind_from_ttype_t(return_var->m_type);
            if (is_array) {
                sub = "struct i" + std::to_string(kind * 8) + "* ";
            } else {
                sub = "int" + std::to_string(kind * 8) + "_t ";
            }
        } else if (ASRUtils::is_unsigned_integer(*return_var->m_type)) {
            int kind = ASRUtils::extract_kind_from_ttype_t(return_var->m_type);
            if (is_array) {
                sub = "struct u" + std::to_string(kind * 8) + "* ";
            } else {
                sub = "uint" + std::to_string(kind * 8) + "_t ";
            }
        } else if (ASRUtils::is_real(*return_var->m_type)) {
            int kind = ASRUtils::extract_kind_from_ttype_t(return_var->m_type);
            bool is_float = (kind == 4);
            if (is_array) {
                sub = "struct r" + std::to_string(kind * 8) + "* ";
            } else {
                if (is_float) {
                    sub = "float ";
                } else {
                    sub = "double ";
                }
            }
        } else if (ASRUtils::is_logical(*return_var->m_type)) {
            if (is_array) {
                sub = "struct i1* ";
            } else {
                sub = "bool ";
            }
        } else if (ASRUtils::is_character(*return_var->m_type)) {
            if (gen_stdstring) {
                sub = "std::string ";
            } else {
                sub = "char* ";
            }
        } else if (ASRUtils::is_complex(*return_var->m_type)) {
            int kind = ASRUtils::extract_kind_from_ttype_t(return_var->m_type);
            if (is_array) {
                sub = "struct c" + std::to_string(kind * 8) + "* ";
            } else {
                bool is_float = kind == 4;
                if (is_float) {
                    if (gen_stdcomplex) {
                        sub = "std::complex<float> ";
                    } else {
                        sub = "float_complex_t ";
                    }
                } else {
                    if (gen_stdcomplex) {
                        sub = "std::complex<double> ";
                    } else {
                        sub = "double_complex_t ";
                    }
                }
            }
        } else if (ASR::is_a<ASR::CPtr_t>(*return_var->m_type)) {
            sub = "void* ";
        } else if (ASR::is_a<ASR::List_t>(*return_var->m_type)) {
            ASR::List_t* list_type = ASR::down_cast<ASR::List_t>(return_var->m_type);
            sub = c_ds_api->get_list_type(list_type) + " ";
        } else if (ASR::is_a<ASR::Tuple_t>(*return_var->m_type)) {
            ASR::Tuple_t* tup_type = ASR::down_cast<ASR::Tuple_t>(return_var->m_type);
            sub = c_ds_api->get_tuple_type(tup_type) + " ";
        } else if (ASR::is_a<ASR::Pointer_t>(*return_var->m_type)) {
            ASR::Pointer_t* ptr_type = ASR::down_cast<ASR::Pointer_t>(return_var->m_type);
            std::string pointer_type_str = CUtils::get_c_type_from_ttype_t(ptr_type->m_type);
            sub = pointer_type_str + "*";
        } else if (ASR::is_a<ASR::TypeParameter_t>(*return_var->m_type)) {
            return "";
        } else if (ASR::is_a<ASR::Dict_t>(*return_var->m_type)) {
            ASR::Dict_t* dict_type = ASR::down_cast<ASR::Dict_t>(return_var->m_type);
            sub = c_ds_api->get_dict_type(dict_type) + " ";
        } else {
            throw CodeGenError("Return type not supported in function '" +
                std::string(ASRUtils::symbol_name(ASR::down_cast<ASR::symbol_t>(
                    return_var->m_parent_symtab->asr_owner))) +
                    + "'", return_var->base.base.loc);
        }

        return sub;
    }

    // Returns the declaration, no semi colon at the end
    std::string get_function_declaration(const ASR::Function_t &x, bool &has_typevar, bool is_pointer=false) {
        template_for_Kokkos.clear();
        template_number = 0;
        std::string sub, inl, static_attr;

        // This helps to check if the function is generic.
        // If it is generic we skip the codegen for that function.
        has_typevar = false;
        if (ASRUtils::get_FunctionType(x)->m_inline && !is_pointer) {
            inl = "inline __attribute__((always_inline)) ";
        }
        if( ASRUtils::get_FunctionType(x)->m_static && !is_pointer) {
            static_attr = "static ";
        }
        if (x.m_return_var) {
            ASR::Variable_t *return_var = ASRUtils::EXPR2VAR(x.m_return_var);
            has_typevar = ASR::is_a<ASR::TypeParameter_t>(*return_var->m_type);
            sub = get_return_var_type(return_var);
        } else {
            sub = "void ";
        }
        std::string sym_name = x.m_name;
        if (sym_name == "main") {
            sym_name = "_xx_lcompilers_changed_main_xx";
        }
        if (sym_name == "exit") {
            sym_name = "_xx_lcompilers_changed_exit_xx";
        }
        ASR::FunctionType_t *f_type = ASRUtils::get_FunctionType(x);
        if (f_type->m_abi == ASR::abiType::BindPython &&
                f_type->m_deftype == ASR::deftypeType::Implementation) {
            sym_name = "_xx_internal_" + sym_name + "_xx";
        }
        std::string func = static_attr + inl + sub;
        if (is_pointer) {
            func += "(*" + sym_name + ")(";
        } else {
            func += sym_name + "(";
        }
        bracket_open++;
        for (size_t i=0; i<x.n_args; i++) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_args[i])->m_v);
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                ASR::Variable_t *arg = ASR::down_cast<ASR::Variable_t>(sym);
                LCOMPILERS_ASSERT(ASRUtils::is_arg_dummy(arg->m_intent));
                if( is_c ) {
                    CDeclarationOptions c_decl_options;
                    c_decl_options.pre_initialise_derived_type = false;
                    func += self().convert_variable_decl(*arg, &c_decl_options);
                } else {
                    CPPDeclarationOptions cpp_decl_options;
                    cpp_decl_options.use_static = false;
                    cpp_decl_options.use_templates_for_arrays = true;
                    func += self().convert_variable_decl(*arg, &cpp_decl_options);
                }
                if (ASR::is_a<ASR::TypeParameter_t>(*arg->m_type)) {
                    has_typevar = true;
                    bracket_open--;
                    return "";
                }
            } else if (ASR::is_a<ASR::Function_t>(*sym)) {
                ASR::Function_t *fun = ASR::down_cast<ASR::Function_t>(sym);
                func += get_function_declaration(*fun, has_typevar, true);
            } else {
                throw CodeGenError("Unsupported function argument");
            }
            if (i < x.n_args-1) func += ", ";
        }
        func += ")";
        bracket_open--;
        if (is_c && f_type->m_abi == ASR::abiType::Source) {
            forward_decl_functions += func + ";\n";
        }
        if( is_c || template_for_Kokkos.empty() ) {
            return func;
        }

        template_for_Kokkos.pop_back();
        template_for_Kokkos.pop_back();
        return "\ntemplate <" + template_for_Kokkos + ">\n" + func;
    }

    std::string get_arg_conv_bind_python(const ASR::Function_t &x) {
        std::string arg_conv = R"(
    pArgs = PyTuple_New()" + std::to_string(x.n_args) + R"();
)";
        for (size_t i = 0; i < x.n_args; ++i) {
            ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i]);
            std::string arg_name = std::string(arg->m_name);
            std::string indent = "\n    ";
            if (ASRUtils::is_array(arg->m_type)) {
                arg_conv += indent + bind_py_utils_functions->get_conv_dims_to_1D_arr() + "(" + arg_name + "->n_dims, " + arg_name + "->dims, __new_dims);";
                std::string func_call = BindPyUtils::get_py_obj_type_conv_func_from_ttype_t(arg->m_type);
                arg_conv += indent + "pValue = " + func_call + "(" + arg_name + "->n_dims, __new_dims, "
                    + BindPyUtils::get_numpy_c_obj_type_conv_func_from_ttype_t(arg->m_type) + ", " + arg_name + "->data);";
            } else {
                arg_conv += indent + "pValue = " + BindPyUtils::get_py_obj_type_conv_func_from_ttype_t(arg->m_type)
                    + "(" + arg_name + ");";
            }
            arg_conv += R"(
    if (!pValue) {
        Py_DECREF(pArgs);
        Py_DECREF(pModule);
        fprintf(stderr, "Cannot convert argument\n");
        exit(1);
    }
    /* pValue reference stolen here: */
    PyTuple_SetItem(pArgs, )" + std::to_string(i) +  R"(, pValue);
)";
        }
        return arg_conv;
    }

    std::string get_return_value_conv_bind_python(const ASR::Function_t &x) {
        if (!x.m_return_var) return "";
        ASR::Variable_t* r_v = ASRUtils::EXPR2VAR(x.m_return_var);
        std::string indent = "\n    ";
        std::string ret_var_decl = indent + get_return_var_type(r_v) + " _lpython_return_variable;";
        std::string py_val_cnvrt = BindPyUtils::get_py_obj_ret_type_conv_fn_from_ttype(r_v->m_type,
            array_types_decls, c_ds_api, bind_py_utils_functions);
        std::string ret_assign = indent + "_lpython_return_variable = " + py_val_cnvrt + "(pValue);";
        std::string clear_pValue = indent + "Py_DECREF(pValue);";
        std::string ret_stmt = indent + "return _lpython_return_variable;";
        return ret_var_decl + ret_assign + clear_pValue + ret_stmt + "\n";
    }

    std::string get_func_body_bind_python(const ASR::Function_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string var_decls = "PyObject *pName, *pModule, *pFunc; PyObject *pArgs, *pValue;\n";
        std::string func_body = R"(
    pName = PyUnicode_FromString(")" + std::string(x.m_module_file) + R"(");
    if (pName == NULL) {
        PyErr_Print();
        fprintf(stderr, "Failed to convert to unicode string )" + std::string(x.m_module_file) + R"(\n");
        exit(1);
    }

    pModule = PyImport_Import(pName);
    Py_DECREF(pName);
    if (pModule == NULL) {
        PyErr_Print();
        fprintf(stderr, "Failed to load python module )" + std::string(x.m_module_file) + R"(\n");
        exit(1);
    }

    pFunc = PyObject_GetAttrString(pModule, ")" + std::string(x.m_name) + R"(");
    if (!pFunc || !PyCallable_Check(pFunc)) {
        if (PyErr_Occurred()) PyErr_Print();
        fprintf(stderr, "Cannot find function )" + std::string(x.m_name) + R"(\n");
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        exit(1);
    }
)" + get_arg_conv_bind_python(x) + R"(
    pValue = PyObject_CallObject(pFunc, pArgs);
    Py_DECREF(pArgs);
    if (pValue == NULL) {
        Py_DECREF(pFunc);
        Py_DECREF(pModule);
        PyErr_Print();
        fprintf(stderr,"Call failed\n");
        exit(1);
    }
)" + get_return_value_conv_bind_python(x);
        return "{\n" + indent + var_decls + func_body + "}\n";
    }

    std::string declare_all_functions(const SymbolTable &scope) {
        std::string code, t;
        for (auto &item : scope.get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(item.second);
                t = declare_all_functions(*s->m_symtab);
                bool has_typevar = false;
                t += get_function_declaration(*s, has_typevar);
                if (!has_typevar) code += t  + ";\n";
            }
        }
        return code;
    }

    std::string get_type_format(ASR::ttype_t *type) {
        // See: https://docs.python.org/3/c-api/arg.html for more info on `type format`
        switch (type->type) {
            case ASR::ttypeType::Integer: {
                int a_kind = ASRUtils::extract_kind_from_ttype_t(type);
                if (a_kind == 4) {
                    return "i";
                } else {
                    return "l";
                }
            } case ASR::ttypeType::Real : {
                int a_kind = ASRUtils::extract_kind_from_ttype_t(type);
                if (a_kind == 4) {
                    return "f";
                } else {
                    return "d";
                }
            } case ASR::ttypeType::Logical : {
                return "p";
            } case ASR::ttypeType::Array : {
                return "O";
            } default: {
                throw CodeGenError("CPython type format not supported yet");
            }
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        std::string sub = "";
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*f);
                sub += src + "\n";
            }
        }

        current_body = "";
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        if (std::string(x.m_name) == "size" && intrinsic_module ) {
            // Intrinsic function `size`
            SymbolInfo s;
            s.intrinsic_function = true;
            sym_info[get_hash((ASR::asr_t*)&x)] = s;
            src = "";
            return;
        } else if ((
                std::string(x.m_name) == "int" ||
                std::string(x.m_name) == "char" ||
                std::string(x.m_name) == "present" ||
                std::string(x.m_name) == "len" ||
                std::string(x.m_name) == "cabs" ||
                std::string(x.m_name) == "cacos" ||
                std::string(x.m_name) == "cacosh" ||
                std::string(x.m_name) == "casin" ||
                std::string(x.m_name) == "casinh" ||
                std::string(x.m_name) == "catan" ||
                std::string(x.m_name) == "catanh" ||
                std::string(x.m_name) == "ccos" ||
                std::string(x.m_name) == "ccosh" ||
                std::string(x.m_name) == "cexp" ||
                std::string(x.m_name) == "clog" ||
                std::string(x.m_name) == "csin" ||
                std::string(x.m_name) == "csinh" ||
                std::string(x.m_name) == "csqrt" ||
                std::string(x.m_name) == "ctan" ||
                std::string(x.m_name) == "ctanh" ||
                std::string(x.m_name) == "not"
                ) && intrinsic_module) {
            // Intrinsic function `int`
            SymbolInfo s;
            s.intrinsic_function = true;
            sym_info[get_hash((ASR::asr_t*)&x)] = s;
            src = "";
            return;
        } else {
            SymbolInfo s;
            s.intrinsic_function = false;
            sym_info[get_hash((ASR::asr_t*)&x)] = s;
        }
        bool has_typevar = false;
        sub += get_function_declaration(x, has_typevar);
        if (has_typevar) {
            src = "";
            return;
        }
        ASR::FunctionType_t *f_type = ASRUtils::get_FunctionType(x);
        bool generate_body = true;
        if (f_type->m_deftype == ASR::deftypeType::Interface) {
            generate_body = false;
            if (f_type->m_abi == ASR::abiType::BindC) {
                if (x.m_module_file) {
                    user_headers.insert(std::string(x.m_module_file));
                    src = "";
                    return;
                } else {
                    sub += ";\n";
                }
            } else if (f_type->m_abi == ASR::abiType::BindPython) {
                indentation_level += 1;
                sub += "\n" + get_func_body_bind_python(x);
                indentation_level -= 1;
            } else {
                generate_body = true;
            }
        }
        if( generate_body ) {
            sub += "\n";

            indentation_level += 1;
            std::string indent(indentation_level*indentation_spaces, ' ');
            std::string decl;
            std::vector<std::string> var_order = ASRUtils::determine_variable_declaration_order(x.m_symtab);
            for (auto &item : var_order) {
                ASR::symbol_t* var_sym = x.m_symtab->get_symbol(item);
                if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(var_sym);
                    if (v->m_intent == ASRUtils::intent_local ||
                        v->m_intent == ASRUtils::intent_return_var) {
                        std::string d = indent + self().convert_variable_decl(*v) + ";\n";
                        decl += check_tmp_buffer() + d;
                    }
                    if (ASR::is_a<ASR::TypeParameter_t>(*v->m_type)) {
                        has_typevar = true;
                        break;
                    }
                }
            }
            if (has_typevar) {
                indentation_level -= 1;
                src = "";
                return;
            }

            current_function = &x;

            for (size_t i=0; i<x.n_body; i++) {
                self().visit_stmt(*x.m_body[i]);
                current_body += src;
            }
            decl += check_tmp_buffer();
            current_function = nullptr;
            bool visited_return = false;

            if (x.n_body > 0 && ASR::is_a<ASR::Return_t>(*x.m_body[x.n_body-1])) {
                visited_return = true;
            }

            if (!visited_return && x.m_return_var) {
                current_body += indent + "return "
                    + ASRUtils::EXPR2VAR(x.m_return_var)->m_name
                    + ";\n";
            }

            if (decl.size() > 0 || current_body.size() > 0) {
                sub += "{\n" + decl + current_body + "}\n";
            } else {
                sub[sub.size()-1] = ';';
                sub += "\n";
            }
            indentation_level -= 1;
        }
        sub += "\n";
        src = sub;
        if (f_type->m_deftype == ASR::deftypeType::Implementation) {
            if (f_type->m_abi == ASR::abiType::BindC && x.m_module_file) {
                std::string header_name = std::string(x.m_module_file);
                user_headers.insert(header_name);
                emit_headers[header_name]+= "\n" + src;
                src = "";
            } else if (f_type->m_abi == ASR::abiType::BindPython) {
                indentation_level += 1;
                headers.insert("Python.h");
                std::string variables_decl = ""; // Stores the argument declarations
                std::string fill_parse_args_details = "";
                std::string type_format = "";
                std::string fn_args = "";
                std::string fill_array_details = "";
                std::string numpy_init = "";

                for (size_t i = 0; i < x.n_args; i++) {
                    ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i]);
                    std::string arg_name = arg->m_name;
                    fill_parse_args_details += "&" + arg_name;
                    type_format += get_type_format(arg->m_type);
                    if (ASR::is_a<ASR::Array_t>(*arg->m_type)) {
                        if (numpy_init.size() == 0) {
                            numpy_init = R"(
    // Initialize NumPy
    import_array();
)";
                            // Insert the headers for array handling
                            headers.insert("numpy/ndarrayobject.h");
                            user_defines.insert("NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION");
                        }
    // -------------------------------------------------------------------------
    // `PyArray_AsCArray` is used to convert NumPy Arrays to C Arrays
    // `fill_array_details` contains array operations to be performed on the arguments
    // `fill_parse_args_details` are used to capture the args from CPython
    // `fn_args` are the arguments that are passed to the shared library function
                        std::string c_array_type = self().convert_variable_decl(*arg);
                        c_array_type = c_array_type.substr(0,
                            c_array_type.size() - arg_name.size() - 2);
                        fn_args += "s_array_" + arg_name;
                        variables_decl += "    PyArrayObject *" + arg_name + ";\n";

                        fill_array_details += "\n    // Fill array details for " + arg_name
    + "\n    if (PyArray_NDIM(" + arg_name + R"() != 1) {
        PyErr_SetString(PyExc_TypeError, "An error occurred in the `lpython` decorator: "
            "Only 1 dimension array is supported for now.");
        return NULL;
    }

    )" + c_array_type + " *s_array_" + arg_name + " = malloc(sizeof(" + c_array_type + R"());
    {
        )" + CUtils::get_c_type_from_ttype_t(arg->m_type) + R"( *array;
        // Create C arrays from numpy objects:
        PyArray_Descr *descr = PyArray_DescrFromType(PyArray_TYPE()" + arg_name + R"());
        npy_intp dims[1];
        if (PyArray_AsCArray((PyObject **)&)" + arg_name + R"(, (void *)&array, dims, 1, descr) < 0) {
            PyErr_SetString(PyExc_TypeError, "An error occurred in the `lpython` decorator: "
                "Failed to create a C array");
            return NULL;
        }

        s_array_)" + arg_name + R"(->data = array;
        s_array_)" + arg_name + R"(->n_dims = 1;
        s_array_)" + arg_name + R"(->dims[0].lower_bound = 0;
        s_array_)" + arg_name + R"(->dims[0].length = dims[0];
        s_array_)" + arg_name + R"(->dims[0].stride = 1;
        s_array_)" + arg_name + R"(->offset = 0;
        s_array_)" + arg_name + R"(->is_allocated = false;
    }
)";
                    } else {
                        fn_args += arg_name;
                        variables_decl += "    " + self().convert_variable_decl(*arg)
                            + ";\n";
                    }
                    if (i < x.n_args - 1) {
                        fill_parse_args_details += ", ";
                        fn_args += ", ";
                    }
                }

                if (fill_parse_args_details.size() > 0) {
                    fill_parse_args_details = R"(
    // Parse the arguments from Python
    if (!PyArg_ParseTuple(args, ")" + type_format + R"(", )" + fill_parse_args_details + R"()) {
        PyErr_SetString(PyExc_TypeError, "An error occurred in the `lpython` decorator: "
            "Failed to parse or receive arguments from Python");
        return NULL;
    }
)";
                }

                std::string fn_name = x.m_name;
                std::string fill_return_details = "\n    // Call the C function";
                if (variables_decl.size() > 0) {
                    variables_decl.insert(0, "\n    "
                    "// Declare arguments and return variable\n");
                }
                // Handle the return variable if any; otherwise, return None
                if(x.m_return_var) {
                    ASR::Variable_t *return_var = ASRUtils::EXPR2VAR(x.m_return_var);
                    variables_decl += "    " + self().convert_variable_decl(*return_var)
                        + ";\n";
                    fill_return_details += "\n    _lpython_return_variable = _xx_internal_"
                        + fn_name + "_xx(" + fn_args + ");\n";
                    if (ASR::is_a<ASR::Array_t>(*return_var->m_type)) {
                        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(return_var->m_type);
                        if(arr->m_dims[0].m_length &&
                                ASR::is_a<ASR::Var_t>(*arr->m_dims[0].m_length)) {
                            // name() -> f64[n]: Extract `array_type` and `n`
                            std::string array_type
                                = BindPyUtils::get_numpy_c_obj_type_conv_func_from_ttype_t(arr->m_type);
                            std::string return_array_size = ASRUtils::EXPR2VAR(
                                arr->m_dims[0].m_length)->m_name;
                            fill_return_details += R"(
    // Copy the array elements and return the result as a Python object
    {
        npy_intp dims[] = {)" + return_array_size + R"(};
        PyObject* numpy_array = PyArray_SimpleNewFromData(1, dims, )" + array_type + R"(,
            _lpython_return_variable->data);
        if (numpy_array == NULL) {
            PyErr_SetString(PyExc_TypeError, "An error occurred in the `lpython` decorator: "
                "Failed to create an array that was used as a return variable");
            return NULL;
        }
        return numpy_array;
    })";
                        } else {
                            throw CodeGenError("Array return type without a length is not supported yet");
                        }
                    } else {
                        fill_return_details += R"(
    // Build and return the result as a Python object
    return Py_BuildValue(")" + get_type_format(return_var->m_type)
                            + "\", _lpython_return_variable);";
                    }
                } else {
                    fill_return_details += R"(
    _xx_internal_)" + fn_name + "_xx(" + fn_args + ");\n" + R"(
    // Return None
    Py_RETURN_NONE;)";
                }
                // `sub` contains the function to be called
                src = sub;
// Python wrapper for the Shared library
// TODO: Instead of a function call replace it with the function body
// Basically, inlining the function by hand
                src += R"(// Define the Python module and method mappings
static PyObject* )" + fn_name + R"((PyObject* self, PyObject* args) {)"
    + numpy_init + variables_decl + fill_parse_args_details
    + fill_array_details + fill_return_details + R"(
}

// Define the module's method table
static PyMethodDef )" + fn_name + R"(_module_methods[] = {
    {")" + fn_name + R"(", )" + fn_name + R"(, METH_VARARGS,
        "Handle arguments & return variable and call the function"},
    {NULL, NULL, 0, NULL}
};

// Define the module initialization function
static struct PyModuleDef )" + fn_name + R"(_module_def = {
    PyModuleDef_HEAD_INIT,
    "lpython_module_)" + fn_name + R"(",
    "Shared library to use LPython generated functions",
    -1,
    )" + fn_name + R"(_module_methods
};

PyMODINIT_FUNC PyInit_lpython_module_)" + fn_name + R"((void) {
    PyObject* module;

    // Create the module object
    module = PyModule_Create(&)" + fn_name + R"(_module_def);
    if (!module) {
        return NULL;
    }

    return module;
}

)";
            indentation_level -= 1;
            }
        }
        current_scope = current_scope_copy;
    }

    void visit_ArrayPhysicalCast(const ASR::ArrayPhysicalCast_t& x) {
        src = "";
        this->visit_expr(*x.m_arg);
        if (x.m_old == ASR::array_physical_typeType::FixedSizeArray &&
                x.m_new == ASR::array_physical_typeType::SIMDArray) {
            std::string arr_element_type = CUtils::get_c_type_from_ttype_t(ASRUtils::expr_type(x.m_arg));
            int64_t size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(x.m_arg));
            std::string cast = arr_element_type + " __attribute__ (( vector_size(sizeof("
                + arr_element_type + ") * " + std::to_string(size) + ") ))";
            src = "(" + cast + ") " + src;
        }
     }

    std::string construct_call_args(ASR::Function_t* f, size_t n_args, ASR::call_arg_t* m_args) {
        bracket_open++;
        std::string args = "";
        for (size_t i=0; i<n_args; i++) {
            ASR::expr_t* call_arg = m_args[i].m_value;
            self().visit_expr(*call_arg);
            ASR::ttype_t* type = ASRUtils::expr_type(call_arg);
            if (ASR::is_a<ASR::Var_t>(*call_arg)
                && ASR::is_a<ASR::Variable_t>(
                    *ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(m_args[i].m_value)->m_v))) {
                ASR::Variable_t* param = ASRUtils::EXPR2VAR(f->m_args[i]);
                if( (is_c && (param->m_intent == ASRUtils::intent_inout
                    || param->m_intent == ASRUtils::intent_out)
                    && !ASRUtils::is_aggregate_type(param->m_type))) {
                    args += "&" + src;
                } else if (param->m_intent == ASRUtils::intent_out) {
                    if (ASR::is_a<ASR::List_t>(*param->m_type) || 
                        ASR::is_a<ASR::Dict_t>(*param->m_type) || 
                        ASR::is_a<ASR::Tuple_t>(*param->m_type)) {
                        args += "&" + src;
                    } else {
                        args += src;
                    }
                } else {
                    args += src;
                }
            } else if (ASR::is_a<ASR::ArrayItem_t>(*call_arg)) {
                ASR::Variable_t* param = ASRUtils::EXPR2VAR(f->m_args[i]);
                if (param->m_intent == ASRUtils::intent_inout
                    || param->m_intent == ASRUtils::intent_out || ASR::is_a<ASR::StructType_t>(*type)) {
                    args += "&" + src;
                } else {
                    args += src;
                }
            } else {
                if( ASR::is_a<ASR::StructType_t>(*type) ) {
                    args += "&" + src;
                } else {
                    args += src;
                }
            }
            if (i < n_args-1) args += ", ";
        }
        bracket_open--;
        return args;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_name));
        std::string fn_name = fn->m_name;
        ASR::FunctionType_t *fn_type = ASRUtils::get_FunctionType(fn);
        if (fn_type->m_abi == ASR::abiType::BindC && fn_type->m_bindc_name) {
            fn_name = fn_type->m_bindc_name;
        } else {
            fn_name = fn->m_name;
        }
        if (sym_info[get_hash((ASR::asr_t*)fn)].intrinsic_function) {
            if (fn_name == "size") {
                LCOMPILERS_ASSERT(x.n_args > 0);
                self().visit_expr(*x.m_args[0].m_value);
                std::string var_name = src;
                std::string args;
                if (x.n_args == 1) {
                    args = "0";
                } else {
                    for (size_t i=1; i<x.n_args; i++) {
                        self().visit_expr(*x.m_args[i].m_value);
                        args += src + "-1";
                        if (i < x.n_args-1) args += ", ";
                    }
                }
                src = var_name + ".extent(" + args + ")";
            } else if (fn_name == "int") {
                LCOMPILERS_ASSERT(x.n_args > 0);
                self().visit_expr(*x.m_args[0].m_value);
                src = "(int)" + src;
            } else if (fn_name == "not") {
                LCOMPILERS_ASSERT(x.n_args > 0);
                self().visit_expr(*x.m_args[0].m_value);
                src = "!(" + src + ")";
            } else {
                throw CodeGenError("Intrinsic function '" + fn_name
                        + "' not implemented");
            }
        } else {
            if (fn_name == "main") {
                fn_name = "_xx_lcompilers_changed_main_xx";
            }
            src = fn_name + "(" + construct_call_args(fn, x.n_args, x.m_args) + ")";
        }
        last_expr_precedence = 2;
        if( ASR::is_a<ASR::List_t>(*x.m_type) ) {
            ASR::List_t* list_type = ASR::down_cast<ASR::List_t>(x.m_type);
            const_name += std::to_string(const_vars_count);
            const_vars_count += 1;
            const_name = current_scope->get_unique_name(const_name);
            std::string indent(indentation_level*indentation_spaces, ' ');
            tmp_buffer_src.push_back(check_tmp_buffer() + indent + c_ds_api->get_list_type(list_type) + " " +
                                const_name + " = " + src + ";\n");
            src = const_name;
            return;
        } else if( ASR::is_a<ASR::Dict_t>(*x.m_type) ) {
            ASR::Dict_t* dict_type = ASR::down_cast<ASR::Dict_t>(x.m_type);
            const_name += std::to_string(const_vars_count);
            const_vars_count += 1;
            const_name = current_scope->get_unique_name(const_name);
            std::string indent(indentation_level*indentation_spaces, ' ');
            tmp_buffer_src.push_back(check_tmp_buffer() + indent + c_ds_api->get_dict_type(dict_type) +
                                " " + const_name + " = " + src + ";\n");
            src = const_name;
            return;
        }
        src = check_tmp_buffer() + src;
    }

    void visit_SizeOfType(const ASR::SizeOfType_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        std::string c_type = CUtils::get_c_type_from_ttype_t(x.m_arg);
        src = "sizeof(" + c_type + ")";
    }

    void visit_StringSection(const ASR::StringSection_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        std::string arg, left, right, step, left_present, rig_present;
        arg = src;
        if (x.m_start) {
            self().visit_expr(*x.m_start);
            left = src;
            left_present = "true";
        } else {
            left = "0";
            left_present = "false";
        }
        if (x.m_end) {
            self().visit_expr(*x.m_end);
            right = src;
            rig_present = "true";
        } else {
            right = "0";
            rig_present = "false";
        }
        if (x.m_step) {
            self().visit_expr(*x.m_step);
            step = src;
        } else {
            step = "1";
        }
        src = "_lfortran_str_slice(" + arg + ", " + left + ", " + right + ", " + \
                    step + ", " + left_present + ", " + rig_present + ")";
    }

    void visit_StringChr(const ASR::StringChr_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        src = "_lfortran_str_chr(" + src + ")";
    }

    void visit_StringOrd(const ASR::StringOrd_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        if (ASR::is_a<ASR::StringConstant_t>(*x.m_arg)) {
            src = "(int)" + src + "[0]";
        } else {
            src = "_lfortran_str_ord_c(" + src + ")";
        }
    }

    void visit_StringRepeat(const ASR::StringRepeat_t &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_left);
        std::string s = src;
        self().visit_expr(*x.m_right);
        std::string n = src;
        src = "_lfortran_strrepeat_c(" + s + ", " + n + ")";
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        std::string target;
        ASR::ttype_t* m_target_type = ASRUtils::expr_type(x.m_target);
        ASR::ttype_t* m_value_type = ASRUtils::expr_type(x.m_value);
        bool is_target_list = ASR::is_a<ASR::List_t>(*m_target_type);
        bool is_value_list = ASR::is_a<ASR::List_t>(*m_value_type);
        bool is_target_tup = ASR::is_a<ASR::Tuple_t>(*m_target_type);
        bool is_value_tup = ASR::is_a<ASR::Tuple_t>(*m_value_type);
        bool is_target_dict = ASR::is_a<ASR::Dict_t>(*m_target_type);
        bool is_value_dict = ASR::is_a<ASR::Dict_t>(*m_value_type);
        bool alloc_return_var = false;
        std::string indent(indentation_level*indentation_spaces, ' ');
        if (ASRUtils::is_simd_array(x.m_target)) {
            this->visit_expr(*x.m_target);
            target = src;
            if (ASR::is_a<ASR::Var_t>(*x.m_value) ||
                    ASR::is_a<ASR::ArraySection_t>(*x.m_value)) {
                std::string arr_element_type = CUtils::get_c_type_from_ttype_t(
                    ASRUtils::expr_type(x.m_value));
                std::string size = std::to_string(ASRUtils::get_fixed_size_of_array(
                    ASRUtils::expr_type(x.m_target)));
                std::string value;
                if (ASR::is_a<ASR::ArraySection_t>(*x.m_value)) {
                    ASR::ArraySection_t *arr = ASR::down_cast<ASR::ArraySection_t>(x.m_value);
                    this->visit_expr(*arr->m_v);
                    value = src;
                    if(!ASR::is_a<ASR::ArrayBound_t>(*arr->m_args->m_left)) {
                        this->visit_expr(*arr->m_args->m_left);
                        int n_dims = ASRUtils::extract_n_dims_from_ttype(arr->m_type) - 1;
                        value += "->data + (" + src + " - "+ value +"->dims["
                            + std::to_string(n_dims) +"].lower_bound)";
                    } else {
                        value += "->data";
                    }
                } else if (ASR::is_a<ASR::Var_t>(*x.m_value)) {
                    this->visit_expr(*x.m_value);
                    value = src + "->data";
                }
                src = indent + "memcpy(&"+ target +", "+ value +", sizeof("
                    + arr_element_type + ") * "+ size +");\n";
                return;
            }
        } else if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            ASR::Var_t* x_m_target = ASR::down_cast<ASR::Var_t>(x.m_target);
            visit_Var(*x_m_target);
            target = src;
            if (!is_c && ASRUtils::is_array(ASRUtils::expr_type(x.m_target))) {
                target += "->data";
            }
            if (target == "_lpython_return_variable" && ASRUtils::is_character(*m_target_type)) {
                // ASR assigns return variable only once at the end of function
                alloc_return_var = true;
            }
        } else if (ASR::is_a<ASR::ArrayItem_t>(*x.m_target)) {
            self().visit_ArrayItem(*ASR::down_cast<ASR::ArrayItem_t>(x.m_target));
            target = src;
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*x.m_target)) {
            visit_StructInstanceMember(*ASR::down_cast<ASR::StructInstanceMember_t>(x.m_target));
            target = src;
        } else if (ASR::is_a<ASR::UnionInstanceMember_t>(*x.m_target)) {
            visit_UnionInstanceMember(*ASR::down_cast<ASR::UnionInstanceMember_t>(x.m_target));
            target = src;
        } else if (ASR::is_a<ASR::ListItem_t>(*x.m_target)) {
            self().visit_ListItem(*ASR::down_cast<ASR::ListItem_t>(x.m_target));
            target = src;
        } else if (ASR::is_a<ASR::TupleItem_t>(*x.m_target)) {
            self().visit_TupleItem(*ASR::down_cast<ASR::TupleItem_t>(x.m_target));
            target = src;
        } else if (ASR::is_a<ASR::TupleConstant_t>(*x.m_target)) {
            ASR::TupleConstant_t *tup_c = ASR::down_cast<ASR::TupleConstant_t>(x.m_target);
            std::string src_tmp = "", val_name = "";
            if (ASR::is_a<ASR::TupleConstant_t>(*x.m_value)) {
                ASR::TupleConstant_t *tup_const = ASR::down_cast<ASR::TupleConstant_t>(x.m_value);
                self().visit_TupleConstant(*tup_const);
                val_name = const_var_names[get_hash((ASR::asr_t*)tup_const)];
            } else if (ASR::is_a<ASR::FunctionCall_t>(*x.m_value)) {
                self().visit_FunctionCall(*ASR::down_cast<ASR::FunctionCall_t>(x.m_value));
                ASR::Tuple_t* t = ASR::down_cast<ASR::Tuple_t>(tup_c->m_type);
                std::string tuple_type_c = c_ds_api->get_tuple_type(t);
                const_name += std::to_string(const_vars_count);
                const_vars_count += 1;
                const_name = current_scope->get_unique_name(const_name);
                src_tmp += indent + tuple_type_c + " " + const_name + " = " + src + ";\n";
                val_name = const_name;
            } else {
                visit_Var(*ASR::down_cast<ASR::Var_t>(x.m_value));
                val_name = src;
            }
            for (size_t i=0; i<tup_c->n_elements; i++) {
                self().visit_expr(*tup_c->m_elements[i]);
                ASR::ttype_t *t = ASRUtils::expr_type(tup_c->m_elements[i]);
                src_tmp += indent + c_ds_api->get_deepcopy(t,
                        val_name + ".element_" + std::to_string(i), src) + "\n";
            }
            src = check_tmp_buffer() + src_tmp;
            return;
        } else if (ASR::is_a<ASR::DictItem_t>(*x.m_target)) {
            self().visit_DictItem(*ASR::down_cast<ASR::DictItem_t>(x.m_target));
            target = src;
        } else {
            LCOMPILERS_ASSERT(false)
        }
        from_std_vector_helper.clear();
        if( ASR::is_a<ASR::UnionConstructor_t>(*x.m_value) ) {
            src = "";
            return ;
        }
        self().visit_expr(*x.m_value);
        std::string value = src;
        ASR::ttype_t* value_type = ASRUtils::expr_type(x.m_value);
        if( ASR::is_a<ASR::StructType_t>(*value_type) ) {
             if (ASR::is_a<ASR::ArrayItem_t>(*x.m_value) ||
                 ASR::is_a<ASR::StructInstanceMember_t>(*x.m_value) ||
                 ASR::is_a<ASR::UnionInstanceMember_t>(*x.m_value)) {
                 value = "&" + value;
             }
        }
        if( ASR::is_a<ASR::StructType_t>(*m_target_type) ) {
             if (ASR::is_a<ASR::ArrayItem_t>(*x.m_target) ||
                 ASR::is_a<ASR::StructInstanceMember_t>(*x.m_target) ||
                 ASR::is_a<ASR::UnionInstanceMember_t>(*x.m_target)) {
                 target = "&" + target;
             }
        }
        if( !from_std_vector_helper.empty() ) {
            src = from_std_vector_helper;
        } else {
            src.clear();
        }
        src = check_tmp_buffer();
        if( is_target_list && is_value_list ) {
            ASR::List_t* list_target = ASR::down_cast<ASR::List_t>(ASRUtils::expr_type(x.m_target));
            std::string list_dc_func = c_ds_api->get_list_deepcopy_func(list_target);
            if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                ASR::symbol_t *target_sym = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
                if (ASR::is_a<ASR::Variable_t>(*target_sym)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(target_sym);
                    if (v->m_intent == ASRUtils::intent_out) {
                        src += indent + list_dc_func + "(&" + value + ", " + target + ");\n\n";
                    } else {
                        src += indent + list_dc_func + "(&" + value + ", &" + target + ");\n\n";
                    }
                }
            } else {
                src += indent + list_dc_func + "(&" + value + ", &" + target + ");\n\n";
            }
        } else if ( is_target_tup && is_value_tup ) {
            ASR::Tuple_t* tup_target = ASR::down_cast<ASR::Tuple_t>(ASRUtils::expr_type(x.m_target));
            std::string dc_func = c_ds_api->get_tuple_deepcopy_func(tup_target);
            if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                ASR::symbol_t *target_sym = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
                if (ASR::is_a<ASR::Variable_t>(*target_sym)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(target_sym);
                    if (v->m_intent == ASRUtils::intent_out) {
                        src += indent + dc_func + "(" + value + ", " + target + ");\n\n";
                    } else {
                        src += indent + dc_func + "(" + value + ", &" + target + ");\n\n";
                    }
                }
            } else {
                src += indent + dc_func + "(" + value + ", &" + target + ");\n\n";
            }

        } else if ( is_target_dict && is_value_dict ) {
            ASR::Dict_t* d_target = ASR::down_cast<ASR::Dict_t>(ASRUtils::expr_type(x.m_target));
            std::string dc_func = c_ds_api->get_dict_deepcopy_func(d_target);
            if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                ASR::symbol_t *target_sym = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
                if (ASR::is_a<ASR::Variable_t>(*target_sym)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(target_sym);
                    if (v->m_intent == ASRUtils::intent_out) {
                        src += indent + dc_func + "(&" + value + ", " + target + ");\n\n";
                    } else {
                        src += indent + dc_func + "(&" + value + ", &" + target + ");\n\n";
                    }
                }
            } else {
                src += indent + dc_func + "(&" + value + ", &" + target + ");\n\n";
            }

        } else {
            if( is_c ) {
                std::string alloc = "";
                if (alloc_return_var) {
                    // char * return variable;
                     alloc = indent + target + " = NULL;\n";
                }
                if( ASRUtils::is_array(m_target_type) && ASRUtils::is_array(m_value_type) ) {
                    ASR::dimension_t* m_target_dims = nullptr;
                    size_t n_target_dims = ASRUtils::extract_dimensions_from_ttype(m_target_type, m_target_dims);
                    ASR::dimension_t* m_value_dims = nullptr;
                    size_t n_value_dims = ASRUtils::extract_dimensions_from_ttype(m_value_type, m_value_dims);
                    bool is_target_data_only_array = ASRUtils::is_fixed_size_array(m_target_dims, n_target_dims) &&
                                                     ASR::is_a<ASR::Struct_t>(*ASRUtils::get_asr_owner(x.m_target));
                    bool is_value_data_only_array = ASRUtils::is_fixed_size_array(m_value_dims, n_value_dims) &&
                                                    ASRUtils::get_asr_owner(x.m_value) && ASR::is_a<ASR::Struct_t>(*ASRUtils::get_asr_owner(x.m_value));
                    if( is_target_data_only_array || is_value_data_only_array ) {
                        int64_t target_size = -1, value_size = -1;
                        if( !is_target_data_only_array ) {
                            target = target + "->data";
                        } else {
                            target_size = ASRUtils::get_fixed_size_of_array(m_target_dims, n_target_dims);
                        }
                        if( !is_value_data_only_array ) {
                            value = value + "->data";
                        } else {
                            value_size = ASRUtils::get_fixed_size_of_array(m_value_dims, n_value_dims);
                        }
                        if( target_size != -1 && value_size != -1 ) {
                            LCOMPILERS_ASSERT(target_size == value_size);
                        }
                        int64_t array_size = -1;
                        if( target_size != -1 ) {
                            array_size = target_size;
                        } else {
                            array_size = value_size;
                        }
                        src += indent + "memcpy(" + target + ", " + value + ", " + std::to_string(array_size) + "*sizeof(" +
                                    CUtils::get_c_type_from_ttype_t(m_target_type) + "));\n";
                    } else {
                        src += alloc + indent + c_ds_api->get_deepcopy(m_target_type, value, target) + "\n";
                    }
                } else {
                    src += alloc + indent + c_ds_api->get_deepcopy(m_target_type, value, target) + "\n";
                }
            } else {
                src += indent + c_ds_api->get_deepcopy(m_target_type, value, target) + "\n";
            }
        }
        from_std_vector_helper.clear();
    }

    std::string cmo_convertor_single_element(
        std::string arr, std::vector<std::string>& m_args,
        int n_args, bool check_for_bounds) {
        std::string dim_des_arr_ptr = arr + "->dims";
        std::string idx = "0";
        for( int r = 0; r < n_args; r++ ) {
            std::string curr_llvm_idx = m_args[r];
            std::string dim_des_ptr = dim_des_arr_ptr + "[" + std::to_string(r) + "]";
            std::string lval = dim_des_ptr + ".lower_bound";
            curr_llvm_idx = "(" + curr_llvm_idx + " - " + lval + ")";
            if( check_for_bounds ) {
                // check_single_element(curr_llvm_idx, arr); TODO: To be implemented
            }
            std::string stride = dim_des_ptr + ".stride";
            idx = "(" + idx + " + (" + stride + " * " + curr_llvm_idx + "))";
        }
        std::string offset_val = arr + "->offset";
        return "(" + idx + " + " + offset_val + ")";
    }

    std::string cmo_convertor_single_element_data_only(
        std::vector<std::string>& diminfo, std::vector<std::string>& m_args,
        int n_args, bool check_for_bounds, bool is_unbounded_pointer_to_data) {
        std::string prod = "1";
        std::string idx = "0";
        if (is_unbounded_pointer_to_data) {
            for (int r = 0; r < n_args; r++) {
                std::string curr_llvm_idx = m_args[r];
                std::string lval = diminfo[r];
                curr_llvm_idx = "(" + curr_llvm_idx + " - " + lval + ")";
                if( check_for_bounds ) {
                    // check_single_element(curr_llvm_idx, arr); TODO: To be implemented
                }
                idx = "(" + idx + " + " + "(" + curr_llvm_idx + ")" + ")";
            }
            return idx;
        }
        for( int r = n_args - 1, r1 = 2 * n_args - 1; r >= 0; r--, r1 -= 2) {
            std::string curr_llvm_idx = m_args[r];
            std::string lval = diminfo[r1 - 1];
            curr_llvm_idx = "(" + curr_llvm_idx + " - " + lval + ")";
            if( check_for_bounds ) {
                // check_single_element(curr_llvm_idx, arr); TODO: To be implemented
            }
            idx = "(" + idx + " + " + "(" + prod + " * " + curr_llvm_idx + ")" + ")";
            std::string dim_size = diminfo[r1];
            prod = "(" + prod + " * " + dim_size + ")";
        }
        return idx;
    }

    std::string arr_get_single_element(std::string array,
        std::vector<std::string>& m_args, int n_args, bool data_only,
        bool is_fixed_size, std::vector<std::string>& diminfo, bool is_unbounded_pointer_to_data) {
        std::string tmp = "";
        // TODO: Uncomment later
        // bool check_for_bounds = is_explicit_shape(v);
        bool check_for_bounds = false;
        std::string idx = "";
        if( data_only || is_fixed_size ) {
            LCOMPILERS_ASSERT(diminfo.size() > 0);
            idx = cmo_convertor_single_element_data_only(diminfo, m_args, n_args, check_for_bounds, is_unbounded_pointer_to_data);
            if( is_fixed_size ) {
                tmp = array + "->data[" + idx + "]" ;
            } else {
                tmp = array + "->data[" + idx + "]";
            }
        } else {
            idx = cmo_convertor_single_element(array, m_args, n_args, check_for_bounds);
            std::string full_array = array + "->data";
            tmp = full_array + "[" + idx + "]";
        }
        return tmp;
    }

    void fill_descriptor_for_array_section_data_only(std::string value_desc, std::string target_desc,
        std::vector<std::string>& lbs, std::vector<std::string>& ubs, std::vector<std::string>& ds, std::vector<std::string>& non_sliced_indices,
        std::vector<std::string>& diminfo, int value_rank, int target_rank) {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::vector<std::string> section_first_indices;
            for( int i = 0; i < value_rank; i++ ) {
                if( ds[i] != "" ) {
                    LCOMPILERS_ASSERT(lbs[i] != "");
                    section_first_indices.push_back(lbs[i]);
                } else {
                    LCOMPILERS_ASSERT(non_sliced_indices[i] != "");
                    section_first_indices.push_back(non_sliced_indices[i]);
                }
            }
            std::string target_offset = cmo_convertor_single_element_data_only(
                diminfo, section_first_indices, value_rank, false, false);

            value_desc = "(" + value_desc + " + " + target_offset + ")";
            std::string update_target_desc = "";
            update_target_desc += indent + target_desc + "->data = " + value_desc + ";\n";

            update_target_desc += indent + target_desc + "->offset = 0;\n"; // offset not available yet

            std::string target_dim_des_array = target_desc + "->dims";
            int j = target_rank - 1;
            int r = (int)diminfo.size() - 1;
            std::string stride = "1";
            for( int i = value_rank - 1; i >= 0; i-- ) {
                if( ds[i] != "" ) {
                    std::string dim_length = "((( (" + ubs[i] + ") - (" + lbs[i] + ") )" + "/" + ds[i] + ") + 1)";
                    std::string target_dim_des = target_dim_des_array + "[" + std::to_string(j) + "]";
                    update_target_desc += indent + target_dim_des + ".stride = " + stride + ";\n";
                    update_target_desc += indent + target_dim_des + ".lower_bound = 1;\n";
                    update_target_desc += indent + target_dim_des + ".length = " + dim_length + ";\n";
                    j--;
                }
                stride = "(" + stride + "*" + diminfo[r] + ")";
                r -= 2;
            }
            LCOMPILERS_ASSERT(j == -1);
            update_target_desc += indent + target_desc + "->n_dims = " + std::to_string(target_rank) + ";\n";
            src = update_target_desc;
    }

    void handle_array_section_association_to_pointer(const ASR::Associate_t& x) {
        ASR::ArraySection_t* array_section = ASR::down_cast<ASR::ArraySection_t>(x.m_value);
        self().visit_expr(*array_section->m_v);
        std::string value_desc = src;

        self().visit_expr(*x.m_target);
        std::string target_desc = src;

        int value_rank = array_section->n_args, target_rank = 0;
        std::vector<std::string> lbs(value_rank);
        std::vector<std::string> ubs(value_rank);
        std::vector<std::string> ds(value_rank);
        std::vector<std::string> non_sliced_indices(value_rank);
        for( int i = 0; i < value_rank; i++ ) {
            lbs[i] = ""; ubs[i] = ""; ds[i] = "";
            non_sliced_indices[i] = "";
            if( array_section->m_args[i].m_step != nullptr ) {
                self().visit_expr(*array_section->m_args[i].m_left);
                lbs[i] = src;
                self().visit_expr(*array_section->m_args[i].m_right);
                ubs[i] = src;
                self().visit_expr(*array_section->m_args[i].m_step);
                ds[i] = src;
                target_rank++;
            } else {
                self().visit_expr(*array_section->m_args[i].m_right);
                non_sliced_indices[i] = src;
            }
        }
        LCOMPILERS_ASSERT(target_rank > 0);

        ASR::ttype_t* array_type = ASRUtils::expr_type(array_section->m_v);
        if( ASRUtils::extract_physical_type(array_type) == ASR::array_physical_typeType::PointerToDataArray ||
            ASRUtils::extract_physical_type(array_type) == ASR::array_physical_typeType::FixedSizeArray ) {
            value_desc = value_desc + "->data";
            ASR::dimension_t* m_dims = nullptr;
            // Fill in m_dims:
            [[maybe_unused]] int array_value_rank = ASRUtils::extract_dimensions_from_ttype(array_type, m_dims);
            LCOMPILERS_ASSERT(array_value_rank == value_rank);
            std::vector<std::string> diminfo;
            diminfo.reserve(value_rank * 2);
            for( int i = 0; i < value_rank; i++ ) {
                self().visit_expr(*m_dims[i].m_start);
                diminfo.push_back(src);
                self().visit_expr(*m_dims[i].m_length);
                diminfo.push_back(src);
            }
            fill_descriptor_for_array_section_data_only(value_desc, target_desc,
                lbs, ubs, ds, non_sliced_indices,
                diminfo, value_rank, target_rank);
        } else {
            throw CodeGenError("Only Pointer to Data Array or Fixed Size array supported for now");
        }
    }

    void visit_Associate(const ASR::Associate_t &x) {
        if (ASR::is_a<ASR::ArraySection_t>(*x.m_value)) {
            handle_array_section_association_to_pointer(x);
        } else {
            throw CodeGenError("Associate only implemented for ArraySection so far");
        }
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        src = std::to_string(x.m_n);
        last_expr_precedence = 2;
    }

    void visit_UnsignedIntegerConstant(const ASR::UnsignedIntegerConstant_t &x) {
        src = std::to_string(x.m_n);
        last_expr_precedence = 2;
    }

    void visit_RealConstant(const ASR::RealConstant_t &x) {
        // TODO: remove extra spaces from the front of double_to_scientific result
        src = double_to_scientific(x.m_r);
        last_expr_precedence = 2;
    }


    void visit_StringConstant(const ASR::StringConstant_t &x) {
        src = "\"" + str_escape_c(x.m_s) + "\"";
        last_expr_precedence = 2;
    }

    void visit_StringConcat(const ASR::StringConcat_t& x) {
        is_string_concat_present = true;
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        self().visit_expr(*x.m_right);
        std::string right = std::move(src);
        if( is_c ) {
            src = "strcat_(" + left + ", " + right +")";
        } else {
            src = left + " + " + right;
        }
    }

    void visit_ListConstant(const ASR::ListConstant_t& x) {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string tab(indentation_spaces, ' ');
        const_name += std::to_string(const_vars_count);
        const_vars_count += 1;
        const_name = current_scope->get_unique_name(const_name);
        std::string var_name = const_name;
        const_var_names[get_hash((ASR::asr_t*)&x)] = var_name;
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(x.m_type);
        std::string list_type_c = c_ds_api->get_list_type(t);
        std::string src_tmp = "";
        src_tmp += indent + list_type_c + " " + var_name + ";\n";
        std::string list_init_func = c_ds_api->get_list_init_func(t);
        src_tmp += indent + list_init_func + "(&" + var_name + ", " +
               std::to_string(x.n_args) + ");\n";
        for( size_t i = 0; i < x.n_args; i++ ) {
            self().visit_expr(*x.m_args[i]);
            if( ASR::is_a<ASR::String_t>(*t->m_type) ) {
                src_tmp += indent + var_name + ".data[" + std::to_string(i) +"] = NULL;\n";
            }
            src_tmp += indent + c_ds_api->get_deepcopy(t->m_type, src,
                        var_name + ".data[" + std::to_string(i) +"]") + "\n";
        }
        src_tmp += indent + var_name + ".current_end_point = " + std::to_string(x.n_args) + ";\n";
        src = var_name;
        tmp_buffer_src.push_back(src_tmp);
    }

    void visit_TupleConstant(const ASR::TupleConstant_t& x) {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string tab(indentation_spaces, ' ');
        const_name += std::to_string(const_vars_count);
        const_vars_count += 1;
        const_name = current_scope->get_unique_name(const_name);
        std::string var_name = const_name;
        const_var_names[get_hash((ASR::asr_t*)&x)] = var_name;
        ASR::Tuple_t* t = ASR::down_cast<ASR::Tuple_t>(x.m_type);
        std::string tuple_type_c = c_ds_api->get_tuple_type(t);
        std::string src_tmp = "";
        src_tmp += indent + tuple_type_c + " " + var_name + ";\n";
        for (size_t i = 0; i < x.n_elements; i++) {
            self().visit_expr(*x.m_elements[i]);
            std::string ele = ".element_" + std::to_string(i);
            if (ASR::is_a<ASR::String_t>(*t->m_type[i])) {
                src_tmp += indent + var_name + ele + " = NULL;\n";
            }
            src_tmp += indent + c_ds_api->get_deepcopy(t->m_type[i], src, var_name + ele) + "\n";
        }
        src_tmp += indent + var_name + ".length" + " = " + std::to_string(x.n_elements) + ";\n";
        src = var_name;
        tmp_buffer_src.push_back(src_tmp);
    }

    void visit_DictConstant(const ASR::DictConstant_t& x) {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string tab(indentation_spaces, ' ');
        const_name += std::to_string(const_vars_count);
        const_vars_count += 1;
        const_name = current_scope->get_unique_name(const_name);
        std::string var_name = const_name;
        const_var_names[get_hash((ASR::asr_t*)&x)] = var_name;
        ASR::Dict_t* t = ASR::down_cast<ASR::Dict_t>(x.m_type);
        std::string dict_type_c = c_ds_api->get_dict_type(t);
        std::string src_tmp = "";
        src_tmp += indent + dict_type_c + " " + var_name + ";\n";
        std::string dict_init_func = c_ds_api->get_dict_init_func(t);
        std::string dict_ins_func = c_ds_api->get_dict_insert_func(t);
        src_tmp += indent + dict_init_func + "(&" + var_name + ", " +
               std::to_string(x.n_keys) + " + 1);\n";
        for ( size_t i = 0; i < x.n_keys; i++ ) {
            self().visit_expr(*x.m_keys[i]);
            std::string k, v;
            k = std::move(src);
            self().visit_expr(*x.m_values[i]);
            v = std::move(src);
            src_tmp += indent + dict_ins_func + "(&" + var_name + ", " +\
                                k + ", " + v + ");\n";
        }
        src = var_name;
        tmp_buffer_src.push_back(src_tmp);
    }

    void visit_TupleCompare(const ASR::TupleCompare_t& x) {
        ASR::ttype_t* type = ASRUtils::expr_type(x.m_left);
        std::string tup_cmp_func = c_ds_api->get_compare_func(type);
        bracket_open++;
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        self().visit_expr(*x.m_right);
        std::string right = std::move(src);
        bracket_open--;
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = tup_cmp_func + "(" + left + ", " + right + ")";
        if (x.m_op == ASR::cmpopType::NotEq) {
            src = "!" + src;
        }
        src = check_tmp_buffer() + src;
    }

    void visit_DictInsert(const ASR::DictInsert_t& x) {
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::Dict_t* t = ASR::down_cast<ASR::Dict_t>(t_ttype);
        std::string dict_insert_fun = c_ds_api->get_dict_insert_func(t);
        self().visit_expr(*x.m_a);
        std::string d_var = std::move(src);
        self().visit_expr(*x.m_key);
        std::string key = std::move(src);
        self().visit_expr(*x.m_value);
        std::string val = std::move(src);
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = indent + dict_insert_fun + "(&" + d_var + ", " + key + ", " + val + ");\n";
    }

    void visit_DictItem(const ASR::DictItem_t& x) {
        ASR::Dict_t* dict_type = ASR::down_cast<ASR::Dict_t>(
                                    ASRUtils::expr_type(x.m_a));
        this->visit_expr(*x.m_a);
        std::string d_var = std::move(src);

        this->visit_expr(*x.m_key);
        std::string k = std::move(src);

        if (x.m_default) {
            this->visit_expr(*x.m_default);
            std::string def_value = std::move(src);
            std::string dict_get_fun = c_ds_api->get_dict_get_func(dict_type,
                                                                    true);
            src = dict_get_fun + "(&" + d_var + ", " + k + ", " + def_value + ")";
        } else {
            std::string dict_get_fun = c_ds_api->get_dict_get_func(dict_type);
            src = dict_get_fun + "(&" + d_var + ", " + k + ")";
        }
    }

    void visit_ListAppend(const ASR::ListAppend_t& x) {
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(t_ttype);
        std::string list_append_func = c_ds_api->get_list_append_func(t);
        bracket_open++;
        self().visit_expr(*x.m_a);
        std::string list_var = std::move(src);
        self().visit_expr(*x.m_ele);
        std::string element = std::move(src);
        bracket_open--;
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = check_tmp_buffer();
        src += indent + list_append_func + "(&" + list_var + ", " + element + ");\n";
    }

    void visit_ListConcat(const ASR::ListConcat_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(x.m_type);
        std::string list_concat_func = c_ds_api->get_list_concat_func(t);
        bracket_open++;
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        self().visit_expr(*x.m_right);
        bracket_open--;
        std::string rig = std::move(src);
        tmp_buffer_src.push_back(check_tmp_buffer());
        src = "(*" + list_concat_func + "(&" + left + ", &" + rig + "))";
    }

    void visit_ListSection(const ASR::ListSection_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        std::string left, right, step, l_present, r_present;
        bracket_open++;
        if (x.m_section.m_left) {
            self().visit_expr(*x.m_section.m_left);
            left = src;
            l_present = "true";
        } else {
            left = "0";
            l_present = "false";
        }
        if (x.m_section.m_right) {
            self().visit_expr(*x.m_section.m_right);
            right = src;
            r_present = "true";
        } else {
            right = "0";
            r_present = "false";
        }
        if (x.m_section.m_step) {
            self().visit_expr(*x.m_section.m_step);
            step = src;
        } else {
            step = "1";
        }
        self().visit_expr(*x.m_a);
        bracket_open--;
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(t_ttype);
        std::string list_var = std::move(src);
        std::string list_type_c = c_ds_api->get_list_type(t);
        std::string list_section_func = c_ds_api->get_list_section_func(t);
        std::string indent(indentation_level * indentation_spaces, ' ');
        const_name += std::to_string(const_vars_count);
        const_vars_count += 1;
        const_name = current_scope->get_unique_name(const_name);
        std::string var_name = const_name, tmp_src_gen = "";
        tmp_src_gen = indent + list_type_c + "* " + var_name + " = ";
        tmp_src_gen += list_section_func + "(&" + list_var + ", " + left + ", " +
            right + ", " + step + ", " + l_present + ", " + r_present + ");\n";
        const_var_names[get_hash((ASR::asr_t*)&x)] = var_name;
        tmp_buffer_src.push_back(tmp_src_gen);
        src = "(* " + var_name + ")";
    }

    void visit_ListClear(const ASR::ListClear_t& x) {
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(t_ttype);
        std::string list_clear_func = c_ds_api->get_list_clear_func(t);
        bracket_open++;
        self().visit_expr(*x.m_a);
        bracket_open--;
        std::string list_var = std::move(src);
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = check_tmp_buffer() + indent + list_clear_func + "(&" + list_var + ");\n";
    }

    void visit_ListRepeat(const ASR::ListRepeat_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(x.m_type);
        std::string list_repeat_func = c_ds_api->get_list_repeat_func(t);
        bracket_open++;
        self().visit_expr(*x.m_left);
        std::string list_var = std::move(src);
        self().visit_expr(*x.m_right);
        std::string freq = std::move(src);
        bracket_open--;
        tmp_buffer_src.push_back(check_tmp_buffer());
        src = "(*" + list_repeat_func + "(&" + list_var + ", " + freq + "))";
    }

    void visit_ListCompare(const ASR::ListCompare_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        ASR::ttype_t* type = ASRUtils::expr_type(x.m_left);
        std::string list_cmp_func = c_ds_api->get_compare_func(type);
        bracket_open++;
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        self().visit_expr(*x.m_right);
        bracket_open--;
        std::string right = std::move(src), tmp_gen= "";
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string val = list_cmp_func + "(" + left + ", " + right + ")";
        if (x.m_op == ASR::cmpopType::NotEq) {
            val = "!" + val;
        }
        src = check_tmp_buffer() + val;
    }

    void visit_ListInsert(const ASR::ListInsert_t& x) {
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(t_ttype);
        std::string list_insert_func = c_ds_api->get_list_insert_func(t);
        bracket_open++;
        self().visit_expr(*x.m_a);
        std::string list_var = std::move(src);
        self().visit_expr(*x.m_ele);
        std::string element = std::move(src);
        self().visit_expr(*x.m_pos);
        bracket_open--;
        std::string pos = std::move(src);
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = check_tmp_buffer();
        src += indent + list_insert_func + "(&" + list_var + ", " + pos + ", " + element + ");\n";
    }

    void visit_ListRemove(const ASR::ListRemove_t& x) {
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::List_t* t = ASR::down_cast<ASR::List_t>(t_ttype);
        std::string list_remove_func = c_ds_api->get_list_remove_func(t);
        bracket_open++;
        self().visit_expr(*x.m_a);
        std::string list_var = std::move(src);
        self().visit_expr(*x.m_ele);
        bracket_open--;
        std::string element = std::move(src);
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = check_tmp_buffer();
        src += indent + list_remove_func + "(&" + list_var + ", " + element + ");\n";
    }

    void visit_ListLen(const ASR::ListLen_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        src = src + ".current_end_point";
    }

    void visit_TupleLen(const ASR::TupleLen_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        src = src + ".length";
    }

    void visit_DictLen(const ASR::DictLen_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_arg);
        ASR::Dict_t* t = ASR::down_cast<ASR::Dict_t>(t_ttype);
        std::string dict_len_fun = c_ds_api->get_dict_len_func(t);
        bracket_open++;
        self().visit_expr(*x.m_arg);
        src = dict_len_fun + "(&" + src + ")";
        bracket_open--;
    }

    void visit_DictPop(const ASR::DictPop_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        ASR::ttype_t* t_ttype = ASRUtils::expr_type(x.m_a);
        ASR::Dict_t* t = ASR::down_cast<ASR::Dict_t>(t_ttype);
        std::string dict_pop_fun = c_ds_api->get_dict_pop_func(t);
        bracket_open++;
        self().visit_expr(*x.m_a);
        std::string d = std::move(src);
        self().visit_expr(*x.m_key);
        std::string k = std::move(src);
        src = dict_pop_fun + "(&" + d + ", "  + k + ")";
        bracket_open--;
    }

    void visit_ListItem(const ASR::ListItem_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_a);
        std::string list_var = std::move(src);
        self().visit_expr(*x.m_pos);
        std::string pos = std::move(src);
        // TODO: check for out of bound indices
        src = list_var + ".data[" + pos + "]";
    }

    void visit_TupleItem(const ASR::TupleItem_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_a);
        std::string tup_var = std::move(src);
        ASR::expr_t *pos_val = ASRUtils::expr_value(x.m_pos);
        if (pos_val == nullptr) {
            throw CodeGenError("Compile time constant values are supported in Tuple Item yet");
        }
        self().visit_expr(*pos_val);
        std::string pos = std::move(src);
        // TODO: check for out of bound indices
        src = tup_var + ".element_" + pos;
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        if (x.m_value == true) {
            src = "true";
        } else {
            src = "false";
        }
        last_expr_precedence = 2;
    }

    void visit_Var(const ASR::Var_t &x) {
        const ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (ASR::is_a<ASR::Function_t>(*s)) {
            src = ASRUtils::symbol_name(s);
            return;
        }
        ASR::Variable_t* sv = ASR::down_cast<ASR::Variable_t>(s);
        if (is_c) {
            if ((sv->m_intent == ASRUtils::intent_in
                || sv->m_intent == ASRUtils::intent_inout)
                && ASRUtils::is_array(sv->m_type)
                && ASRUtils::is_pointer(sv->m_type)) {
                src = "(*" + std::string(ASR::down_cast<ASR::Variable_t>(s)->m_name) + ")";
            } else if ((sv->m_intent == ASRUtils::intent_inout
                || sv->m_intent == ASRUtils::intent_out)
                && !ASRUtils::is_aggregate_type(sv->m_type)) {
                src = "(*" + std::string(ASR::down_cast<ASR::Variable_t>(s)->m_name) + ")";
            } else {
                src = std::string(ASR::down_cast<ASR::Variable_t>(s)->m_name);
            }
        } else {
            src = std::string(ASR::down_cast<ASR::Variable_t>(s)->m_name);
        }
        last_expr_precedence = 2;
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        std::string der_expr, member;
        this->visit_expr(*x.m_v);
        der_expr = std::move(src);
        member = ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(x.m_m));
        if( ASR::is_a<ASR::ArrayItem_t>(*x.m_v) ||
            ASR::is_a<ASR::UnionInstanceMember_t>(*x.m_v) ||
            ASR::is_a<ASR::StructInstanceMember_t>(*x.m_v) ) {
            src = der_expr + "." + member;
        } else {
            src = der_expr + "->" + member;
        }
    }

    void visit_UnionInstanceMember(const ASR::UnionInstanceMember_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        std::string der_expr, member;
        this->visit_expr(*x.m_v);
        der_expr = std::move(src);
        member = ASRUtils::symbol_name(x.m_m);
        src = der_expr + "." + member;
    }

    void visit_Cast(const ASR::Cast_t &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        switch (x.m_kind) {
            case (ASR::cast_kindType::IntegerToReal) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                switch (dest_kind) {
                    case 4: src = "(float)(" + src + ")"; break;
                    case 8: src = "(double)(" + src + ")"; break;
                    default: throw CodeGenError("Cast IntegerToReal: Unsupported Kind " + std::to_string(dest_kind));
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::RealToInteger) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src = "(int" + std::to_string(dest_kind * 8) + "_t)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::RealToReal) : {
                // In C++, we do not need to cast float to float explicitly:
                // src = src;
                break;
            }
            case (ASR::cast_kindType::IntegerToInteger) :
            case (ASR::cast_kindType::UnsignedIntegerToUnsignedInteger) : {
                // In C++, we do not need to cast int <-> long long explicitly:
                // we also do not need to cast uint8_t <-> uint32_t explicitly:
                // src = src;
                break;
            }
            case (ASR::cast_kindType::IntegerToUnsignedInteger) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src = "(uint" + std::to_string(dest_kind * 8) + "_t)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::RealToUnsignedInteger) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src = "(uint" + std::to_string(dest_kind * 8) + "_t)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::UnsignedIntegerToInteger) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src = "(int" + std::to_string(dest_kind * 8) + "_t)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::UnsignedIntegerToReal) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                switch (dest_kind) {
                    case 4: src = "(float)(" + src + ")"; break;
                    case 8: src = "(double)(" + src + ")"; break;
                    default: throw CodeGenError("Cast IntegerToReal: Unsupported Kind " + std::to_string(dest_kind));
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::ComplexToComplex) : {
                break;
            }
            case (ASR::cast_kindType::IntegerToComplex) : {
                if (is_c) {
                    headers.insert("complex.h");
                    src = "CMPLX(" + src + ", 0)";
                } else {
                    src = "std::complex<double>(" + src + ")";
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::ComplexToReal) : {
                if (is_c) {
                    headers.insert("complex.h");
                    src = "creal(" + src + ")";
                } else {
                    src = "std::real(" + src + ")";
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::RealToComplex) : {
                if (is_c) {
                    headers.insert("complex.h");
                    src = "CMPLX(" + src + ", 0.0)";
                } else {
                    src = "std::complex<double>(" + src + ")";
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::LogicalToInteger) : {
                src = "(int)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::LogicalToString) : {
                src = "(" + src + " ? \"True\" : \"False\")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::IntegerToLogical) :
            case (ASR::cast_kindType::UnsignedIntegerToLogical) : {
                src = "(bool)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::LogicalToReal) : {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                switch (dest_kind) {
                    case 4: src = "(float)(" + src + ")"; break;
                    case 8: src = "(double)(" + src + ")"; break;
                    default: throw CodeGenError("Cast LogicalToReal: Unsupported Kind " + std::to_string(dest_kind));
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::RealToLogical) : {
                src = "(bool)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::StringToLogical) : {
                src = "(bool)(strlen(" + src + ") > 0)";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::ComplexToLogical) : {
                src = "(bool)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::IntegerToString) : {
                if (is_c) {
                    ASR::ttype_t *arg_type = ASRUtils::expr_type(x.m_arg);
                    int arg_kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    switch (arg_kind) {
                        case 1: src = "_lfortran_int_to_str1(" + src + ")"; break;
                        case 2: src = "_lfortran_int_to_str2(" + src + ")"; break;
                        case 4: src = "_lfortran_int_to_str4(" + src + ")"; break;
                        case 8: src = "_lfortran_int_to_str8(" + src + ")"; break;
                        default: throw CodeGenError("Cast IntegerToString: Unsupported Kind " + \
                                        std::to_string(arg_kind));
                    }

                } else {
                    src = "std::to_string(" + src + ")";
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::StringToInteger) : {
                if (is_c) {
                    src = "atoi(" + src + ")";
                } else {
                    src = "std::stoi(" + src + ")";
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::RealToString) : {
                if (is_c) {
                    ASR::ttype_t *arg_type = ASRUtils::expr_type(x.m_arg);
                    int arg_kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    switch (arg_kind) {
                        case 4: src = "_lfortran_float_to_str4(" + src + ")"; break;
                        case 8: src = "_lfortran_float_to_str8(" + src + ")"; break;
                        default: throw CodeGenError("Cast RealToString: Unsupported Kind " + \
                                        std::to_string(arg_kind));
                    }
                } else {
                    src = "std::to_string(" + src + ")";
                }
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::CPtrToUnsignedInteger) : {
                src = "(uint64_t)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            case (ASR::cast_kindType::UnsignedIntegerToCPtr) : {
                src = "(void*)(" + src + ")";
                last_expr_precedence = 2;
                break;
            }
            default : throw CodeGenError("Cast kind " + std::to_string(x.m_kind) + " not implemented",
                x.base.base.loc);
        }
    }

    void visit_IntegerBitLen(const ASR::IntegerBitLen_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_a);
        int arg_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        switch (arg_kind) {
            case 1: src = "_lpython_bit_length1(" + src + ")"; break;
            case 2: src = "_lpython_bit_length2(" + src + ")"; break;
            case 4: src = "_lpython_bit_length4(" + src + ")"; break;
            case 8: src = "_lpython_bit_length8(" + src + ")"; break;
            default: throw CodeGenError("Unsupported Integer Kind: " + \
                            std::to_string(arg_kind));
        }
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t &x) {
        handle_Compare(x);
    }

    void visit_UnsignedIntegerCompare(const ASR::UnsignedIntegerCompare_t &x) {
        handle_Compare(x);
    }

    void visit_RealCompare(const ASR::RealCompare_t &x) {
        handle_Compare(x);
    }

    void visit_ComplexCompare(const ASR::ComplexCompare_t &x) {
        handle_Compare(x);
    }

    void visit_LogicalCompare(const ASR::LogicalCompare_t &x) {
        handle_Compare(x);
    }

    void visit_StringCompare(const ASR::StringCompare_t &x) {
        handle_Compare(x);
    }

    void visit_CPtrCompare(const ASR::CPtrCompare_t &x) {
        handle_Compare(x);
    }

    template<typename T>
    void handle_Compare(const T &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        self().visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        switch (x.m_op) {
            case (ASR::cmpopType::Eq) : { last_expr_precedence = 10; break; }
            case (ASR::cmpopType::Gt) : { last_expr_precedence = 9;  break; }
            case (ASR::cmpopType::GtE) : { last_expr_precedence = 9; break; }
            case (ASR::cmpopType::Lt) : { last_expr_precedence = 9;  break; }
            case (ASR::cmpopType::LtE) : { last_expr_precedence = 9; break; }
            case (ASR::cmpopType::NotEq): { last_expr_precedence = 10; break; }
            default : LCOMPILERS_ASSERT(false); // should never happen
        }
        if (left_precedence <= last_expr_precedence) {
            src += left;
        } else {
            src += "(" + left + ")";
        }
        std::string op_str = ASRUtils::cmpop_to_str(x.m_op);
        if( T::class_type == ASR::exprType::StringCompare && is_c ) {
            src = "strcmp(" + left + ", " + right + ") " + op_str + " 0";
        } else {
            src += op_str;
            if (right_precedence <= last_expr_precedence) {
                src += right;
            } else {
                src += "(" + right + ")";
            }
        }
    }

    template<typename T>
    void handle_SU_IntegerBitNot(const T& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        int expr_precedence = last_expr_precedence;
        last_expr_precedence = 3;
        if (expr_precedence <= last_expr_precedence) {
            src = "~" + src;
        } else {
            src = "~(" + src + ")";
        }
    }

    void visit_IntegerBitNot(const ASR::IntegerBitNot_t& x) {
        handle_SU_IntegerBitNot(x);
    }

    void visit_UnsignedIntegerBitNot(const ASR::UnsignedIntegerBitNot_t& x) {
        handle_SU_IntegerBitNot(x);
    }

    void visit_IntegerUnaryMinus(const ASR::IntegerUnaryMinus_t &x) {
        handle_UnaryMinus(x);
    }

    void visit_UnsignedIntegerUnaryMinus(const ASR::UnsignedIntegerUnaryMinus_t &x) {
        handle_UnaryMinus(x);
        int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(x.m_arg));
        src = "(uint" + std::to_string(kind * 8) + "_t)" + src;
    }

    void visit_RealUnaryMinus(const ASR::RealUnaryMinus_t &x) {
        handle_UnaryMinus(x);
    }

    void visit_ComplexUnaryMinus(const ASR::ComplexUnaryMinus_t &x) {
        handle_UnaryMinus(x);
    }

    template <typename T>
    void handle_UnaryMinus(const T &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        int expr_precedence = last_expr_precedence;
        last_expr_precedence = 3;
        if (expr_precedence < last_expr_precedence) {
            src = "-" + src;
        } else {
            src = "-(" + src + ")";
        }
    }

    void visit_ComplexRe(const ASR::ComplexRe_t &x) {
        headers.insert("complex.h");
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        if (is_c) {
            src = "creal(" + src + ")";
        } else {
            src = src + ".real()";
        }
    }

    void visit_ComplexIm(const ASR::ComplexIm_t &x) {
        headers.insert("complex.h");
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        if (is_c) {
            src = "cimag(" + src + ")";
        } else {
            src = src + ".imag()";
        }
    }

    void visit_LogicalNot(const ASR::LogicalNot_t &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        int expr_precedence = last_expr_precedence;
        last_expr_precedence = 3;
        if (expr_precedence <= last_expr_precedence) {
            src = "!" + src;
        } else {
            src = "!(" + src + ")";
        }
    }

    void visit_PointerNullConstant(const ASR::PointerNullConstant_t& /*x*/) {
        src = "NULL";
    }

    void visit_GetPointer(const ASR::GetPointer_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        std::string arg_src = std::move(src);
        std::string addr_prefix = "&";
        if( ASRUtils::is_array(ASRUtils::expr_type(x.m_arg)) ||
            ASR::is_a<ASR::StructType_t>(*ASRUtils::expr_type(x.m_arg)) ) {
            addr_prefix.clear();
        }
        src = addr_prefix + arg_src;
    }

    void visit_PointerToCPtr(const ASR::PointerToCPtr_t& x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_arg);
        std::string arg_src = std::move(src);
        if( ASRUtils::is_array(ASRUtils::expr_type(x.m_arg)) ) {
            arg_src += "->data";
        }
        std::string type_src = CUtils::get_c_type_from_ttype_t(x.m_type);
        src = "(" + type_src + ") " + arg_src;
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        handle_BinOp(x);
    }

    void visit_UnsignedIntegerBinOp(const ASR::UnsignedIntegerBinOp_t &x) {
        handle_BinOp(x);
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        src = "(uint" + std::to_string(kind * 8) + "_t)(" + src + ")";
    }

    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        handle_BinOp(x);
    }

    void visit_ComplexBinOp(const ASR::ComplexBinOp_t &x) {
        handle_BinOp(x);
    }

    void visit_ComplexConstructor(const ASR::ComplexConstructor_t &x) {
        self().visit_expr(*x.m_re);
        std::string re = std::move(src);
        self().visit_expr(*x.m_im);
        std::string im = std::move(src);
        src = "CMPLX(" + re + "," + im + ")";
    }

    void visit_StructConstructor(const ASR::StructConstructor_t &x) {
        std::string out = "{";
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(x.m_dt_sym);
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value) {
                out += ".";
                out += st->m_members[i];
                out += " = ";
                self().visit_expr(*x.m_args[i].m_value);
                out += src;
                if (i < x.n_args-1) {
                    out += ", ";
                }
            }
        }
        out += "}";
        src = out;
    }

    template <typename T>
    void handle_BinOp(const T &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        self().visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        switch (x.m_op) {
            case (ASR::binopType::Add) : { last_expr_precedence = 6; break; }
            case (ASR::binopType::Sub) : { last_expr_precedence = 6; break; }
            case (ASR::binopType::Mul) : { last_expr_precedence = 5; break; }
            case (ASR::binopType::Div) : { last_expr_precedence = 5; break; }
            case (ASR::binopType::BitAnd) : { last_expr_precedence = 11; break; }
            case (ASR::binopType::BitOr) : { last_expr_precedence = 13; break; }
            case (ASR::binopType::BitXor) : { last_expr_precedence = 12; break; }
            case (ASR::binopType::BitLShift) : { last_expr_precedence = 7; break; }
            case (ASR::binopType::BitRShift) : { last_expr_precedence = 7; break; }
            case (ASR::binopType::LBitRShift) : { last_expr_precedence = 7; break; }
            case (ASR::binopType::Pow) : {
                src = "pow(" + left + ", " + right + ")";
                if (is_c) {
                    headers.insert("math.h");
                } else {
                    src = "std::" + src;
                }
                return;
            }
            default: throw CodeGenError("BinOp: " + std::to_string(x.m_op) + " operator not implemented yet");
        }
        src = "";
        if (left_precedence == 3) {
            src += "(" + left + ")";
        } else {
            if (left_precedence <= last_expr_precedence) {
                src += left;
            } else {
                src += "(" + left + ")";
            }
        }
        src += ASRUtils::binop_to_str_python(x.m_op);
        if (right_precedence == 3) {
            src += "(" + right + ")";
        } else {
            if (right_precedence < last_expr_precedence) {
                src += right;
            } else {
                src += "(" + right + ")";
            }
        }
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t &x) {
        CHECK_FAST_C_CPP(compiler_options, x)
        self().visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        self().visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        switch (x.m_op) {
            case (ASR::logicalbinopType::And): {
                last_expr_precedence = 14;
                break;
            }
            case (ASR::logicalbinopType::Or): {
                last_expr_precedence = 15;
                break;
            }
            case (ASR::logicalbinopType::NEqv): {
                last_expr_precedence = 10;
                break;
            }
            case (ASR::logicalbinopType::Eqv): {
                last_expr_precedence = 10;
                break;
            }
            default : throw CodeGenError("Unhandled switch case");
        }

        if (left_precedence <= last_expr_precedence) {
            src += left;
        } else {
            src += "(" + left + ")";
        }
        src += ASRUtils::logicalbinop_to_str_python(x.m_op);
        if (right_precedence <= last_expr_precedence) {
            src += right;
        } else {
            src += "(" + right + ")";
        }
    }

    template <typename T>
    void handle_alloc_realloc(const T &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = "";
        for (size_t i=0; i<x.n_args; i++) {
            ASR::symbol_t* tmp_sym = nullptr;
            ASR::ttype_t* type = nullptr;
            ASR::expr_t* tmp_expr = x.m_args[i].m_a;
            if( ASR::is_a<ASR::Var_t>(*tmp_expr) ) {
                const ASR::Var_t* tmp_var = ASR::down_cast<ASR::Var_t>(tmp_expr);
                tmp_sym = tmp_var->m_v;
                type = ASRUtils::expr_type(tmp_expr);
            } else {
                throw CodeGenError("Cannot deallocate variables in expression " +
                                    ASRUtils::type_to_str_python(ASRUtils::expr_type(tmp_expr)),
                                    tmp_expr->base.loc);
            }
            std::string sym = ASRUtils::symbol_name(tmp_sym);
            if (ASRUtils::is_array(type)) {
                std::string size_str = "1";
                out += indent + sym + "->n_dims = " + std::to_string(x.m_args[i].n_dims) + ";\n";
                std::string stride = "1";
                for (int j = (int)x.m_args[i].n_dims - 1; j >= 0; j--) {
                    std::string st, l;
                    if (x.m_args[i].m_dims[j].m_start) {
                        self().visit_expr(*x.m_args[i].m_dims[j].m_start);
                        st = src;
                    } else {
                        st = "0";
                    }
                    if (x.m_args[i].m_dims[j].m_length) {
                        self().visit_expr(*x.m_args[i].m_dims[j].m_length);
                        l = src;
                    } else {
                        l = "1";
                    }
                    size_str += "*" + sym + "->dims[" + std::to_string(j) + "].length";
                    out += indent + sym + "->dims[" + std::to_string(j) + "].lower_bound = ";
                    out += st + ";\n";
                    out += indent + sym + "->dims[" + std::to_string(j) + "].length = ";
                    out += l + ";\n";
                    out += indent + sym + "->dims[" + std::to_string(j) + "].stride = ";
                    out += stride + ";\n";
                    stride = "(" + stride + " * " + l + ")";
                }
                std::string ty = CUtils::get_c_type_from_ttype_t(
                    ASRUtils::type_get_past_array(
                        ASRUtils::type_get_past_allocatable(type)));
                size_str += "*sizeof(" + ty + ")";
                out += indent + sym + "->data = (" + ty + "*) _lfortran_malloc(" + size_str + ")";
                out += ";\n";
                out += indent + sym + "->is_allocated = true;\n";
            } else {
                std::string ty = CUtils::get_c_type_from_ttype_t(type), size_str;
                size_str = "sizeof(" + ty + ")";
                out += indent + sym + " = (" + ty + "*) _lfortran_malloc(" + size_str + ")";
                out += ";\n";
            }
        }
        src = out;
    }

    void visit_Allocate(const ASR::Allocate_t &x) {
        handle_alloc_realloc(x);
    }

    void visit_ReAlloc(const ASR::ReAlloc_t &x) {
        handle_alloc_realloc(x);
    }


    void visit_Assert(const ASR::Assert_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent;
        if (x.m_msg) {
            out += "assert ((";
            self().visit_expr(*x.m_msg);
            out += src + ", ";
            self().visit_expr(*x.m_test);
            out += src + "));\n";
        } else {
            out += "assert (";
            self().visit_expr(*x.m_test);
            out += src + ");\n";
        }
        src = out;
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "// FIXME: deallocate(";
        for (size_t i=0; i<x.n_vars; i++) {
            ASR::symbol_t* tmp_sym = nullptr;
            ASR::expr_t* tmp_expr = x.m_vars[i];
            if( ASR::is_a<ASR::Var_t>(*tmp_expr) ) {
                const ASR::Var_t* tmp_var = ASR::down_cast<ASR::Var_t>(tmp_expr);
                tmp_sym = tmp_var->m_v;
            } else {
                throw CodeGenError("Cannot deallocate variables in expression " +
                                    ASRUtils::type_to_str_python(ASRUtils::expr_type(tmp_expr)),
                                    tmp_expr->base.loc);
            }
            out += std::string(ASRUtils::symbol_name(tmp_sym)) + ", ";
        }
        out += ");\n";
        src = out;
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "// FIXME: implicit deallocate(";
        for (size_t i=0; i<x.n_vars; i++) {
            ASR::symbol_t* tmp_sym = nullptr;
            ASR::expr_t* tmp_expr = x.m_vars[i];
            if( ASR::is_a<ASR::Var_t>(*tmp_expr) ) {
                const ASR::Var_t* tmp_var = ASR::down_cast<ASR::Var_t>(tmp_expr);
                tmp_sym = tmp_var->m_v;
            } else {
                throw CodeGenError("Cannot deallocate variables in expression " +
                                    ASRUtils::type_to_str_python(ASRUtils::expr_type(tmp_expr)),
                                    tmp_expr->base.loc);
            }
            out += std::string(ASRUtils::symbol_name(tmp_sym)) + ", ";
        }
        out += ");\n";
        src = out;
    }

    void visit_Select(const ASR::Select_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        this->visit_expr(*x.m_test);
        std::string var = std::move(src);
        std::string out = indent + "if (";

        for (size_t i = 0; i < x.n_body; i++) {
            if (i > 0)
                out += indent + "else if (";
            bracket_open++;
            ASR::case_stmt_t* stmt = x.m_body[i];
            if (stmt->type == ASR::case_stmtType::CaseStmt) {
                ASR::CaseStmt_t* case_stmt = ASR::down_cast<ASR::CaseStmt_t>(stmt);
                for (size_t j = 0; j < case_stmt->n_test; j++) {
                    if (j > 0)
                        out += " || ";
                    this->visit_expr(*case_stmt->m_test[j]);
                    out += var + " == " + src;
                }
                out += ") {\n";
                bracket_open--;
                indentation_level += 1;
                for (size_t j = 0; j < case_stmt->n_body; j++) {
                    this->visit_stmt(*case_stmt->m_body[j]);
                    out += src;
                }
                out += indent + "}\n";
                indentation_level -= 1;
            } else {
                ASR::CaseStmt_Range_t* case_stmt_range
                    = ASR::down_cast<ASR::CaseStmt_Range_t>(stmt);
                std::string left, right;
                if (case_stmt_range->m_start) {
                    this->visit_expr(*case_stmt_range->m_start);
                    left = std::move(src);
                }
                if (case_stmt_range->m_end) {
                    this->visit_expr(*case_stmt_range->m_end);
                    right = std::move(src);
                }
                if (left.empty() && right.empty()) {
                    diag.codegen_error_label(
                        "Empty range in select statement", { x.base.base.loc }, "");
                    throw Abort();
                }
                if (left.empty()) {
                    out += var + " <= " + right;
                } else if (right.empty()) {
                    out += var + " >= " + left;
                } else {
                    out += left + " <= " + var + " <= " + right;
                }
                out += ") {\n";
                bracket_open--;
                indentation_level += 1;
                for (size_t j = 0; j < case_stmt_range->n_body; j++) {
                    this->visit_stmt(*case_stmt_range->m_body[j]);
                    out += src;
                }
                out += indent + "}\n";
                indentation_level -= 1;
            }
        }
        if (x.n_default) {
            out += indent + "else {\n";
            indentation_level += 1;
            for (size_t i = 0; i < x.n_default; i++) {
                this->visit_stmt(*x.m_default[i]);
                out += src;
            }
            out += indent + "}\n";
            indentation_level -= 1;
        }
        src = check_tmp_buffer() + out;
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        bracket_open++;
        std::string out = indent + "while (";
        self().visit_expr(*x.m_test);
        out += src + ") {\n";
        bracket_open--;
        out = check_tmp_buffer() + out;
        indentation_level += 1;
        for (size_t i=0; i<x.n_body; i++) {
            self().visit_stmt(*x.m_body[i]);
            out += check_tmp_buffer() + src;
        }
        out += indent + "}\n";
        indentation_level -= 1;
        src = out;
    }

    void visit_Exit(const ASR::Exit_t & /* x */) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        src = indent + "break;\n";
    }

    void visit_Cycle(const ASR::Cycle_t & /* x */) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        src = indent + "continue;\n";
    }

    void visit_Return(const ASR::Return_t & /* x */) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        if (current_function && current_function->m_return_var) {
            src = indent + "return "
                + ASRUtils::EXPR2VAR(current_function->m_return_var)->m_name
                + ";\n";
        } else {
            src = indent + "return;\n";
        }
    }

    void visit_GoTo(const ASR::GoTo_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string goto_c_name = "__c__goto__" + std::string(x.m_name);
        src =  indent + "goto " + goto_c_name + ";\n";
        gotoid2name[x.m_target_id] = goto_c_name;
    }

    void visit_GoToTarget(const ASR::GoToTarget_t &x) {
        std::string goto_c_name = "__c__goto__" + std::string(x.m_name);
        src = goto_c_name + ":\n";
    }

    void visit_Stop(const ASR::Stop_t &x) {
        if (x.m_code) {
            self().visit_expr(*x.m_code);
        } else {
            src = "0";
        }
        std::string indent(indentation_level*indentation_spaces, ' ');
        src = indent + "exit(" + src + ");\n";
    }

    void visit_ErrorStop(const ASR::ErrorStop_t & /* x */) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        if (is_c) {
            src = indent + "fprintf(stderr, \"ERROR STOP\");\n";
        } else {
            src = indent + "std::cerr << \"ERROR STOP\" << std::endl;\n";
        }
        src += indent + "exit(1);\n";
    }

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &/*x*/) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + " /* FIXME: implied do loop */ ";
        src = out;
        last_expr_precedence = 2;
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        std::string current_body_copy = current_body;
        current_body = "";
        std::string loop_end_decl = "";
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "for (";
        ASR::Variable_t *loop_var = ASRUtils::EXPR2VAR(x.m_head.m_v);
        std::string lvname=loop_var->m_name;
        ASR::expr_t *a=x.m_head.m_start;
        ASR::expr_t *b=x.m_head.m_end;
        ASR::expr_t *c=x.m_head.m_increment;
        LCOMPILERS_ASSERT(a);
        LCOMPILERS_ASSERT(b);
        int increment;
        bool is_c_constant = false;
        if (!c) {
            increment = 1;
            is_c_constant = true;
        } else {
            ASR::expr_t* c_value = ASRUtils::expr_value(c);
            is_c_constant = ASRUtils::extract_value(c_value, increment);
        }

        if( is_c_constant ) {
            std::string cmp_op;
            if (increment > 0) {
                cmp_op = "<=";
            } else {
                cmp_op = ">=";
            }

            out += lvname + "=";
            self().visit_expr(*a);
            out += src + "; " + lvname + cmp_op;
            self().visit_expr(*b);
            out += src + "; " + lvname;
            if (increment == 1) {
                out += "++";
            } else if (increment == -1) {
                out += "--";
            } else {
                out += "+=" + std::to_string(increment);
            }
        } else {
            this->visit_expr(*c);
            std::string increment_ = std::move(src);
            self().visit_expr(*b);
            std::string do_loop_end = std::move(src);
            std::string do_loop_end_name = current_scope->get_unique_name(
                "loop_end___" + std::to_string(loop_end_count));
            loop_end_count += 1;
            loop_end_decl = indent + CUtils::get_c_type_from_ttype_t(ASRUtils::expr_type(b), is_c) +
                            " " + do_loop_end_name + " = " + do_loop_end + ";\n";
            out += lvname + " = ";
            self().visit_expr(*a);
            out += src + "; ";
            out += "((" + increment_ + " >= 0) && (" +
                    lvname + " <= " + do_loop_end_name + ")) || (("
                    + increment_ + " < 0) && (" + lvname + " >= "
                    + do_loop_end_name + ")); " + lvname;
            out += " += " + increment_;
        }

        out += ") {\n";
        indentation_level += 1;
        for (size_t i=0; i<x.n_body; i++) {
            self().visit_stmt(*x.m_body[i]);
            current_body += src;
        }
        out += current_body;
        out += indent + "}\n";
        indentation_level -= 1;
        src = loop_end_decl + out;
        current_body = current_body_copy;
    }

    void visit_If(const ASR::If_t &x) {
        std::string current_body_copy = current_body;
        current_body = "";
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "if (";
        bracket_open++;
        self().visit_expr(*x.m_test);
        out += src + ") {\n";
        bracket_open--;
        out = check_tmp_buffer() + out;
        indentation_level += 1;
        for (size_t i=0; i<x.n_body; i++) {
            self().visit_stmt(*x.m_body[i]);
            current_body += check_tmp_buffer() + src;
        }
        out += current_body;
        out += indent + "}";
        if (x.n_orelse == 0) {
            out += "\n";
        } else {
            current_body = "";
            out += " else {\n";
            for (size_t i=0; i<x.n_orelse; i++) {
                self().visit_stmt(*x.m_orelse[i]);
                current_body += check_tmp_buffer() + src;
            }
            out += current_body;
            out += indent + "}\n";
        }
        indentation_level -= 1;
        src = out;
        current_body = current_body_copy;
    }

    void visit_IfExp(const ASR::IfExp_t &x) {
        // IfExp is like a ternary operator in c++
        // test ? body : orelse;
        CHECK_FAST_C_CPP(compiler_options, x)
        std::string out = "(";
        self().visit_expr(*x.m_test);
        out += src + ") ? (";
        self().visit_expr(*x.m_body);
        out += src + ") : (";
        self().visit_expr(*x.m_orelse);
        out += src + ")";
        src = out;
        last_expr_precedence = 16;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_name));
        // TODO: use a mapping with a hash(s) instead:
        std::string sym_name = s->m_name;
        ASR::FunctionType_t *s_type = ASRUtils::get_FunctionType(s);
        if (s_type->m_abi == ASR::abiType::BindC && s_type->m_bindc_name) {
            sym_name = s_type->m_bindc_name;
        } else {
            sym_name = s->m_name;
        }

        if (sym_name == "exit") {
            sym_name = "_xx_lcompilers_changed_exit_xx";
        }
        if (sym_name == "main") {
            sym_name = "_xx_lcompilers_changed_main_xx";
        }
        src = indent + sym_name + "(" + construct_call_args(s, x.n_args, x.m_args) + ");\n";
    }

    #define SET_INTRINSIC_NAME(X, func_name)                                    \
        case (static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::X)) : {  \
            out += func_name; break;                                            \
        }

    #define SET_INTRINSIC_SUBROUTINE_NAME(X, func_name)                                    \
        case (static_cast<int64_t>(ASRUtils::IntrinsicImpureSubroutines::X)) : {  \
            out += func_name; break;                                            \
        }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t &x) {
        CHECK_FAST_C_CPP(compiler_options, x);
        std::string out;
        std::string indent(4, ' ');
        switch (x.m_intrinsic_id) {
            SET_INTRINSIC_NAME(Sin, "sin");
            SET_INTRINSIC_NAME(Cos, "cos");
            SET_INTRINSIC_NAME(Tan, "tan");
            SET_INTRINSIC_NAME(Asin, "asin");
            SET_INTRINSIC_NAME(Acos, "acos");
            SET_INTRINSIC_NAME(Atan, "atan");
            SET_INTRINSIC_NAME(Sinh, "sinh");
            SET_INTRINSIC_NAME(Cosh, "cosh");
            SET_INTRINSIC_NAME(Tanh, "tanh");
            SET_INTRINSIC_NAME(Abs, "abs");
            SET_INTRINSIC_NAME(Exp, "exp");
            SET_INTRINSIC_NAME(Exp2, "exp2");
            SET_INTRINSIC_NAME(Expm1, "expm1");
            SET_INTRINSIC_NAME(Trunc, "trunc");
            SET_INTRINSIC_NAME(Fix, "fix");
            SET_INTRINSIC_NAME(FloorDiv, "floordiv");
            SET_INTRINSIC_NAME(Char, "char");
            SET_INTRINSIC_NAME(StringContainsSet, "verify");
            SET_INTRINSIC_NAME(StringFindSet, "scan");
            SET_INTRINSIC_NAME(SubstrIndex, "index");
            SET_INTRINSIC_NAME(StringLenTrim, "len_trim");
            SET_INTRINSIC_NAME(StringTrim, "trim");
            case (static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::FMA)) : {
                this->visit_expr(*x.m_args[0]);
                std::string a = src;
                this->visit_expr(*x.m_args[1]);
                std::string b = src;
                this->visit_expr(*x.m_args[2]);
                std::string c = src;
                src = a +" + "+ b +"*"+ c;
                return;
            }
            default : {
                throw LCompilersException("IntrinsicElementalFunction: `"
                    + ASRUtils::get_intrinsic_name(x.m_intrinsic_id)
                    + "` is not implemented");
            }
        }
        headers.insert("math.h");
        this->visit_expr(*x.m_args[0]);
        out += "(" + src + ")";
        src = out;
    }

    void visit_TypeInquiry(const ASR::TypeInquiry_t &x) {
        this->visit_expr(*x.m_value);
    }

    void visit_RealSqrt(const ASR::RealSqrt_t &x) {
        std::string out = "sqrt";
        headers.insert("math.h");
        this->visit_expr(*x.m_arg);
        out += "(" + src + ")";
        src = out;
    }

};

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_C_CPP_H
