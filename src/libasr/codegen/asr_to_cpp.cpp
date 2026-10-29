#include <memory>

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_cpp.h>
#include <libasr/codegen/asr_to_c_cpp.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/string_utils.h>
#include <libasr/pass/unused_functions.h>
#include <libasr/pass/update_array_dim_intrinsic_calls.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/array_struct_temporary.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/replace_intrinsic_function.h>
#include <libasr/pass/replace_print_arr.h>
#include <libasr/pass/create_subroutine_from_function.h>


namespace LCompilers {

std::string format_type(const std::string &dims, const std::string &type,
        const std::string &name, bool use_ref, bool dummy, bool use_kokko=true,
        std::string kokko_ref="&", bool use_name=false, size_t size=0,
        const std::string &runtime_size="")
{
    std::string fmt;
    if (dims.size() == 0) {
        std::string ref;
        if (use_ref) ref = "&";
        fmt = type + " " + ref + name;
    } else {
        if (dummy) {
            std::string c;
            if (!use_ref) c = "const ";
            if( use_kokko ) {
                fmt = "const Kokkos::View<" + c + type + dims + "> &" + name;
            } else {
                fmt = c + type + dims + " " + name;
            }
        } else {
            if( use_kokko ) {
                fmt = "Kokkos::View<" + type + dims + ">" + kokko_ref + " " + name;
                if( use_name ) {
                   fmt += "(\"" + name + "\"";
                   if( dims == "*" || size > 0 ) {
                    fmt += ", " + (runtime_size.empty() ? std::to_string(size) : runtime_size);
                   }
                   fmt += ")";
                }
            } else {
                fmt = type + dims + " " + name;
            }
        }
    }
    return fmt;
}

std::string trim_dims(std::string &dims) {
    std::string trimmed;
    bool last_is_digit = true;
    size_t i = 0;
    while (i < dims.size() && !isdigit(dims[i])) i++;
    for (; i < dims.size(); i++) {
        if (isdigit(dims[i])) {
            if (!last_is_digit) {
                trimmed += "_";
                last_is_digit = true;
            }
            trimmed.push_back(dims[i]);
        } else {
            last_is_digit = false;
        }
    }
    return trimmed;
}

class ASRToCPPVisitor : public BaseCCPPVisitor<ASRToCPPVisitor>
{
public:

    std::map<std::string, std::map<std::string,
             std::map<size_t, std::string>>> eltypedims2arraytype;
    std::string array_bound_helper;

    ASRToCPPVisitor(diag::Diagnostics &diag, CompilerOptions &co,
                    int64_t default_lower_bound)
        : BaseCCPPVisitor(diag, co.platform, co, true, true, false,
                          default_lower_bound) {}

    std::string convert_dims(size_t n_dims, ASR::dimension_t *m_dims, size_t& size)
    {
        std::string dims;
        size = 1;
        for (size_t i=0; i<n_dims; i++) {
            ASR::expr_t *length = m_dims[i].m_length;
            if (!length) {
                dims += "*";
            } else if (length) {
                ASR::expr_t* length_value = ASRUtils::expr_value(length);
                if( length_value ) {
                    int64_t length_int = -1;
                    ASRUtils::extract_value(length_value, length_int);
                    size *= (length_int);
                    dims += "[" + std::to_string(length_int) + "]";
                } else {
                    size = 0;
                    dims += "[ /* FIXME symbolic dimensions */ ]";
                }
            } else {
                throw CodeGenError("Dimension type not supported");
            }
        }
        return dims;
    }

    bool is_array_type_available(std::string& encoded_type_name, std::string& dims,
                                 size_t n_dims) {
        return eltypedims2arraytype.find(encoded_type_name) != eltypedims2arraytype.end() &&
               eltypedims2arraytype[encoded_type_name].find(dims) !=
               eltypedims2arraytype[encoded_type_name].end() &&
               eltypedims2arraytype[encoded_type_name][dims].find(n_dims) !=
               eltypedims2arraytype[encoded_type_name][dims].end();
    }

    std::string get_array_type(std::string type_name, std::string encoded_type_name,
                               std::string& dims,
                               size_t n_dims, bool make_ptr=true) {
        std::string dims_copy = trim_dims(dims);
        if( is_array_type_available(encoded_type_name, dims_copy, n_dims) ) {
            if( make_ptr ) {
                return eltypedims2arraytype[encoded_type_name][dims_copy][n_dims] + "*";
            } else {
                return eltypedims2arraytype[encoded_type_name][dims_copy][n_dims];
            }
        }

        std::string struct_name;
        std::string new_array_type;
        std::string name = encoded_type_name + "_" + dims_copy + "_" + std::to_string(n_dims);
        struct_name = "struct " + name;
        std::string array_data = format_type("*", type_name, "data", false, false, true, "*");
        new_array_type = struct_name + "\n{\n    " + array_data +
                            ";\n    dimension_descriptor dims[" +
                            std::to_string(n_dims) + "];\n    bool is_allocated;\n\n";
        std::string constructor = "    " + name + "(" + array_data + "_): data{data_} {};\n};\n";
        new_array_type += constructor;
        type_name = name + "*";
        eltypedims2arraytype[encoded_type_name][dims_copy][n_dims] = name;
        array_types_decls += "\n" + new_array_type + "\n";
        return type_name;
    }

    void generate_array_decl(std::string& sub, std::string v_m_name,
                             std::string& type_name, std::string& dims,
                             std::string& encoded_type_name,
                             ASR::dimension_t* m_dims, size_t n_dims, size_t size,
                             bool use_ref, bool dummy,
                             bool declare_value, bool /*is_fixed_size*/,
                             bool is_pointer=false, bool is_allocatable=false) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string type_name_copy = type_name;
        type_name = get_array_type(type_name, encoded_type_name, dims, n_dims);
        std::string type_name_without_ptr = get_array_type(type_name, encoded_type_name, dims,
                                                           n_dims, false);
        if( declare_value ) {
            std::string variable_name = std::string(v_m_name) + "_value";
            if( !is_pointer ) {
                std::string runtime_size;
                if (!is_allocatable && size == 0) {
                    runtime_size = "1";
                    for (size_t i = 0; i < n_dims; i++) {
                        visit_expr(*m_dims[i].m_length);
                        runtime_size += " * (" + src + ")";
                    }
                }
                sub = format_type("*", type_name_copy, std::string(v_m_name) + "_data",
                                  use_ref, dummy, true, "", !is_allocatable, size,
                                  runtime_size) + ";\n" + indent;
            } else {
                sub = indent;
            }
            sub += format_type("", type_name_without_ptr, variable_name, use_ref, dummy, false);
            if( !is_pointer ) {
                sub += "(&" + std::string(v_m_name) + "_data);\n";
            } else {
                sub += ";\n";
            }
            sub += indent + format_type("", type_name, v_m_name, use_ref, dummy, false);
            sub += " = &" + variable_name + ";\n";
            if( !is_pointer ) {
                for (size_t i = 0; i < n_dims; i++) {
                    if( !is_allocatable && m_dims[i].m_start ) {
                        this->visit_expr(*m_dims[i].m_start);
                        sub += indent + std::string(v_m_name) +
                            "->dims[" + std::to_string(i) + "].lower_bound = " + src + ";\n";
                    } else {
                        sub += indent + std::string(v_m_name) +
                            "->dims[" + std::to_string(i) + "].lower_bound = 0" + ";\n";
                    }
                    if( !is_allocatable && m_dims[i].m_length ) {
                        this->visit_expr(*m_dims[i].m_length);
                        sub += indent + std::string(v_m_name) +
                            "->dims[" + std::to_string(i) + "].length = " + src + ";\n";
                    } else {
                        sub += indent + std::string(v_m_name) +
                            "->dims[" + std::to_string(i) + "].length = 0" + ";\n";
                    }
                }
                if (is_allocatable) {
                    for (size_t i = 0; i < n_dims; i++) {
                        sub += indent + v_m_name + "->dims[" +
                            std::to_string(i) + "].stride = " +
                            (i + 1 == n_dims ? "1" : "0") + ";\n";
                    }
                }
                sub += indent + v_m_name + "->is_allocated = " +
                    (is_allocatable ? "false" : "true");
            }
        } else {
            sub = format_type("", type_name, v_m_name, use_ref, dummy, false);
        }
    }

    std::string generate_template_parameter(std::string v_name, bool is_array=false) {
        std::string typename_T = "T" + std::to_string(template_number);
        template_for_Kokkos += "typename " + typename_T + ", ";
        template_number += 1;
        return typename_T + (is_array ? "* " : " ") + v_name;
    }

    std::string convert_procedure_actual(ASR::expr_t* actual, ASR::expr_t* dummy,
                                        const std::string& value) {
        if (!ASR::is_a<ASR::Var_t>(*actual)
                || !ASR::is_a<ASR::Function_t>(
                    *ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(actual)->m_v))
                || !ASR::is_a<ASR::Var_t>(*dummy)) {
            return value;
        }
        ASR::symbol_t* symbol = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(dummy)->m_v);
        if (ASR::is_a<ASR::Variable_t>(*symbol)) {
            ASR::Function_t* iface = get_dummy_procedure_interface(
                ASR::down_cast<ASR::Variable_t>(symbol));
            if (iface && ASRUtils::get_FunctionType(*iface)->m_abi == ASR::abiType::Source) {
                std::string args = current_scope->get_unique_name("args");
                // The actual's emitted name may come from an enclosing scope.
                if (args == value) {
                    args = current_scope->get_unique_name(args + "_");
                }
                // Delay specialization of array templates until the callback is invoked.
                return "[](auto&&... " + args + ") -> decltype(auto) { return "
                    + value + "(" + args + "...); }";
            }
        }
        return value;
    }

    std::string capture_array_arguments(const ASR::Function_t& x) {
        std::string result;
        std::string indent(indentation_level*indentation_spaces, ' ');
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Variable_t* formal = ASRUtils::EXPR2VAR(x.m_args[i]);
            if (!ASRUtils::is_array(formal->m_type) ||
                    ASRUtils::is_allocatable_or_pointer(formal->m_type)) {
                continue;
            }
            ASR::dimension_t* dims = nullptr;
            size_t rank = ASRUtils::extract_dimensions_from_ttype(formal->m_type, dims);
            std::string bounds;
            for (size_t j = 0; j < rank; j++) {
                if (dims[j].m_start && !dims[j].m_length) {
                    visit_expr(*dims[j].m_start);
                    bounds += indent + std::string(formal->m_name) + "->dims["
                        + std::to_string(j) + "].lower_bound = " + src + ";\n";
                }
            }
            if (!bounds.empty()) {
                std::string capture = current_scope->get_unique_name(
                    std::string(formal->m_name) + "_descriptor");
                result += indent + "auto " + capture + " = *" + formal->m_name + ";\n"
                    + indent + formal->m_name + " = &" + capture + ";\n" + bounds;
            }
        }
        return result;
    }

    std::string convert_variable_decl(const ASR::Variable_t &v, DeclarationOptions* decl_options=nullptr)
    {
        if (v.m_storage == ASR::storage_typeType::Threadprivate) {
            throw CodeGenError("threadprivate variables are not supported by the cpp backend",
                v.base.base.loc);
        }
        bool use_static;
        bool use_templates_for_arrays;

        if( decl_options ) {
            CPPDeclarationOptions* cpp_decl_options = reinterpret_cast<CPPDeclarationOptions*>(decl_options);
            use_static = cpp_decl_options->use_static;
            use_templates_for_arrays = cpp_decl_options->use_templates_for_arrays;
        } else {
            use_static = true;
            use_templates_for_arrays = false;
        }

        std::string sub;
        bool use_ref = (v.m_intent == ASRUtils::intent_out ||
                        v.m_intent == ASRUtils::intent_inout ||
                        v.m_intent == ASRUtils::intent_unspecified
                        );
        bool is_allocatable = ASRUtils::is_allocatable(v.m_type);
        ASR::ttype_t* v_m_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(v.m_type));
        if (ASR::is_a<ASR::FunctionType_t>(*v_m_type)) {
            ASR::Function_t* iface = nullptr;
            if (v.m_type_declaration) {
                ASR::symbol_t* td = ASRUtils::symbol_get_past_external(v.m_type_declaration);
                if (td && ASR::is_a<ASR::Function_t>(*td)) {
                    iface = ASR::down_cast<ASR::Function_t>(td);
                }
            }
            if (!iface) {
                diag.codegen_error_label("Procedure variable '"
                    + std::string(v.m_name)
                    + "' has no interface. Add explicit interface.",
                    {v.base.base.loc}, "");
                throw Abort();
            }
            if (use_templates_for_arrays && ASRUtils::is_arg_dummy(v.m_intent)
                    && ASRUtils::get_FunctionType(*iface)->m_abi == ASR::abiType::Source) {
                return generate_template_parameter(v.m_name);
            }
            std::string ret = "void ";
            if (iface->m_return_var) {
                ASR::Variable_t* rv = ASRUtils::EXPR2VAR(iface->m_return_var);
                ret = get_return_var_type(rv);
            }
            std::string params;
            for (size_t i=0; i<iface->n_args; i++) {
                ASR::Variable_t* arg = ASRUtils::EXPR2VAR(iface->m_args[i]);
                CPPDeclarationOptions inner_opts;
                inner_opts.use_static = false;
                inner_opts.use_templates_for_arrays = true;
                params += convert_variable_decl(*arg, &inner_opts);
                if (i < iface->n_args-1) params += ", ";
            }
            sub = ret + "(*" + std::string(v.m_name) + ")(" + params + ")";
            return sub;
        }
        bool is_array = ASRUtils::is_array(v.m_type);
        bool dummy = ASRUtils::is_arg_dummy(v.m_intent);
        if (is_allocatable && (!is_array ||
                (v.m_intent != ASR::intentType::Local &&
                 !is_array_result_argument(v)))) {
            throw CodeGenError("only local or caller-owned result allocatable array storage is supported "
                "by the C++ backend", v.base.base.loc);
        }

        #define extract_dimensions(t_) ASR::dimension_t* m_dims = nullptr; \
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(t_, m_dims); \

        #define handle_array(t_, type_name_, is_pointer_) size_t size; \
            extract_dimensions(t_) \
            std::string dims = convert_dims(n_dims, m_dims, size); \
            if( is_array ) { \
                if( use_templates_for_arrays ) { \
                    sub += generate_template_parameter(std::string(v.m_name), true); \
                } else { \
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims, \
                                        encoded_type_name, m_dims, n_dims, size, \
                                        use_ref, dummy, \
                                        v.m_intent != ASRUtils::intent_in && \
                                        v.m_intent != ASRUtils::intent_inout && \
                                        v.m_intent != ASRUtils::intent_out, true, is_pointer_, is_allocatable); \
                } \
            } else { \
                sub = format_type(dims, type_name_, v.m_name, use_ref, dummy); \
            } \

        if (ASRUtils::is_pointer(v_m_type)) {
            ASR::ttype_t *t2 = ASR::down_cast<ASR::Pointer_t>(v_m_type)->m_type;
            if (ASRUtils::is_integer(*t2)) {
                ASR::Integer_t *t = ASR::down_cast<ASR::Integer_t>(
                    ASRUtils::type_get_past_array(t2));
                std::string type_name = "int" + std::to_string(t->m_kind * 8) + "_t";
                std::string encoded_type_name = "i" + std::to_string(t->m_kind * 8);
                handle_array(t2, type_name, true)
            } else {
                diag.codegen_error_label("Type '"
                    + ASRUtils::type_to_str_python_symbol(v.m_type, v.m_type_declaration)
                    + "' not supported", {v.base.base.loc}, "");
                throw Abort();
            }
        } else {
            std::string dims;
            use_ref = use_ref && !is_array;
            if (ASRUtils::is_integer(*v_m_type)) {
                ASR::Integer_t *t = ASR::down_cast<ASR::Integer_t>(v_m_type);
                std::string type_name = "int" + std::to_string(t->m_kind * 8) + "_t";
                std::string encoded_type_name = "i" + std::to_string(t->m_kind * 8);
                handle_array(v.m_type, type_name, false)
            } else if (ASRUtils::is_real(*v.m_type)) {
                ASR::Real_t *t = ASR::down_cast<ASR::Real_t>(v_m_type);
                std::string type_name = "float";
                if (t->m_kind == 8) type_name = "double";
                std::string encoded_type_name = "f" + std::to_string(t->m_kind * 8);
                handle_array(v.m_type, type_name, false)
            } else if (ASRUtils::is_complex(*v.m_type)) {
                ASR::Complex_t *t = ASR::down_cast<ASR::Complex_t>(v_m_type);
                std::string type_name = "std::complex<float>";
                if (t->m_kind == 8) type_name = "std::complex<double>";
                std::string encoded_type_name = "c" + std::to_string(t->m_kind * 8);
                handle_array(v.m_type, type_name, false)
            } else if (ASRUtils::is_logical(*v.m_type)) {
                std::string type_name = "bool";
                std::string encoded_type_name = "b";
                handle_array(v.m_type, type_name, false)
            } else if (ASRUtils::is_character(*v.m_type)) {
                // std::string is a byte string, so it can only carry kind 1.
                int64_t char_kind = ASRUtils::extract_kind_from_ttype_t(v.m_type);
                if (char_kind != 1) {
                    throw CodeGenError("character(kind=" + std::to_string(char_kind) +
                        ") is not supported by the C++ backend, only kind 1 is",
                        v.base.base.loc);
                }
                std::string type_name = "std::string";
                std::string encoded_type_name = "s";
                handle_array(v.m_type, type_name, false)
            } else if (ASR::is_a<ASR::StructType_t>(*v.m_type)) {
                std::string der_type_name = ASRUtils::symbol_name(v.m_type_declaration);
                std::string encoded_type_name = "x" + der_type_name;
                std::string type_name = std::string("struct ") + der_type_name;
                handle_array(v.m_type, "struct", false)
            } else if (ASR::is_a<ASR::List_t>(*v.m_type)) {
                ASR::List_t* t = ASR::down_cast<ASR::List_t>(v_m_type);
                std::string list_type_c = c_ds_api->get_list_type(t);
                sub = format_type_c("", list_type_c, v.m_name,
                                    false, false);
            } else {
                diag.codegen_error_label("Type '"
                    + ASRUtils::type_to_str_python_symbol(v.m_type, v.m_type_declaration)
                    + "' not supported", {v.base.base.loc}, "");
                throw Abort();
            }
            if (dims.size() == 0 && v.m_storage == ASR::storage_typeType::Save && use_static) {
                sub = "static " + sub;
            }
            if (dims.size() == 0 && v.m_symbolic_value) {
                this->visit_expr(*v.m_symbolic_value);
                std::string init = src;
                sub += "=" + init;
            }
        }
        return sub;
    }

    bool is_array_result_argument(const ASR::Variable_t &variable) {
        if (variable.m_storage != ASR::storage_typeType::ResultArgument ||
                variable.m_intent != ASR::intentType::Out ||
                !is_source_allocatable_descriptor_array(variable)) return false;
        ASR::asr_t *owner = variable.m_parent_symtab->asr_owner;
        if (!ASR::is_a<ASR::symbol_t>(*owner) ||
                !ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(owner))) {
            return false;
        }
        ASR::Function_t *function = ASR::down_cast<ASR::Function_t>(
            ASR::down_cast<ASR::symbol_t>(owner));
        ASR::FunctionType_t *signature = ASRUtils::get_FunctionType(function);
        return !function->m_return_var &&
            ASRUtils::is_dummy_argument(*function, &variable.base) &&
            signature->m_abi == ASR::abiType::Source &&
            signature->m_exec_space == ASR::exec_spaceType::Host;
    }

    bool is_array_owner_storage(ASR::expr_t* expr) {
        if (!ASR::is_a<ASR::Var_t>(*expr)) return false;
        ASR::Variable_t* variable = ASRUtils::EXPR2VAR(expr);
        return (variable->m_intent == ASR::intentType::Local &&
                ASRUtils::is_allocatable(variable->m_type) &&
                ASRUtils::is_array(variable->m_type)) ||
            is_array_result_argument(*variable);
    }

    std::string allocate_array_owner(const ASR::alloc_arg_t& arg) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out;
        if (!is_array_owner_storage(arg.m_a) || arg.n_codims ||
                arg.m_len_expr || arg.m_type || arg.m_sym_subclass ||
                arg.n_dims != ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(arg.m_a))) {
            throw CodeGenError("allocation requires local or caller-owned result typed array storage "
                "in the C++ backend", arg.loc);
        }
        visit_expr(*arg.m_a);
        std::string array = src;
        std::string size = "1";
        for (size_t j = 0; j < arg.n_dims; j++) {
            std::string dim = array + "->dims[" + std::to_string(j) + "]";
            std::string lower = "1";
            if (arg.m_dims[j].m_start) {
                visit_expr(*arg.m_dims[j].m_start);
                lower = src;
            }
            out += indent + dim + ".lower_bound = " + lower + ";\n";
            if (!arg.m_dims[j].m_length) {
                throw CodeGenError("allocation requires an explicit extent "
                    "in the C++ backend", arg.loc);
            }
            visit_expr(*arg.m_dims[j].m_length);
            out += indent + dim + ".length = " + src + ";\n";
            size += " * " + dim + ".length";
        }
        std::string stride = "1";
        for (size_t j = arg.n_dims; j-- > 0;) {
            std::string dim = array + "->dims[" + std::to_string(j) + "]";
            out += indent + dim + ".stride = " + stride + ";\n";
            stride = "(" + stride + " * " + dim + ".length)";
        }
        out += indent + "*" + array + "->data = std::decay_t<decltype(*" +
            array + "->data)>(\"" + array + "\", " + size + ");\n";
        out += indent + array + "->is_allocated = true;\n";
        return out;
    }

    void visit_Allocate(const ASR::Allocate_t &x) {
        if (x.m_stat || x.m_errmsg || x.m_source) {
            throw CodeGenError("allocation options are not supported by the C++ backend",
                x.base.base.loc);
        }
        std::string out;
        for (size_t i = 0; i < x.n_args; i++) {
            out += allocate_array_owner(x.m_args[i]);
        }
        src = out;
    }

    void visit_ReAlloc(const ASR::ReAlloc_t &x) {
        if (x.n_args != 1 || !is_array_owner_storage(x.m_args[0].m_a)) {
            throw CodeGenError("reallocation requires local or caller-owned result typed array storage "
                "in the C++ backend", x.base.base.loc);
        }
        visit_expr(*x.m_args[0].m_a);
        std::string array = src;
        visit_expr(*x.m_shape_differs);
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "if (!" + array + "->is_allocated || (" + src + ")) {\n";
        indentation_level++;
        out += allocate_array_owner(x.m_args[0]);
        indentation_level--;
        src = out + indent + "}\n";
    }

    std::string release_array_owner(ASR::expr_t* expr) {
        visit_expr(*expr);
        std::string array = src;
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "*" + array +
            "->data = std::decay_t<decltype(*" + array + "->data)>();\n";
        out += indent + array + "->is_allocated = false;\n";
        for (size_t i = 0; i < ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(expr)); i++) {
            std::string dim = array + "->dims[" + std::to_string(i) + "]";
            out += indent + dim + ".lower_bound = 0;\n";
            out += indent + dim + ".length = 0;\n";
            out += indent + dim + ".stride = " +
                (i + 1 == ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(expr)) ? "1" : "0") + ";\n";
        }
        return out;
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        std::string out;
        for (size_t i = 0; i < x.n_vars; i++) {
            if (is_array_owner_storage(x.m_vars[i]) && !x.m_stat && !x.m_errmsg) {
                out += release_array_owner(x.m_vars[i]);
            } else {
                ASR::ExplicitDeallocate_t single = x;
                single.m_vars = &x.m_vars[i];
                single.n_vars = 1;
                BaseCCPPVisitor<ASRToCPPVisitor>::visit_ExplicitDeallocate(single);
                out += src;
            }
        }
        src = out;
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &x) {
        std::string out;
        for (size_t i = 0; i < x.n_vars; i++) {
            if (is_array_owner_storage(x.m_vars[i])) {
                out += release_array_owner(x.m_vars[i]);
            } else {
                ASR::ImplicitDeallocate_t single = x;
                single.m_vars = &x.m_vars[i];
                single.n_vars = 1;
                BaseCCPPVisitor<ASRToCPPVisitor>::visit_ImplicitDeallocate(single);
                out += src;
            }
        }
        src = out;
    }

    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t &x) {
        if (x.m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) &&
                x.n_args == 1 && is_array_owner_storage(x.m_args[0])) {
            visit_expr(*x.m_args[0]);
            src = "(" + src + "->is_allocated)";
            last_expr_precedence = 2;
            return;
        }
        BaseCCPPVisitor<ASRToCPPVisitor>::visit_IntrinsicImpureFunction(x);
    }


    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        global_scope = x.m_symtab;
        // All loose statements must be converted to a function, so the items
        // must be empty:
        LCOMPILERS_ASSERT(x.n_items == 0);
        // The storage of a common block is shared with the other objects that
        // name it through a common symbol, which C++ cannot define.
        std::vector<const ASR::CommonBlock_t*> common_blocks;
        collect_common_blocks(x.m_symtab, common_blocks);
        if (!common_blocks.empty()) {
            throw CodeGenError("COMMON blocks are not supported by the C++ backend",
                common_blocks.front()->base.base.loc);
        }
        indentation_level = 0;
        indentation_spaces = 4;

        SymbolTable* current_scope_copy = current_scope;
        current_scope = global_scope;
        c_ds_api->set_indentation(indentation_level, indentation_spaces);
        c_ds_api->set_global_scope(global_scope);
        c_utils_functions->set_indentation(indentation_level, indentation_spaces);
        c_utils_functions->set_global_scope(global_scope);
        c_ds_api->set_c_utils_functions(c_utils_functions.get());

        std::string head =
R"(#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <cmath>
#include <complex>
#include <type_traits>
#include <Kokkos_Core.hpp>
#include <lfortran_intrinsics.h>

template <typename T>
Kokkos::View<T*> from_std_vector(const std::vector<T> &v)
{
    Kokkos::View<T*> r("r", v.size());
    for (size_t i=0; i < v.size(); i++) {
        r(i) = v[i];
    }
    return r;
}

)";


        // Pre-declare all functions first, then generate code
        // Otherwise some function might not be found.
        std::string unit_src = "// Forward declarations\n";
        unit_src += declare_all_functions(*x.m_symtab);
        // Now pre-declare all functions from modules and programs
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Module_t>(*item.second)) {
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(item.second);
                unit_src += declare_all_functions(*m->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*item.second)) {
                ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(item.second);
                unit_src += "namespace {\n"
                            + declare_all_functions(*p->m_symtab)
                            + "}\n";
            }
        }
        unit_src += "\n";
        unit_src += "// Implementations\n";

        {
            // Process intrinsic modules in the right order
            std::vector<std::string> build_order
                = ASRUtils::determine_module_dependencies(x);
            for (auto &item : build_order) {
                LCOMPILERS_ASSERT(x.m_symtab->get_scope().find(item)
                    != x.m_symtab->get_scope().end());
                if (startswith(item, "lfortran_intrinsic")) {
                    ASR::symbol_t *mod = x.m_symtab->get_symbol(item);
                    visit_symbol(*mod);
                    unit_src += src;
                }
            }
        }

        // Process procedures first:
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
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
                visit_symbol(*mod);
                unit_src += src;
            }
        }

        // Then the main program:
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
                unit_src += src;
            }
        }

        src = get_final_combined_src(head, unit_src);
        current_scope = current_scope_copy;
    }

    void visit_Program(const ASR::Program_t &x) {
        // Generate code for nested subroutines and functions first:
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
        std::string decl;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                decl += indent1;
                decl += convert_variable_decl(*v) + ";\n";
            }
        }

        std::string body;
        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            body += src;
        }

        src = "namespace {\n"
                + contains
                + "\nvoid main2() {\n"
                + decl + body
                + "}\n\n"
                + "}\n"
                + "int main(int argc, char* argv[])\n{\n"
                + indent1 + "Kokkos::initialize(argc, argv);\n"
                + indent1 + "main2();\n"
                + indent1 + "Kokkos::finalize();\n"
                + indent1 + "return 0;\n}\n";
        indentation_level -= 2;
    }

    void visit_ComplexConstructor(const ASR::ComplexConstructor_t &x) {
        this->visit_expr(*x.m_re);
        std::string re = src;
        this->visit_expr(*x.m_im);
        std::string im = src;
        src = "std::complex<float>(" + re + ", " + im + ")";
        if (ASRUtils::extract_kind_from_ttype_t(x.m_type) == 8) {
            src = "std::complex<double>(" + re + ", " + im + ")";
        }
        last_expr_precedence = 2;
    }

    void visit_ComplexConstant(const ASR::ComplexConstant_t &x) {
        std::string re = std::to_string(x.m_re);
        std::string im = std::to_string(x.m_im);
        src = "std::complex<float>(" + re + ", " + im + ")";
        if (ASRUtils::extract_kind_from_ttype_t(x.m_type) == 8) {
            src = "std::complex<double>(" + re + ", " + im + ")";
        }
        last_expr_precedence = 2;
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        if (x.m_value == true) {
            src = "true";
        } else {
            src = "false";
        }
        last_expr_precedence = 2;
    }

    void visit_SetConstant(const ASR::SetConstant_t &x) {
        std::string out = "{";
        for (size_t i=0; i<x.n_elements; i++) {
            visit_expr(*x.m_elements[i]);
            out += src;
            if (i != x.n_elements - 1)
                out += ", ";
        }
        out += "}";
        src = out;
        last_expr_precedence = 2;
    }

    void visit_DictConstant(const ASR::DictConstant_t &x) {
        LCOMPILERS_ASSERT(x.n_keys == x.n_values);
        std::string out = "{";
        for(size_t i=0; i<x.n_keys; i++) {
            out += "{";
            visit_expr(*x.m_keys[i]);
            out += src + ", ";
            visit_expr(*x.m_values[i]);
            out += src + "}";
            if (i!=x.n_keys-1) out += ", ";
        }
        out += "}";
        src = out;
        last_expr_precedence = 2;
    }

    void visit_ArrayConstant(const ASR::ArrayConstant_t &x) {
        throw CodeGenError("array constant must be materialized before C++ code generation",
            x.base.base.loc);
    }

    void visit_ArraySize(const ASR::ArraySize_t& x) {
        visit_expr(*x.m_v);
        std::string var_name = src;
        std::string result;
        if (x.m_dim == nullptr) {
            result = "1";
            for (size_t i = 0; i < ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(x.m_v)); i++) {
                result += " * " + var_name + "->dims[" + std::to_string(i) + "].length";
            }
        } else {
            visit_expr(*x.m_dim);
            result = var_name + "->dims[(" + src + ") - 1].length";
        }
        src = "((" + CUtils::get_c_type_from_ttype_t(x.m_type) + ")(" + result + "))";
    }

    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (x.m_value) {
            visit_expr(*x.m_value);
            return;
        }
        ASR::ttype_t* array_type = ASRUtils::expr_type(x.m_v);
        if (!x.m_dim) {
            throw CodeGenError("array bounds without dim must be lowered "
                "before C++ code generation", x.base.base.loc);
        }
        ASR::expr_t* array = ASRUtils::get_expr_size_expr(x.m_v);
        if (array && ASR::is_a<ASR::Var_t>(*array)) {
            ASR::Variable_t* variable = ASRUtils::EXPR2VAR(array);
            if (ASRUtils::is_arg_dummy(variable->m_intent) &&
                    !is_array_result_argument(*variable)) {
                throw CodeGenError("bounds of this dummy array with this dim "
                    "expression are not supported by the C++ backend", x.base.base.loc);
            }
        }
        if (array_bound_helper.empty()) {
            array_bound_helper = global_scope->get_unique_name("array_bound");
            array_types_decls += "\ntemplate <typename I>\nI " + array_bound_helper
                + "(const dimension_descriptor &dim, bool upper)\n{\n"
                  "    if (dim.length == 0) return upper ? I(0) : I(1);\n"
                  "    return upper ? I(dim.lower_bound) + I(dim.length) - I(1)\n"
                  "                 : I(dim.lower_bound);\n}\n";
        }
        visit_expr(*x.m_v);
        std::string var_name = src;
        std::string result_type = CUtils::get_c_type_from_ttype_t(x.m_type);
        visit_expr(*x.m_dim);
        std::string idx = src;
        src = "::" + array_bound_helper + "<" + result_type + ">((" + var_name
            + ")->dims[(" + idx + ")-1], "
            + (x.m_bound == ASR::arrayboundType::UBound ? "true" : "false") + ")";
        last_expr_precedence = 2;
    }

    void visit_StringConcat(const ASR::StringConcat_t &x) {
        this->visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        this->visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        last_expr_precedence = 6;
        if (left_precedence <= last_expr_precedence) {
            src += "std::string(" + left + ")";
        } else {
            src += left;
        }
        src += " + "; // handle only concatenation for now
        if (right_precedence <= last_expr_precedence) {
            src += "std::string(" + right + ")";
        } else {
            src += right;
        }
    }

    void visit_StringItem(const ASR::StringItem_t& x) {
        this->visit_expr(*x.m_idx);
        std::string idx = std::move(src);
        this->visit_expr(*x.m_arg);
        std::string str = std::move(src);
        src = str + "[" + idx + " - 1]";
    }

    void visit_StringLen(const ASR::StringLen_t &x) {
        this->visit_expr(*x.m_arg);
        src = src + ".length()";
    }

    void visit_Print(const ASR::Print_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "std::cout ", sep;
        //HACKISH way to handle print refactoring (always using stringformat).
        // TODO : Implement stringformat visitor.
        ASR::StringFormat_t* str_fmt;
        size_t n_values = 0;
        sep = "\" \"";
        if(ASR::is_a<ASR::StringFormat_t>(*x.m_text)) {
            str_fmt = ASR::down_cast<ASR::StringFormat_t>(x.m_text);
            if (str_fmt->n_dtio_bindings > 0) {
                throw CodeGenError("user-defined derived-type output is not "
                    "supported by the C++ backend yet", x.base.base.loc);
            }
            n_values = str_fmt->n_args;
        } else if(ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(x.m_text))){
            this->visit_expr(*x.m_text);
            src = "std::cout<< (" + src + ")<<std::endl;\n";
            return;
        } else {
            throw CodeGenError("print statment supported for stringformat and single character argument",
                x.base.base.loc);
        }
        for (size_t i=0; i<n_values; i++) {
            ASR::expr_t* arg = str_fmt->m_args[i];
            this->visit_expr(*arg);
            out += "<< (" + src + ") ";
            if (i+1 != n_values) {
                out += "<< " + sep + " ";
            }
        }
        out += "<< std::endl;\n";
        src = check_tmp_buffer() + out;
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "std::cout ";
        //HACKISH way to handle print refactoring (always using stringformat).
        // TODO : Implement stringformat visitor.
        ASR::StringFormat_t* str_fmt = nullptr;
        size_t n_values = x.n_values;
        if(x.m_values[0] && ASR::is_a<ASR::StringFormat_t>(*x.m_values[0])) {
            str_fmt = ASR::down_cast<ASR::StringFormat_t>(x.m_values[0]);
            if (str_fmt->n_dtio_bindings > 0) {
                throw CodeGenError("user-defined derived-type output is not "
                    "supported by the C++ backend yet", x.base.base.loc);
            }
            n_values = str_fmt->n_args;
        }
        for (size_t i=0; i<n_values; i++) {
            ASR::expr_t* arg = str_fmt ? str_fmt->m_args[i] : x.m_values[i];
            this->visit_expr(*arg);
            out += "<< (" + src + ") ";
        }
        out += "<< std::endl;\n";
        src = check_tmp_buffer() + out;
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "// FIXME: READ: std::cout ";
        for (size_t i=0; i<x.n_values; i++) {
            this->visit_expr(*x.m_values[i]);
            out += "<< " + src + " ";
        }
        out += "<< std::endl;\n";
        src = out;
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent + "Kokkos::parallel_for(";
        out += "Kokkos::RangePolicy<Kokkos::DefaultExecutionSpace>(";
        LCOMPILERS_ASSERT(x.n_head == 1);
        visit_expr(*x.m_head[0].m_start);
        out += src + ", ";
        visit_expr(*x.m_head[0].m_end);
        out += src + "+1)";
        ASR::Variable_t *loop_var = ASRUtils::EXPR2VAR(x.m_head[0].m_v);
        sym_info[get_hash((ASR::asr_t*) loop_var)].needs_declaration = false;
        out += ", KOKKOS_LAMBDA(const long " + std::string(loop_var->m_name)
                + ") {\n";
        indentation_level += 1;
        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            out += src;
        }
        out += indent + "});\n";
        indentation_level -= 1;
        src = out;
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        this->visit_expr(*x.m_v);
        std::string array = src;
        std::string out = array;
        ASR::dimension_t* m_dims;
        ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(x.m_v), m_dims);
        std::string flat;
        for (size_t i=0; i<x.n_args; i++) {
            if (x.m_args[i].m_right) {
                this->visit_expr(*x.m_args[i].m_right);
            } else {
                src = "/* FIXME right index */";
            }
            std::string term = "((" + src + ") - " + array + "->dims["
                + std::to_string(i) + "].lower_bound)";
            if (i == 0) {
                flat = term;
            } else {
                flat = "(((" + flat + ")*(" + array + "->dims["
                    + std::to_string(i) + "].length)) + " + term + ")";
            }
        }
        out += "->data->operator[](" + flat + ")";
        last_expr_precedence = 2;
        src = out;
    }

};

class CPPConstructorReadiness : public ASR::BaseWalkVisitor<CPPConstructorReadiness>
{
public:
    void visit_ttype(const ASR::ttype_t&) {}
    void visit_DebugCheckArrayBounds(const ASR::DebugCheckArrayBounds_t&) {}
    void visit_ArrayConstructor(const ASR::ArrayConstructor_t& x) {
        throw CodeGenError("array constructor must be materialized before C++ code generation",
            x.base.base.loc);
    }
    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t& x) {
        throw CodeGenError("implied-do must be lowered before C++ code generation",
            x.base.base.loc);
    }
    void visit_ArrayConstant(const ASR::ArrayConstant_t& x) {
        throw CodeGenError("array constant must be materialized before C++ code generation",
            x.base.base.loc);
    }
    void visit_ArraySection(const ASR::ArraySection_t& x) {
        throw CodeGenError("array section must be lowered before C++ code generation",
            x.base.base.loc);
    }
};

Result<std::string> asr_to_cpp(Allocator &al, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co,
    int64_t default_lower_bound)
{
    co.po.always_run = true;
    pass_unused_functions(al, asr, co.po);
    PassOptions array_options = co.po;
    array_options.materialize_array_constants = true;
    pass_replace_implied_do_loops(al, asr, array_options);
    if (!asr_verify(asr, true, diagnostics)) return Error();
    pass_array_struct_temporary(al, asr, co.po, ArraySectionLowering::ValueSnapshot);
    if (!asr_verify(asr, true, diagnostics)) return Error();
    pass_replace_array_op(al, asr, co.po);
    if (!asr_verify(asr, true, diagnostics)) return Error();
    pass_replace_intrinsic_function(al, asr, co.po);
    if (!asr_verify(asr, true, diagnostics)) return Error();
    pass_create_subroutine_from_function(al, asr, co.po,
        FunctionResultLowering::ExplicitArraySnapshot);
    if (!asr_verify(asr, true, diagnostics)) return Error();
    pass_replace_array_op(al, asr, co.po);
    if (!asr_verify(asr, true, diagnostics)) return Error();
    pass_replace_print_arr(al, asr, co.po);
    pass_update_array_dim_intrinsic_calls(al, asr, co.po);
    if (!asr_verify(asr, true, diagnostics)) {
        return Error();
    }
    ASRToCPPVisitor v(diagnostics, co, default_lower_bound);
    try {
        CPPConstructorReadiness readiness;
        readiness.visit_TranslationUnit(asr);
        v.visit_asr((ASR::asr_t &)asr);
    } catch (const CodeGenError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const Abort &) {
        return Error();
    }
    return v.src;
}

} // namespace LCompilers
