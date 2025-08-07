#include <fstream>
#include <memory>

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_c.h>
#include <libasr/codegen/asr_to_c_cpp.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/pass/unused_functions.h>
#include <libasr/pass/replace_class_constructor.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/create_subroutine_from_function.h>

#include <map>
#include <utility>

#define CHECK_FAST_C(compiler_options, x)                         \
        if (compiler_options.po.fast && x.m_value != nullptr) {    \
            visit_expr(*x.m_value);                             \
            return;                                             \
        }                                                       \

namespace LCompilers {

class ASRToCVisitor : public BaseCCPPVisitor<ASRToCVisitor>
{
public:

    int counter;

    bool target_offload_enabled;
    std::vector<std::string> kernel_func_names;
    int kernel_counter=0; // To generate unique kernel names
    std::string current_kernel_name; // Track current kernel for wrapper
    std::vector<std::pair<std::string, ASR::OMPMap_t*>> map_vars; // Track map vars for target offload
    std::string indent() {
        return std::string(indentation_level * indentation_spaces, ' ');
    }
    std ::string kernel_func_code;

    ASRToCVisitor(diag::Diagnostics &diag, CompilerOptions &co,
                  int64_t default_lower_bound)
         : BaseCCPPVisitor(diag, co.platform, co, false, false, true, default_lower_bound),
           counter{0} {
            target_offload_enabled = co.target_offload_enabled;
           }

    std::string convert_dims_c(size_t n_dims, ASR::dimension_t *m_dims,
                               ASR::ttype_t* element_type, bool& is_fixed_size,
                               bool convert_to_1d=false)
    {
        std::string dims = "";
        size_t size = 1;
        std::string array_size = "";
        for (size_t i=0; i<n_dims; i++) {
            ASR::expr_t *length = m_dims[i].m_length;
            if (!length) {
                is_fixed_size = false;
                return dims;
            } else {
                visit_expr(*length);
                array_size += "*" + src;
                ASR::expr_t* length_value = ASRUtils::expr_value(length);
                if( length_value ) {
                    int64_t length_int = -1;
                    ASRUtils::extract_value(length_value, length_int);
                    size *= length_int;
                    dims += "[" + std::to_string(length_int) + "]";
                } else {
                    size = 0;
                    dims += "[ /* FIXME symbolic dimensions */ ]";
                }
            }
        }
        if( size == 0 ) {
            std::string element_type_str = CUtils::get_c_type_from_ttype_t(element_type);
            dims = "(" + element_type_str + "*)" + " malloc(sizeof(" + element_type_str + ")" + array_size + ")";
            is_fixed_size = false;
            return dims;
        }
        if( convert_to_1d && size != 0 ) {
            dims = "[" + std::to_string(size) + "]";
        }
        return dims;
    }

    void generate_array_decl(std::string& sub, std::string v_m_name,
                             std::string& type_name, std::string& dims,
                             std::string& encoded_type_name,
                             ASR::dimension_t* m_dims, int n_dims,
                             bool use_ref, bool dummy,
                             bool declare_value, bool is_fixed_size,
                             bool is_pointer,
                             ASR::abiType m_abi,
                             bool is_simd_array) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string type_name_copy = type_name;
        std::string original_type_name = type_name;
        type_name = c_ds_api->get_array_type(type_name, encoded_type_name, array_types_decls);
        std::string type_name_without_ptr = c_ds_api->get_array_type(type_name, encoded_type_name, array_types_decls, false);
        if (is_simd_array) {
            int64_t size = ASRUtils::get_fixed_size_of_array(m_dims, n_dims);
            sub = original_type_name + " " + v_m_name + " __attribute__ (( vector_size(sizeof(" + original_type_name + ") * " + std::to_string(size) + ") ))";
            return;
        }
        if( declare_value ) {
            std::string variable_name = std::string(v_m_name) + "_value";
            sub = format_type_c("", type_name_without_ptr, variable_name, use_ref, dummy) + ";\n";
            sub += indent + format_type_c("", type_name, v_m_name, use_ref, dummy);
            sub += " = &" + variable_name;
            if( !is_pointer ) {
                sub += ";\n";
                if( !is_fixed_size ) {
                    sub += indent + format_type_c("*", type_name_copy, std::string(v_m_name) + "_data",
                                                use_ref, dummy);
                    if( dims.size() > 0 ) {
                        sub += " = " + dims + ";\n";
                    } else {
                        sub += ";\n";
                    }
                } else {
                    sub += indent + format_type_c(dims, type_name_copy, std::string(v_m_name) + "_data",
                                                use_ref, dummy) + ";\n";
                }
                sub += indent + std::string(v_m_name) + "->data = " + std::string(v_m_name) + "_data;\n";
                sub += indent + std::string(v_m_name) + "->n_dims = " + std::to_string(n_dims) + ";\n";
                sub += indent + std::string(v_m_name) + "->offset = " + std::to_string(0) + ";\n";
                std::string stride = "1";
                for (int i = n_dims - 1; i >= 0; i--) {
                    std::string start = "1", length = "0";
                    if( m_dims[i].m_start ) {
                        this->visit_expr(*m_dims[i].m_start);
                        start = src;
                    }
                    if( m_dims[i].m_length ) {
                        this->visit_expr(*m_dims[i].m_length);
                        length = src;
                    }
                    sub += indent + std::string(v_m_name) +
                        "->dims[" + std::to_string(i) + "].lower_bound = " + start + ";\n";
                    sub += indent + std::string(v_m_name) +
                        "->dims[" + std::to_string(i) + "].length = " + length + ";\n";
                    sub += indent + std::string(v_m_name) +
                        "->dims[" + std::to_string(i) + "].stride = " + stride + ";\n";
                    stride = "(" + stride + "*" + length + ")";
                }
                sub.pop_back();
                sub.pop_back();
            }
        } else {
            if( m_abi == ASR::abiType::BindC ) {
                sub = format_type_c("", type_name_copy, v_m_name + "[]", use_ref, dummy);
            } else {
                sub = format_type_c("", type_name, v_m_name, use_ref, dummy);
            }
        }
    }

    void allocate_array_members_of_struct(ASR::Struct_t* der_type_t, std::string& sub,
        std::string indent, std::string name) {
        for( auto itr: der_type_t->m_symtab->get_scope() ) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(itr.second);
            if( ASR::is_a<ASR::Union_t>(*sym) ||
                ASR::is_a<ASR::Struct_t>(*sym) ) {
                continue ;
            }
            ASR::ttype_t* mem_type = ASRUtils::symbol_type(sym);
            if( ASRUtils::is_character(*mem_type) ) {
                sub += indent + name + "->" + itr.first + " = NULL;\n";
            } else if( ASRUtils::is_array(mem_type) &&
                        ASR::is_a<ASR::Variable_t>(*itr.second) ) {
                ASR::Variable_t* mem_var = ASR::down_cast<ASR::Variable_t>(itr.second);
                std::string mem_var_name = current_scope->get_unique_name(itr.first + std::to_string(counter));
                counter += 1;
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(mem_type, m_dims);
                CDeclarationOptions c_decl_options_;
                c_decl_options_.pre_initialise_derived_type = true;
                c_decl_options_.use_ptr_for_derived_type = true;
                c_decl_options_.use_static = true;
                c_decl_options_.force_declare = true;
                c_decl_options_.force_declare_name = mem_var_name;
                c_decl_options_.do_not_initialize = true;
                sub += indent + convert_variable_decl(*mem_var, &c_decl_options_) + ";\n";
                if( !ASRUtils::is_fixed_size_array(m_dims, n_dims) ) {
                    sub += indent + name + "->" + itr.first + " = " + mem_var_name + ";\n";
                }
            } else if( ASR::is_a<ASR::StructType_t>(*mem_type) ) {
                // TODO: StructType
                // ASR::StructType_t* struct_t = ASR::down_cast<ASR::StructType_t>(mem_type);
                // ASR::Struct_t* struct_type_t = ASR::down_cast<ASR::Struct_t>(
                //     ASRUtils::symbol_get_past_external(struct_t->m_derived_type));
                // allocate_array_members_of_struct(struct_type_t, sub, indent, "(&(" + name + "->" + itr.first + "))");
            }
        }
    }

    void convert_variable_decl_util(const ASR::Variable_t &v,
        bool is_array, bool declare_as_constant, bool use_ref, bool dummy,
        bool force_declare, std::string &force_declare_name,
        size_t n_dims, ASR::dimension_t* m_dims, ASR::ttype_t* v_m_type,
        std::string &dims, std::string &sub) {
        std::string type_name = CUtils::get_c_type_from_ttype_t(v_m_type);
        if( is_array ) {
            bool is_fixed_size = true;
            dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
            bool is_struct_type_member = ASR::is_a<ASR::Struct_t>(
                    *ASR::down_cast<ASR::symbol_t>(v.m_parent_symtab->asr_owner));
            if( is_fixed_size && is_struct_type_member ) {
                if( !force_declare ) {
                    force_declare_name = std::string(v.m_name);
                }
                sub = type_name + " " + force_declare_name + dims;
            } else {
                std::string encoded_type_name = ASRUtils::get_type_code(v_m_type);
                if( !force_declare ) {
                    force_declare_name = std::string(v.m_name);
                }
                bool is_module_var = ASR::is_a<ASR::Module_t>(
                    *ASR::down_cast<ASR::symbol_t>(v.m_parent_symtab->asr_owner));
                bool is_simd_array = (ASR::is_a<ASR::Array_t>(*v.m_type) &&
                    ASR::down_cast<ASR::Array_t>(v.m_type)->m_physical_type
                        == ASR::array_physical_typeType::SIMDArray);
                generate_array_decl(sub, force_declare_name, type_name, dims,
                                    encoded_type_name, m_dims, n_dims,
                                    use_ref, dummy,
                                    (v.m_intent != ASRUtils::intent_in &&
                                    v.m_intent != ASRUtils::intent_inout &&
                                    v.m_intent != ASRUtils::intent_out &&
                                    v.m_intent != ASRUtils::intent_unspecified &&
                                    !is_struct_type_member && !is_module_var) || force_declare,
                                    is_fixed_size, false, ASR::abiType::Source, is_simd_array);
            }
        } else {
            bool is_fixed_size = true;
            std::string v_m_name = v.m_name;
            if( declare_as_constant ) {
                type_name = "const " + type_name;
                v_m_name = const_name;
            }
            dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
            sub = format_type_c(dims, type_name, v_m_name, use_ref, dummy);
        }
    }

    std::string convert_variable_decl(const ASR::Variable_t &v,
        DeclarationOptions* decl_options=nullptr)
    {
        bool pre_initialise_derived_type;
        bool use_ptr_for_derived_type;
        bool use_static;
        bool force_declare;
        std::string force_declare_name;
        bool declare_as_constant;
        std::string const_name;
        bool do_not_initialize;

        if( decl_options ) {
            CDeclarationOptions* c_decl_options = reinterpret_cast<CDeclarationOptions*>(decl_options);
            pre_initialise_derived_type = c_decl_options->pre_initialise_derived_type;
            use_ptr_for_derived_type = c_decl_options->use_ptr_for_derived_type;
            use_static = c_decl_options->use_static;
            force_declare = c_decl_options->force_declare;
            force_declare_name = c_decl_options->force_declare_name;
            declare_as_constant = c_decl_options->declare_as_constant;
            const_name = c_decl_options->const_name;
            do_not_initialize = c_decl_options->do_not_initialize;
        } else {
            pre_initialise_derived_type = true;
            use_ptr_for_derived_type = true;
            use_static = true;
            force_declare = false;
            force_declare_name = "";
            declare_as_constant = false;
            const_name = "";
            do_not_initialize = false;
        }
        std::string sub;
        bool use_ref = (v.m_intent == ASRUtils::intent_out ||
                        v.m_intent == ASRUtils::intent_inout);
        bool is_array = ASRUtils::is_array(v.m_type);
        bool dummy = ASRUtils::is_arg_dummy(v.m_intent);
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(v.m_type, m_dims);
        ASR::ttype_t* v_m_type = v.m_type;
        v_m_type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(v_m_type));
        if (ASRUtils::is_pointer(v_m_type)) {
            ASR::ttype_t *t2 = ASR::down_cast<ASR::Pointer_t>(v_m_type)->m_type;
            t2 = ASRUtils::type_get_past_array(t2);
            if (ASRUtils::is_integer(*t2)) {
                ASR::Integer_t *t = ASR::down_cast<ASR::Integer_t>(ASRUtils::type_get_past_array(t2));
                std::string type_name = "int" + std::to_string(t->m_kind * 8) + "_t";
                if( !ASRUtils::is_array(v_m_type) ) {
                    type_name.append(" *");
                }
                if( is_array ) {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
                    std::string encoded_type_name = "i" + std::to_string(t->m_kind * 8);
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims,
                                        encoded_type_name, m_dims, n_dims,
                                        use_ref, dummy,
                                        v.m_intent != ASRUtils::intent_in &&
                                        v.m_intent != ASRUtils::intent_inout &&
                                        v.m_intent != ASRUtils::intent_out &&
                                        v.m_intent != ASRUtils::intent_unspecified, is_fixed_size, true, ASR::abiType::Source, false);
                } else {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                    sub = format_type_c(dims, type_name, v.m_name, use_ref, dummy);
                }
            } else if (ASRUtils::is_unsigned_integer(*t2)) {
                ASR::UnsignedInteger_t *t = ASR::down_cast<ASR::UnsignedInteger_t>(ASRUtils::type_get_past_array(t2));
                std::string type_name = "uint" + std::to_string(t->m_kind * 8) + "_t";
                if( !ASRUtils::is_array(v_m_type) ) {
                    type_name.append(" *");
                }
                if( is_array ) {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
                    std::string encoded_type_name = "u" + std::to_string(t->m_kind * 8);
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims,
                                        encoded_type_name, m_dims, n_dims,
                                        use_ref, dummy,
                                        v.m_intent != ASRUtils::intent_in &&
                                        v.m_intent != ASRUtils::intent_inout &&
                                        v.m_intent != ASRUtils::intent_out &&
                                        v.m_intent != ASRUtils::intent_unspecified,
                                        is_fixed_size, true, ASR::abiType::Source, false);
                } else {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                    sub = format_type_c(dims, type_name, v.m_name, use_ref, dummy);
                }
            } else if (ASRUtils::is_real(*t2)) {
                ASR::Real_t *t = ASR::down_cast<ASR::Real_t>(t2);
                std::string type_name;
                if (t->m_kind == 4) {
                    type_name = "float";
                } else if (t->m_kind == 8) {
                    type_name = "double";
                } else {
                    diag.codegen_error_label("Real kind '"
                        + std::to_string(t->m_kind)
                        + "' not supported", {v.base.base.loc}, "");
                    throw Abort();
                }
                if( !ASRUtils::is_array(v_m_type) ) {
                    type_name.append(" *");
                }
                if( is_array ) {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
                    std::string encoded_type_name = ASRUtils::get_type_code(t2);
                    bool is_simd_array = (ASR::is_a<ASR::Array_t>(*v_m_type) &&
                        ASR::down_cast<ASR::Array_t>(v_m_type)->m_physical_type
                            == ASR::array_physical_typeType::SIMDArray);
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims,
                                        encoded_type_name, m_dims, n_dims,
                                        use_ref, dummy,
                                        v.m_intent != ASRUtils::intent_in &&
                                        v.m_intent != ASRUtils::intent_inout &&
                                        v.m_intent != ASRUtils::intent_out &&
                                        v.m_intent != ASRUtils::intent_unspecified,
                                        is_fixed_size, true, ASR::abiType::Source, is_simd_array);
                } else {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                    sub = format_type_c(dims, type_name, v.m_name, use_ref, dummy);
                }
            } else if(ASR::is_a<ASR::StructType_t>(*t2)) {
                std::string der_type_name = ASRUtils::symbol_name(v.m_type_declaration);
                if( is_array ) {
                    bool is_fixed_size = true;
                    std::string dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
                    std::string encoded_type_name = "x" + der_type_name;
                    std::string type_name = std::string("struct ") + der_type_name;
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims,
                                        encoded_type_name, m_dims, n_dims,
                                        use_ref, dummy,
                                        v.m_intent != ASRUtils::intent_in &&
                                        v.m_intent != ASRUtils::intent_inout &&
                                        v.m_intent != ASRUtils::intent_out &&
                                        v.m_intent != ASRUtils::intent_unspecified,
                                        is_fixed_size, false, ASR::abiType::Source, false);
                 } else {
                    std::string ptr_char = "*";
                    if( !use_ptr_for_derived_type ) {
                        ptr_char.clear();
                    }
                    sub = format_type_c("", "struct " + der_type_name + ptr_char,
                                        v.m_name, use_ref, dummy);
                }
            } else if(ASR::is_a<ASR::CPtr_t>(*t2)) {
                sub = format_type_c("", "void**", v.m_name, false, false);
            } else {
                diag.codegen_error_label("Type '"
                    + ASRUtils::type_to_str_python(t2)
                    + "' not supported", {v.base.base.loc}, "");
                throw Abort();
            }
        } else {
            std::string dims;
            use_ref = use_ref && !is_array;
            if (v.m_storage == ASR::storage_typeType::Parameter) {
                convert_variable_decl_util(v, is_array, declare_as_constant, use_ref, dummy,
                    force_declare, force_declare_name, n_dims, m_dims, v_m_type, dims, sub);
                if (v.m_intent != ASR::intentType::ReturnVar) {
                    sub = "const " + sub;
                }
            } else if (ASRUtils::is_integer(*v_m_type)) {
                headers.insert("inttypes.h");
                convert_variable_decl_util(v, is_array, declare_as_constant, use_ref, dummy,
                    force_declare, force_declare_name, n_dims, m_dims, v_m_type, dims, sub);
            } else if (ASRUtils::is_unsigned_integer(*v_m_type)) {
                headers.insert("inttypes.h");
                convert_variable_decl_util(v, is_array, declare_as_constant, use_ref, dummy,
                    force_declare, force_declare_name, n_dims, m_dims, v_m_type, dims, sub);
            } else if (ASRUtils::is_real(*v_m_type)) {
                convert_variable_decl_util(v, is_array, declare_as_constant, use_ref, dummy,
                    force_declare, force_declare_name, n_dims, m_dims, v_m_type, dims, sub);
            } else if (ASRUtils::is_complex(*v_m_type)) {
                headers.insert("complex.h");
                convert_variable_decl_util(v, is_array, declare_as_constant, use_ref, dummy,
                    force_declare, force_declare_name, n_dims, m_dims, v_m_type, dims, sub);
            } else if (ASRUtils::is_logical(*v_m_type)) {
                convert_variable_decl_util(v, is_array, declare_as_constant, use_ref, dummy,
                    force_declare, force_declare_name, n_dims, m_dims, v_m_type, dims, sub);
            } else if (ASRUtils::is_character(*v_m_type)) {
                bool is_fixed_size = true;
                dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                sub = format_type_c(dims, "char *", v.m_name, use_ref, dummy);
                if(v.m_intent == ASRUtils::intent_local &&
                    !(ASR::is_a<ASR::symbol_t>(*v.m_parent_symtab->asr_owner) &&
                      ASR::is_a<ASR::Struct_t>(
                        *ASR::down_cast<ASR::symbol_t>(v.m_parent_symtab->asr_owner))) &&
                    !(dims.size() == 0 && v.m_symbolic_value) && !do_not_initialize) {
                    sub += " = NULL";
                    return sub;
                }
            } else if (ASR::is_a<ASR::StructType_t>(*v_m_type)) {
                std::string indent(indentation_level*indentation_spaces, ' ');
                std::string der_type_name = ASRUtils::symbol_name(v.m_type_declaration);
                 if( is_array ) {
                    bool is_fixed_size = true;
                    dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
                    std::string encoded_type_name = "x" + der_type_name;
                    std::string type_name = std::string("struct ") + der_type_name;
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims,
                                        encoded_type_name, m_dims, n_dims,
                                        use_ref, dummy,
                                        v.m_intent != ASRUtils::intent_in &&
                                        v.m_intent != ASRUtils::intent_inout &&
                                        v.m_intent != ASRUtils::intent_out &&
                                        v.m_intent != ASRUtils::intent_unspecified,
                                        is_fixed_size, false, ASR::abiType::Source, false);
                } else if( v.m_intent == ASRUtils::intent_local && pre_initialise_derived_type) {
                    bool is_fixed_size = true;
                    dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                    std::string value_var_name = v.m_parent_symtab->get_unique_name(std::string(v.m_name) + "_value");
                    sub = format_type_c(dims, "struct " + der_type_name,
                                        value_var_name, use_ref, dummy);
                    if (v.m_symbolic_value && !do_not_initialize) {
                        this->visit_expr(*v.m_symbolic_value);
                        std::string init = src;
                        sub += "=" + init;
                    }
                    sub += ";\n";
                    std::string ptr_char = "*";
                    if( !use_ptr_for_derived_type ) {
                        ptr_char.clear();
                    }
                    sub += indent + format_type_c("", "struct " + der_type_name + ptr_char, v.m_name, use_ref, dummy);
                    if( n_dims != 0 ) {
                        sub += " = " + value_var_name;
                    } else {
                        sub += " = &" + value_var_name + ";\n";
                        ASR::Struct_t* der_type_t = ASR::down_cast<ASR::Struct_t>(
                        ASRUtils::symbol_get_past_external(v.m_type_declaration));
                        allocate_array_members_of_struct(der_type_t, sub, indent, std::string(v.m_name));
                        sub.pop_back();
                        sub.pop_back();
                    }
                    return sub;
                } else {
                    bool is_fixed_size = true;
                    dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                    if( v.m_intent == ASRUtils::intent_in ||
                        v.m_intent == ASRUtils::intent_inout ||
                        v.m_intent == ASRUtils::intent_out ) {
                        use_ref = false;
                        dims = "";
                    }
                    std::string ptr_char = "*";
                    if( !use_ptr_for_derived_type ) {
                        ptr_char.clear();
                    }
                    sub = format_type_c(dims, "struct " + der_type_name + ptr_char,
                                        v.m_name, use_ref, dummy);
                }
            } else if (ASR::is_a<ASR::UnionType_t>(*v_m_type)) {
                std::string indent(indentation_level*indentation_spaces, ' ');
                ASR::UnionType_t *t = ASR::down_cast<ASR::UnionType_t>(v_m_type);
                std::string der_type_name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(t->m_union_type));
                if( is_array ) {
                    bool is_fixed_size = true;
                    dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size, true);
                    std::string encoded_type_name = "x" + der_type_name;
                    std::string type_name = std::string("union ") + der_type_name;
                    generate_array_decl(sub, std::string(v.m_name), type_name, dims,
                                        encoded_type_name, m_dims, n_dims,
                                        use_ref, dummy,
                                        v.m_intent != ASRUtils::intent_in &&
                                        v.m_intent != ASRUtils::intent_inout &&
                                        v.m_intent != ASRUtils::intent_out &&
                                        v.m_intent != ASRUtils::intent_unspecified, is_fixed_size,
                                        false,
                                        ASR::abiType::Source, false);
                } else {
                    bool is_fixed_size = true;
                    dims = convert_dims_c(n_dims, m_dims, v_m_type, is_fixed_size);
                    if( v.m_intent == ASRUtils::intent_in ||
                        v.m_intent == ASRUtils::intent_inout ) {
                        use_ref = false;
                        dims = "";
                    }
                    sub = format_type_c(dims, "union " + der_type_name,
                                        v.m_name, use_ref, dummy);
                }
            } else if (ASR::is_a<ASR::List_t>(*v_m_type)) {
                ASR::List_t* t = ASR::down_cast<ASR::List_t>(v_m_type);
                std::string list_type_c = c_ds_api->get_list_type(t);
                std::string name = v.m_name;
                if (v.m_intent == ASRUtils::intent_out) {
                    name = "*" + name;
                }
                sub = format_type_c("", list_type_c, name,
                                    false, false);
            } else if (ASR::is_a<ASR::Tuple_t>(*v_m_type)) {
                ASR::Tuple_t* t = ASR::down_cast<ASR::Tuple_t>(v_m_type);
                std::string tuple_type_c = c_ds_api->get_tuple_type(t);
                std::string name = v.m_name;
                if (v.m_intent == ASRUtils::intent_out) {
                    name = "*" + name;
                }
                sub = format_type_c("", tuple_type_c, name,
                                    false, false);
            } else if (ASR::is_a<ASR::Dict_t>(*v_m_type)) {
                ASR::Dict_t* t = ASR::down_cast<ASR::Dict_t>(v_m_type);
                std::string dict_type_c = c_ds_api->get_dict_type(t);
                std::string name = v.m_name;
                if (v.m_intent == ASRUtils::intent_out) {
                    name = "*" + name;
                }
                sub = format_type_c("", dict_type_c, name,
                                    false, false);
            } else if (ASR::is_a<ASR::CPtr_t>(*v_m_type)) {
                sub = format_type_c("", "void*", v.m_name, false, false);
            } else if (ASR::is_a<ASR::EnumType_t>(*v_m_type)) {
                ASR::EnumType_t* enum_ = ASR::down_cast<ASR::EnumType_t>(v_m_type);
                ASR::Enum_t* enum_type = ASR::down_cast<ASR::Enum_t>(enum_->m_enum_type);
                sub = format_type_c("", "enum " + std::string(enum_type->m_name), v.m_name, false, false);
            } else if (ASR::is_a<ASR::TypeParameter_t>(*v_m_type)) {
                // Ignore type variables
                return "";
            } else {
                diag.codegen_error_label("Type '"
                    + ASRUtils::type_to_str_python(v_m_type)
                    + "' not supported", {v.base.base.loc}, "");
                throw Abort();
            }
            if (dims.size() == 0 && v.m_storage == ASR::storage_typeType::Save && use_static) {
                sub = "static " + sub;
            }
            if (dims.size() == 0 && v.m_symbolic_value && !do_not_initialize) {
                ASR::expr_t* init_expr = v.m_symbolic_value;
                if( v.m_storage != ASR::storage_typeType::Parameter ) {
                    for( size_t i = 0; i < v.n_dependencies; i++ ) {
                        std::string variable_name = v.m_dependencies[i];
                        ASR::symbol_t* dep_sym = current_scope->resolve_symbol(variable_name);
                        if( (dep_sym && ASR::is_a<ASR::Variable_t>(*dep_sym) &&
                            !ASR::down_cast<ASR::Variable_t>(dep_sym)->m_symbolic_value) )  {
                            init_expr = nullptr;
                            break;
                        }
                    }
                }
                if( init_expr ) {
                    if (is_c && ASR::is_a<ASR::StringChr_t>(*init_expr)) {
                        // TODO: Not supported yet
                    } else {
                        this->visit_expr(*init_expr);
                        std::string init = src;
                        sub += " = " + init;
                    }
                }
            }
        }
        return sub;
    }


    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        is_string_concat_present = false;
        global_scope = x.m_symtab;
        // All loose statements must be converted to a function, so the items
        // must be empty:
        LCOMPILERS_ASSERT(x.n_items == 0);
        std::string unit_src = "";
        indentation_level = 0;
        indentation_spaces = 4;
        c_ds_api->set_indentation(indentation_level, indentation_spaces);
        c_ds_api->set_global_scope(global_scope);
        c_utils_functions->set_indentation(indentation_level, indentation_spaces);
        c_utils_functions->set_global_scope(global_scope);
        c_ds_api->set_c_utils_functions(c_utils_functions.get());
        bind_py_utils_functions->set_indentation(indentation_level, indentation_spaces);
        bind_py_utils_functions->set_global_scope(global_scope);
        std::string head =
R"(
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <lfortran_intrinsics.h>

)";
        if(compiler_options.target_offload_enabled) {
            head += R"(
#ifdef USE_GPU
#include<cuda_runtime.h>
#else
#include"cuda_cpu_runtime.h"
#endif
)";
        }

        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string tab(indentation_spaces, ' ');

        std::string unit_src_tmp;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                unit_src_tmp = convert_variable_decl(*v);
                unit_src += unit_src_tmp;
                if(unit_src_tmp.size() > 0) {
                    unit_src += ";\n";
                }
            }
        }


        std::map<std::string, std::vector<std::string>> struct_dep_graph;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Struct_t>(*item.second) ||
                ASR::is_a<ASR::Enum_t>(*item.second) ||
                ASR::is_a<ASR::Union_t>(*item.second)) {
                std::vector<std::string> struct_deps_vec;
                std::pair<char**, size_t> struct_deps_ptr = ASRUtils::symbol_dependencies(item.second);
                for( size_t i = 0; i < struct_deps_ptr.second; i++ ) {
                    struct_deps_vec.push_back(std::string(struct_deps_ptr.first[i]));
                }
                struct_dep_graph[item.first] = struct_deps_vec;
            }
        }

        std::vector<std::string> struct_deps = ASRUtils::order_deps(struct_dep_graph);

        for (auto &item : struct_deps) {
            ASR::symbol_t* struct_sym = x.m_symtab->get_symbol(item);
            visit_symbol(*struct_sym);
            array_types_decls += src;
        }

        // Topologically sort all global functions
        // and then define them in the right order
        std::vector<std::string> global_func_order = ASRUtils::determine_function_definition_order(x.m_symtab);

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
                    if( ASRUtils::get_body_size(mod) != 0 ) {
                        visit_symbol(*mod);
                        unit_src += src;
                    }
                }
            }
        }

        // Process global functions
        size_t i;
        for (i = 0; i < global_func_order.size(); i++) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(global_func_order[i]);
            // Ignore external symbols because they are already defined by the loop above.
            if( !sym || ASR::is_a<ASR::ExternalSymbol_t>(*sym) ) {
                continue ;
            }
            visit_symbol(*sym);
            unit_src += src;
        }

        // Process modules in the right order
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

        forward_decl_functions += "\n\n";
        src = get_final_combined_src(head, unit_src);

        if (!emit_headers.empty()) {
            std::string to_includes_1 = "";
            for (auto &s: headers) {
                to_includes_1 += "#include <" + s + ">\n";
            }
            for (auto &f_name: emit_headers) {
                std::ofstream out_file;
                std::string out_src = to_includes_1 + head + f_name.second;
                std::string ifdefs = f_name.first.substr(0, f_name.first.length() - 2);
                std::transform(ifdefs.begin(), ifdefs.end(), ifdefs.begin(), ::toupper);
                ifdefs += "_H";
                out_src = "#ifndef " + ifdefs + "\n#define " + ifdefs + "\n\n" + out_src;
                out_src += "\n\n#endif\n";
                out_file.open(f_name.first);
                out_file << out_src;
                out_file.close();
            }
        }
    }

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_intrinsic) {
            intrinsic_module = true;
        } else {
            intrinsic_module = false;
        }

        if(to_lower(x.m_name) == "omp_lib") {
            headers.insert("omp.h");
            return;
        } else if (to_lower(x.m_name) == "iso_c_binding" || to_lower(x.m_name) == "lfortran_intrinsic_iso_c_binding") {
            return;
        }

        std::string unit_src = "";
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                std::string unit_src_tmp;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                unit_src_tmp = convert_variable_decl(*v);
                unit_src += unit_src_tmp;
                if(unit_src_tmp.size() > 0) {
                    unit_src += ";\n";
                }
            }
        }
        std::map<std::string, std::vector<std::string>> struct_dep_graph;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Struct_t>(*item.second) ||
                    ASR::is_a<ASR::Enum_t>(*item.second) ||
                    ASR::is_a<ASR::Union_t>(*item.second)) {
                std::vector<std::string> struct_deps_vec;
                std::pair<char**, size_t> struct_deps_ptr = ASRUtils::symbol_dependencies(item.second);
                for( size_t i = 0; i < struct_deps_ptr.second; i++ ) {
                    struct_deps_vec.push_back(std::string(struct_deps_ptr.first[i]));
                }
                struct_dep_graph[item.first] = struct_deps_vec;
            }
        }

        std::vector<std::string> struct_deps = ASRUtils::order_deps(struct_dep_graph);
        for (auto &item : struct_deps) {
            ASR::symbol_t* struct_sym = x.m_symtab->get_symbol(item);
            visit_symbol(*struct_sym);
        }

        // Topologically sort all module functions
        // and then define them in the right order
        std::vector<std::string> func_order = ASRUtils::determine_function_definition_order(x.m_symtab);
        for (auto &item : func_order) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(item);
            ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(sym);
            visit_Function(*s);
            unit_src += src;
        }
        src = unit_src;
        intrinsic_module = false;
    }

    void visit_Program(const ASR::Program_t &x) {
        // Topologically sort all program functions
        // and then define them in the right order
        std::vector<std::string> func_order = ASRUtils::determine_function_definition_order(x.m_symtab);

        // Generate code for nested subroutines and functions first:
        std::string contains;
        for (auto &item : func_order) {
            ASR::symbol_t* sym = x.m_symtab->get_symbol(item);
            ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(sym);
            visit_Function(*s);
            contains += src;
        }

        // Generate code for the main program
        indentation_level += 1;
        std::string indent1(indentation_level*indentation_spaces, ' ');
        std::string decl;
        // Topologically sort all program functions
        // and then define them in the right order
        std::vector<std::string> var_order = ASRUtils::determine_variable_declaration_order(x.m_symtab);
        std::string decl_tmp;
        for (auto &item : var_order) {
            ASR::symbol_t* var_sym = x.m_symtab->get_symbol(item);
            if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(var_sym);
                decl += indent1;
                decl_tmp = convert_variable_decl(*v);
                decl += decl_tmp;
                if(decl_tmp.size() > 0) {
                    decl += ";\n";
                }
            }
        }

        std::string body;
        if (compiler_options.enable_cpython) {
            headers.insert("Python.h");
            body += R"(
    Py_Initialize();
    wchar_t* argv1 = Py_DecodeLocale("", NULL);
    wchar_t** argv_ = {&argv1};
    PySys_SetArgv(1, argv_);
)";
            body += "\n";
        }

        if (compiler_options.link_numpy) {
            user_defines.insert("NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION");
            headers.insert("numpy/arrayobject.h");
            body +=
R"(    // Initialise Numpy
    if (_import_array() < 0) {
        PyErr_Print();
        PyErr_SetString(PyExc_ImportError, "numpy.core.multiarray failed to import");
        fprintf(stderr, "Failed to import numpy Python module(s)\n");
        return -1;
    }
)";
            body += "\n";
        }

        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            body += src;
        }

        if (compiler_options.enable_cpython) {
            body += R"(
    if (Py_FinalizeEx() < 0) {
        fprintf(stderr,"BindPython: Unknown Error\n");
        exit(1);
    }
)";
            body += "\n";
        }

        if(compiler_options.target_offload_enabled) {
            contains += kernel_func_code;
        }

        if (target_offload_enabled && !kernel_func_names.empty()) {
            std::string dispatch_code = "#ifndef USE_GPU\n";
            dispatch_code += "void compute_kernel_wrapper(void **args, void *func) {\n";
            for (const auto &kname : kernel_func_names) {
                dispatch_code += "    if (func == (void*)" + kname + ") {\n";
                dispatch_code += "        " + kname + "_wrapper(args);\n";
                dispatch_code += "        return;\n";
                dispatch_code += "    }\n";
            }
            dispatch_code += "    fprintf(stderr, \"Unknown kernel function\\n\");\n";
            dispatch_code += "    exit(1);\n";
            dispatch_code += "}\n#endif\n";
            contains += dispatch_code;
        }

        src = contains
                + "int main(int argc, char* argv[])\n{\n"
                + indent1 + "_lpython_set_argv(argc, argv);\n"
                + decl + body
                + indent1 + "return 0;\n}\n";
        indentation_level -= 2;
    }

    template <typename T>
    void visit_AggregateTypeUtil(const T& x, std::string c_type_name,
        std::string& src_dest) {
        std::string body = "";
        int indendation_level_copy = indentation_level;
        for( auto itr: x.m_symtab->get_scope() ) {
            if( ASR::is_a<ASR::Union_t>(*itr.second) ) {
                visit_AggregateTypeUtil(*ASR::down_cast<ASR::Union_t>(itr.second),
                                        "union", src_dest);
            } else if( ASR::is_a<ASR::Struct_t>(*itr.second) ) {
                std::string struct_c_type_name = get_StructTypeCTypeName(
                    *ASR::down_cast<ASR::Struct_t>(itr.second));
                visit_AggregateTypeUtil(*ASR::down_cast<ASR::Struct_t>(itr.second),
                                        struct_c_type_name, src_dest);
            }
        }
        indentation_level = indendation_level_copy;
        std::string indent(indentation_level*indentation_spaces, ' ');
        indentation_level += 1;
        std::string open_struct = indent + c_type_name + " " + std::string(x.m_name) + " {\n";
        indent.push_back(' ');
        CDeclarationOptions c_decl_options_;
        c_decl_options_.pre_initialise_derived_type = false;
        c_decl_options_.use_ptr_for_derived_type = false;
        c_decl_options_.do_not_initialize = true;
        for( size_t i = 0; i < x.n_members; i++ ) {
            ASR::symbol_t* member = x.m_symtab->get_symbol(x.m_members[i]);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*member));
            body += indent + convert_variable_decl(
                        *ASR::down_cast<ASR::Variable_t>(member),
                        &c_decl_options_);
            body += ";\n";
        }
        indentation_level -= 1;
        std::string end_struct = "};\n\n";
        src_dest += open_struct + body + end_struct;
    }

    std::string get_StructTypeCTypeName(const ASR::Struct_t& x) {
        std::string c_type_name = "struct";
        if( x.m_is_packed ) {
            std::string attr_args = "(packed";
            if( x.m_alignment ) {
                LCOMPILERS_ASSERT(ASRUtils::expr_value(x.m_alignment));
                ASR::expr_t* alignment_value = ASRUtils::expr_value(x.m_alignment);
                int64_t alignment_int = -1;
                if( !ASRUtils::extract_value(alignment_value, alignment_int) ) {
                    LCOMPILERS_ASSERT(false);
                }
                attr_args += ", aligned(" + std::to_string(alignment_int) + ")";
            }
            attr_args += ")";
            c_type_name += " __attribute__(" + attr_args + ")";
        }
        return c_type_name;
    }

    void visit_Struct(const ASR::Struct_t& x) {
        src = "";
        std::string c_type_name = get_StructTypeCTypeName(x);
        visit_AggregateTypeUtil(x, c_type_name, array_types_decls);
        src = "";
    }

    void visit_Union(const ASR::Union_t& x) {
        visit_AggregateTypeUtil(x, "union", array_types_decls);
    }

    void visit_Enum(const ASR::Enum_t& x) {
        if( x.m_enum_value_type == ASR::enumtypeType::NonInteger ) {
            throw CodeGenError("C backend only supports integer valued EnumType. " +
                std::string(x.m_name) + " is not integer valued.");
        }
        if( x.m_enum_value_type == ASR::enumtypeType::IntegerNotUnique ) {
            throw CodeGenError("C backend only supports uniquely valued integer EnumType. " +
                std::string(x.m_name) + " EnumType is having duplicate values for its members.");
        }
        if( x.m_enum_value_type == ASR::enumtypeType::IntegerUnique &&
            x.m_abi == ASR::abiType::BindC ) {
            throw CodeGenError("C-interoperation support for non-consecutive but uniquely "
                               "valued integer enums isn't available yet.");
        }
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string tab(indentation_spaces, ' ');
        std::string meta_data = " = {";
        std::string open_struct = indent + "enum " + std::string(x.m_name) + " {\n";
        std::string body = "";
        int64_t min_value = INT64_MAX;
        int64_t max_value = INT64_MIN;
        size_t max_name_len = 0;
        for( size_t i = 0; i < x.n_members; i++ ) {
            ASR::symbol_t* member = x.m_symtab->get_symbol(x.m_members[i]);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*member));
            ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member);
            ASR::expr_t* value = ASRUtils::expr_value(member_var->m_symbolic_value);
            int64_t value_int64 = -1;
            ASRUtils::extract_value(value, value_int64);
            min_value = std::min(value_int64, min_value);
            max_value = std::max(value_int64, max_value);
            max_name_len = std::max(max_name_len, std::string(x.m_members[i]).size());
            this->visit_expr(*member_var->m_symbolic_value);
            body += indent + tab + std::string(member_var->m_name) + " = " + src + ",\n";
        }
        size_t max_names = max_value - min_value + 1;
        std::vector<std::string> enum_names(max_names, "\"\"");
        for( size_t i = 0; i < x.n_members; i++ ) {
            ASR::symbol_t* member = x.m_symtab->get_symbol(x.m_members[i]);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*member));
            ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member);
            ASR::expr_t* value = ASRUtils::expr_value(member_var->m_symbolic_value);
            int64_t value_int64 = -1;
            ASRUtils::extract_value(value, value_int64);
            min_value = std::min(value_int64, min_value);
            enum_names[value_int64 - min_value] = "\"" + std::string(member_var->m_name) + "\"";
        }
        for( auto enum_name: enum_names ) {
            meta_data += enum_name + ", ";
        }
        meta_data.pop_back();
        meta_data.pop_back();
        meta_data += "};\n";
        std::string end_struct = "};\n\n";
        std::string enum_names_type = "char " + global_scope->get_unique_name("enum_names_" + std::string(x.m_name)) +
         + "[" + std::to_string(max_names) + "][" + std::to_string(max_name_len + 1) + "] ";
        array_types_decls += enum_names_type + meta_data + open_struct + body + end_struct;
        src = "";
    }

    void visit_EnumConstructor(const ASR::EnumConstructor_t& x) {
        LCOMPILERS_ASSERT(x.n_args == 1);
        ASR::expr_t* m_arg = x.m_args[0];
        this->visit_expr(*m_arg);
        ASR::Enum_t* enum_type = ASR::down_cast<ASR::Enum_t>(x.m_dt_sym);
        src = "(enum " + std::string(enum_type->m_name) + ") (" + src + ")";
    }

    void visit_UnionConstructor(const ASR::UnionConstructor_t& /*x*/) {

    }

    void visit_EnumStaticMember(const ASR::EnumStaticMember_t& x) {
        CHECK_FAST_C(compiler_options, x)
        ASR::Variable_t* enum_var = ASR::down_cast<ASR::Variable_t>(x.m_m);
        src = std::string(enum_var->m_name);
    }

    void visit_EnumValue(const ASR::EnumValue_t& x) {
        CHECK_FAST_C(compiler_options, x)
        visit_expr(*x.m_v);
    }

    void visit_EnumName(const ASR::EnumName_t& x) {
        CHECK_FAST_C(compiler_options, x)
        int64_t min_value = INT64_MAX;
        ASR::EnumType_t* enum_t = ASR::down_cast<ASR::EnumType_t>(x.m_enum_type);
        ASR::Enum_t* enum_type = ASR::down_cast<ASR::Enum_t>(enum_t->m_enum_type);
        for( auto itr: enum_type->m_symtab->get_scope() ) {
            ASR::Variable_t* itr_var = ASR::down_cast<ASR::Variable_t>(itr.second);
            ASR::expr_t* value = ASRUtils::expr_value(itr_var->m_symbolic_value);
            int64_t value_int64 = -1;
            ASRUtils::extract_value(value, value_int64);
            min_value = std::min(value_int64, min_value);
        }
        visit_expr(*x.m_v);
        std::string enum_var_name = src;
        src = global_scope->get_unique_name("enum_names_" + std::string(enum_type->m_name)) +
                "[" + std::string(enum_var_name) + " - " + std::to_string(min_value) + "]";
    }

    void visit_ComplexConstant(const ASR::ComplexConstant_t &x) {
        headers.insert("complex.h");
        std::string re = std::to_string(x.m_re);
        std::string im = std::to_string(x.m_im);
        src = "CMPLX(" + re + ", " + im + ")";

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

    void visit_Assert(const ASR::Assert_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string out = indent;
        bracket_open++;
        visit_expr(*x.m_test);
        std::string test_condition = src;
        if (x.m_msg) {
            this->visit_expr(*x.m_msg);
            std::string tmp_gen = "";
            ASR::ttype_t* value_type = ASRUtils::expr_type(x.m_msg);
            if( ASR::is_a<ASR::List_t>(*value_type) ||
                ASR::is_a<ASR::Tuple_t>(*value_type)) {
                std::string p_func = c_ds_api->get_print_func(value_type);
                tmp_gen += indent + p_func + "(" + src + ");\n";
            } else {
                tmp_gen += "\"";
                tmp_gen += c_ds_api->get_print_type(value_type, ASR::is_a<ASR::ArrayItem_t>(*x.m_msg));
                tmp_gen += "\", ";
                if( ASRUtils::is_array(value_type) ) {
                    src += "->data";
                }
                if (ASR::is_a<ASR::Complex_t>(*value_type)) {
                    tmp_gen += "creal(" + src + ")";
                    tmp_gen += ", ";
                    tmp_gen += "cimag(" + src + ")";
                } else {
                    tmp_gen += src;
                }
            }
            out += "ASSERT_MSG(";
            out += test_condition + ", ";
            out += tmp_gen + ");\n";
        } else {
            out += "ASSERT(";
            out += test_condition + ");\n";
        }
        bracket_open--;
        src = check_tmp_buffer() + out;
    }

    void visit_CPtrToPointer(const ASR::CPtrToPointer_t& x) {
        visit_expr(*x.m_cptr);
        std::string source_src = std::move(src);
        visit_expr(*x.m_ptr);
        std::string dest_src = std::move(src);
        src = "";
        std::string indent(indentation_level*indentation_spaces, ' ');
        ASR::ArrayConstant_t* lower_bounds = nullptr;
        if( x.m_lower_bounds ) {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*x.m_lower_bounds));
            lower_bounds = ASR::down_cast<ASR::ArrayConstant_t>(x.m_lower_bounds);
        }
        if( ASRUtils::is_array(ASRUtils::expr_type(x.m_ptr)) ) {
            std::string dim_set_code = "";
            ASR::dimension_t* m_dims = nullptr;
            int n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(x.m_ptr), m_dims);
            dim_set_code = indent + dest_src + "->n_dims = " + std::to_string(n_dims) + ";\n";
            dim_set_code = indent + dest_src + "->offset = 0;\n";
            std::string stride = "1";
            for (int i = n_dims - 1; i >= 0; i--) {
                std::string start = "0", length = "0";
                if( lower_bounds ) {
                    start = ASRUtils::fetch_ArrayConstant_value(lower_bounds, i);
                }
                if( m_dims[i].m_length ) {
                    this->visit_expr(*m_dims[i].m_length);
                    length = src;
                }
                dim_set_code += indent + dest_src +
                    "->dims[" + std::to_string(i) + "].lower_bound = " + start + ";\n";
                dim_set_code += indent + dest_src +
                    "->dims[" + std::to_string(i) + "].length = " + length + ";\n";
                dim_set_code += indent + dest_src +
                    "->dims[" + std::to_string(i) + "].stride = " + stride + ";\n";
                stride = "(" + stride + "*" + length + ")";
            }
            src.clear();
            src += dim_set_code;
            dest_src += "->data";
        }
        std::string type_src = CUtils::get_c_type_from_ttype_t(ASRUtils::expr_type(x.m_ptr));
        src += indent + dest_src + " = (" + type_src + ") " + source_src + ";\n";
    }

    void visit_Print(const ASR::Print_t &x) {
        std::string indent(indentation_level*indentation_spaces, ' ');
        std::string tmp_gen = indent + "printf(\"", out = "";
        bracket_open++;
        std::vector<std::string> v;
        std::string separator;
        separator = "\" \"";
        //HACKISH way to handle print refactoring (always using stringformat).
        // TODO : Implement stringformat visitor.
        ASR::StringFormat_t* str_fmt;
        size_t n_values = 0;
        if(ASR::is_a<ASR::StringFormat_t>(*x.m_text)){
            str_fmt = ASR::down_cast<ASR::StringFormat_t>(x.m_text);
            n_values = str_fmt->n_args;
        } else if (ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(x.m_text))) {
            this->visit_expr(*x.m_text);
            src = indent + "printf(\"%s\\n\"," + src + ");\n";
            return;
        } else {
            throw CodeGenError("print statment supported for stringformat and single character argument",
            x.base.base.loc);
        }

        for (size_t i=0; i<n_values; i++) {
            this->visit_expr(*(str_fmt->m_args[i]));
            ASR::ttype_t* value_type = ASRUtils::expr_type(str_fmt->m_args[i]);
            if( ASRUtils::is_array(value_type) ) {
                src += "->data";
            }
            if( ASR::is_a<ASR::List_t>(*value_type) ||
                ASR::is_a<ASR::Tuple_t>(*value_type)) {
                tmp_gen += "\"";
                if (!v.empty()) {
                    for (auto &s: v) {
                        tmp_gen += ", " + s;
                    }
                }
                tmp_gen += ");\n";
                out += tmp_gen;
                tmp_gen = indent + "printf(\"";
                v.clear();
                std::string p_func = c_ds_api->get_print_func(value_type);
                out += indent + p_func + "(" + src + ");\n";
                continue;
            }
            tmp_gen +=c_ds_api->get_print_type(value_type, ASR::is_a<ASR::ArrayItem_t>(*(str_fmt->m_args[i])));
            v.push_back(src);
            if (ASR::is_a<ASR::Complex_t>(*value_type)) {
                v.pop_back();
                v.push_back("creal(" + src + ")");
                v.push_back("cimag(" + src + ")");
            }
            if (i+1!=n_values) {
                tmp_gen += "\%s";
                v.push_back(separator);
            }
        }
        tmp_gen += "\\n\"";
        if (!v.empty()) {
            for (auto &s: v) {
                tmp_gen += ", " + s;
            }
        }
        tmp_gen += ");\n";
        bracket_open--;
        out += tmp_gen;
        src = this->check_tmp_buffer() + out;
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t &x) {
        /*
            !LF$ attributes simd :: A
            real :: A(8)
            A = 1
            We need to generate:
            a = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
        */
        size_t size = ASRUtils::get_fixed_size_of_array(x.m_type);
        std::string array_const_str = "{";
        for( size_t i = 0; i < size; i++ ) {
            this->visit_expr(*x.m_array);
            array_const_str += src;
            if (i < size - 1) array_const_str += ", ";
        }
        array_const_str += "}";
        src = array_const_str;
    }

    void visit_ArraySize(const ASR::ArraySize_t& x) {
        CHECK_FAST_C(compiler_options, x)
        visit_expr(*x.m_v);
        std::string var_name = src;
        std::string args = "";
        std::string result_type = CUtils::get_c_type_from_ttype_t(x.m_type);
        if (x.m_dim == nullptr) {
            std::string array_size_func = c_utils_functions->get_array_size();
            ASR::dimension_t* m_dims = nullptr;
            int n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(x.m_v), m_dims);
            src = "((" + result_type + ") " + array_size_func + "(" + var_name + "->dims, " + std::to_string(n_dims) + "))";
        } else {
            visit_expr(*x.m_dim);
            std::string idx = src;
            src = "((" + result_type + ")" + var_name + "->dims[" + idx + "-1].length)";
        }
    }

    void visit_ArrayReshape(const ASR::ArrayReshape_t& x) {
        CHECK_FAST_C(compiler_options, x)
        visit_expr(*x.m_array);
        std::string array = src;
        visit_expr(*x.m_shape);
        std::string shape = src;

        ASR::ttype_t* array_type_asr = ASRUtils::expr_type(x.m_array);
        std::string array_type_name = CUtils::get_c_type_from_ttype_t(array_type_asr);
        std::string array_encoded_type_name = ASRUtils::get_type_code(array_type_asr, true, false, false);
        std::string array_type = c_ds_api->get_array_type(array_type_name, array_encoded_type_name, array_types_decls, true);
        std::string return_type = c_ds_api->get_array_type(array_type_name, array_encoded_type_name, array_types_decls, false);

        ASR::ttype_t* shape_type_asr = ASRUtils::expr_type(x.m_shape);
        std::string shape_type_name = CUtils::get_c_type_from_ttype_t(shape_type_asr);
        std::string shape_encoded_type_name = ASRUtils::get_type_code(shape_type_asr, true, false, false);
        std::string shape_type = c_ds_api->get_array_type(shape_type_name, shape_encoded_type_name, array_types_decls, true);

        std::string array_reshape_func = c_utils_functions->get_array_reshape(array_type, shape_type,
            return_type, array_type_name, array_encoded_type_name);
        src = array_reshape_func + "(" + array + ", " + shape + ")";
    }

    void visit_ArrayBound(const ASR::ArrayBound_t& x) {
        CHECK_FAST_C(compiler_options, x)
        visit_expr(*x.m_v);
        std::string var_name = src;
        std::string args = "";
        std::string result_type = CUtils::get_c_type_from_ttype_t(x.m_type);
        visit_expr(*x.m_dim);
        std::string idx = src;
        if( x.m_bound == ASR::arrayboundType::LBound ) {
            if (ASRUtils::is_simd_array(x.m_v)) {
                src = "0";
            } else {
                src = "((" + result_type + ")" + var_name + "->dims[" + idx + "-1].lower_bound)";
            }
        } else if( x.m_bound == ASR::arrayboundType::UBound ) {
            if (ASRUtils::is_simd_array(x.m_v)) {
                int64_t size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(x.m_v));
                src = std::to_string(size - 1);
            } else {
                std::string lower_bound = var_name + "->dims[" + idx + "-1].lower_bound";
                std::string length = var_name + "->dims[" + idx + "-1].length";
                std::string upper_bound = length + " + " + lower_bound + " - 1";
                src = "((" + result_type + ") " + upper_bound + ")";
            }
        }
    }

    void visit_ArrayConstant(const ASR::ArrayConstant_t& x) {
        // TODO: Support and test for multi-dimensional array constants
        headers.insert("stdarg.h");
        std::string array_const = "";
        for( size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(x.m_type); i++ ) {
            array_const += ASRUtils::fetch_ArrayConstant_value(x, i) + ", ";
        }
        array_const.pop_back();
        array_const.pop_back();

        ASR::ttype_t* array_type_asr = x.m_type;
        std::string array_type_name = CUtils::get_c_type_from_ttype_t(array_type_asr);
        std::string array_encoded_type_name = ASRUtils::get_type_code(array_type_asr, true, false);
        std::string return_type = c_ds_api->get_array_type(array_type_name, array_encoded_type_name,array_types_decls, false);

        src = c_utils_functions->get_array_constant(return_type, array_type_name, array_encoded_type_name) +
                "(" + std::to_string(ASRUtils::get_fixed_size_of_array(x.m_type)) + ", " + array_const + ")";
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        CHECK_FAST_C(compiler_options, x)
        this->visit_expr(*x.m_v);
        std::string array = src;
        ASR::ttype_t* x_mv_type = ASRUtils::expr_type(x.m_v);
        ASR::dimension_t* m_dims;
        int n_dims = ASRUtils::extract_dimensions_from_ttype(x_mv_type, m_dims);
        bool is_data_only_array = ASRUtils::is_fixed_size_array(m_dims, n_dims) &&
                                  ASR::is_a<ASR::Struct_t>(*ASRUtils::get_asr_owner(x.m_v));
        if( is_data_only_array || ASRUtils::is_simd_array(x.m_v)) {
            std::string index = "";
            std::string out = array;
            out += "[";
            for (size_t i=0; i<x.n_args; i++) {
                if (x.m_args[i].m_right) {
                    this->visit_expr(*x.m_args[i].m_right);
                } else {
                    src = "/* FIXME right index */";
                }

                if (ASRUtils::is_simd_array(x.m_v)) {
                    index += src;
                } else {
                    std::string current_index = "";
                    current_index += src;
                    for( size_t j = 0; j < i; j++ ) {
                        int64_t dim_size = 0;
                        ASRUtils::extract_value(m_dims[j].m_length, dim_size);
                        std::string length = std::to_string(dim_size);
                        current_index += " * " + length;
                    }
                    index += current_index;
                }
                if (i < x.n_args - 1) {
                    index += " + ";
                }
            }
            out += index + "]";
            last_expr_precedence = 2;
            src = out;
            return;
        }

        std::vector<std::string> indices;
        for( size_t r = 0; r < x.n_args; r++ ) {
            ASR::array_index_t curr_idx = x.m_args[r];
            this->visit_expr(*curr_idx.m_right);
            indices.push_back(src);
        }

        ASR::ttype_t* x_mv_type_ = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(x_mv_type));
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Array_t>(*x_mv_type_));
        ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(x_mv_type_);
        std::vector<std::string> diminfo;
        if( array_t->m_physical_type == ASR::array_physical_typeType::PointerToDataArray ||
                array_t->m_physical_type == ASR::array_physical_typeType::FixedSizeArray ) {
            for( size_t idim = 0; idim < x.n_args; idim++ ) {
                this->visit_expr(*m_dims[idim].m_start);
                diminfo.push_back(src);
                this->visit_expr(*m_dims[idim].m_length);
                diminfo.push_back(src);
            }
        } else if( array_t->m_physical_type == ASR::array_physical_typeType::UnboundedPointerToDataArray ) {
            for( size_t idim = 0; idim < x.n_args; idim++ ) {
                this->visit_expr(*m_dims[idim].m_start);
                diminfo.push_back(src);
            }
        }

        LCOMPILERS_ASSERT(ASRUtils::extract_n_dims_from_ttype(x_mv_type) > 0);
        if (array_t->m_physical_type == ASR::array_physical_typeType::UnboundedPointerToDataArray) {
            src = arr_get_single_element(array, indices, x.n_args,
                                                true,
                                                false,
                                                diminfo,
                                                true);
        } else {
            src = arr_get_single_element(array, indices, x.n_args,
                                                array_t->m_physical_type == ASR::array_physical_typeType::PointerToDataArray,
                                                array_t->m_physical_type == ASR::array_physical_typeType::FixedSizeArray,
                                                diminfo, false);
        }
        last_expr_precedence = 2;
    }

    void visit_StringItem(const ASR::StringItem_t& x) {
        CHECK_FAST_C(compiler_options, x)
        this->visit_expr(*x.m_idx);
        std::string idx = std::move(src);
        this->visit_expr(*x.m_arg);
        std::string str = std::move(src);
        src = "_lfortran_str_item(" + str + ", " + idx + ")";
    }

    std::string generate_map_clauses(ASR::OMPMap_t* m) {
        std::string result = " map(";
        
        std::string map_type;
        if (m->m_type == ASR::map_typeType::To) {
            map_type = "to";
        } else if (m->m_type == ASR::map_typeType::From) {
            map_type = "from";
        } else if (m->m_type == ASR::map_typeType::ToFrom) {
            map_type = "tofrom";
        } else if (m->m_type == ASR::map_typeType::Alloc) {
            map_type = "alloc";
        } else if (m->m_type == ASR::map_typeType::Release) {
            map_type = "release";
        } else if (m->m_type == ASR::map_typeType::Delete) {
            map_type = "delete";
        }
        
        std::vector<std::string> all_mappings;
        
        for (size_t j = 0; j < m->n_vars; j++) {
            if (m->m_vars[j]->type == ASR::exprType::Var) {
                ASR::Variable_t* var = ASRUtils::EXPR2VAR(m->m_vars[j]);
                std::string var_name;
                visit_expr(*m->m_vars[j]);
                var_name = src;
                
                if (is_allocatable_array(var)) {
                    std::vector<std::string> struct_mappings = generate_struct_mappings(var_name, var, map_type);
                    all_mappings.insert(all_mappings.end(), struct_mappings.begin(), struct_mappings.end());
                } else if (ASRUtils::is_array(var->m_type)) {
                    all_mappings.push_back(generate_array_mapping(var_name, var, map_type));
                } else {
                    all_mappings.push_back(map_type + ": " + var_name);
                }
            } else {
                throw CodeGenError("Unsupported OpenMP map variable type: " +
                    std::to_string((int)m->m_vars[j]->type));
            }
        }
        
        if (all_mappings.size() == 1) {
            result += all_mappings[0] + ")";
        } else {
            result = "";
            for (size_t i = 0; i < all_mappings.size(); i++) {
                result += " map(" + all_mappings[i] + ")";
            }
        }
        
        return result;
    }

    bool is_allocatable_array(ASR::Variable_t* var) {
        return ASR::is_a<ASR::Allocatable_t>(*var->m_type) || 
            (ASRUtils::is_array(var->m_type) && 
                ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable(var->m_type))->m_physical_type == ASR::array_physical_typeType::DescriptorArray);
    }

    std::vector<std::string> generate_struct_mappings(const std::string& var_name, ASR::Variable_t* var, const std::string& base_map_type) {
        std::vector<std::string> mappings;
        
        ASR::ttype_t* type = ASRUtils::type_get_past_allocatable(var->m_type);
        if (ASRUtils::is_array(type)) {
            ASR::Array_t* arr_type = ASR::down_cast<ASR::Array_t>(type);
            
            // 1. Map the data array with proper slice notation
            std::string data_mapping;
            if (base_map_type == "to") {
                data_mapping = "to: " + var_name + "->data[0:";
            } else if (base_map_type == "from") {
                data_mapping = "from: " + var_name + "->data[0:";
            } else {
                data_mapping = "tofrom: " + var_name + "->data[0:";
            }
            
            if (arr_type->n_dims == 1) {
                // For 1D arrays, we need to determine the size at runtime
                data_mapping += var_name + "->dims[0].length-1]";
            } else {
                // Multi-dimensional arrays need more complex slice calculation
                std::string total_size = var_name + "->dims[0].length";
                for (size_t i = 1; i < arr_type->n_dims; i++) {
                    total_size += "*" + var_name + "->dims[" + std::to_string(i) + "].length";
                }
                data_mapping += total_size + "-1]";
            }
            mappings.push_back(data_mapping);
            
            // 2. Map dimension descriptors (usually 'to' since they're metadata)
            for (size_t i = 0; i < arr_type->n_dims; i++) {
                std::string dim_mapping = "to: " + var_name + "->dims[" + std::to_string(i) + "].lower_bound, " +
                                        var_name + "->dims[" + std::to_string(i) + "].length, " +
                                        var_name + "->dims[" + std::to_string(i) + "].stride";
                mappings.push_back(dim_mapping);
            }
            
            // 3. Map other struct members as needed
            mappings.push_back("to: " + var_name + "->n_dims");
            
            // Map offset - use 'from' if base is 'from' or 'tofrom', otherwise 'to'
            if (base_map_type == "from" || base_map_type == "tofrom") {
                mappings.push_back("from: " + var_name + "->offset");
            } else {
                mappings.push_back("to: " + var_name + "->offset");
            }
        }
        
        return mappings;
    }

    std::string generate_array_mapping(const std::string& var_name, ASR::Variable_t* var, const std::string& map_type) {
        ASR::Array_t* arr_type = ASR::down_cast<ASR::Array_t>(var->m_type);
        
        std::string mapping = map_type + ": " + var_name+ "->data";
        
        if (arr_type->n_dims == 1 && arr_type->m_dims[0].m_start && arr_type->m_dims[0].m_length) {
            mapping += "[";
            visit_expr(*arr_type->m_dims[0].m_start);
            std::string start = src;
            visit_expr(*arr_type->m_dims[0].m_length);
            std::string length = src;
            mapping += start + ":" + length + "]";
        }
        
        return mapping;
    }
    void visit_StringLen(const ASR::StringLen_t &x) {
        CHECK_FAST_C(compiler_options, x)
        this->visit_expr(*x.m_arg);
        src = "strlen(" + src + ")";
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (!target_offload_enabled) {
            // Codegen for --show-c: Generate OpenMP pragmas
            std::string opening_pragma;
            if (x.m_region == ASR::omp_region_typeType::Parallel) {
                opening_pragma = "#pragma omp parallel ";
            } else if (x.m_region == ASR::omp_region_typeType::Do) {
                opening_pragma = "#pragma omp for ";
            } else if (x.m_region == ASR::omp_region_typeType::Sections) {
                opening_pragma = "#pragma omp sections ";
            } else if (x.m_region == ASR::omp_region_typeType::Single) {
                opening_pragma = "#pragma omp single ";
            } else if (x.m_region == ASR::omp_region_typeType::Critical) {
                opening_pragma = "#pragma omp critical ";
            } else if (x.m_region == ASR::omp_region_typeType::Atomic) {
                opening_pragma = "#pragma omp atomic ";
            } else if (x.m_region == ASR::omp_region_typeType::Barrier) {
                opening_pragma = "#pragma omp barrier ";
            } else if (x.m_region == ASR::omp_region_typeType::Task) {
                opening_pragma = "#pragma omp task ";
            } else if (x.m_region == ASR::omp_region_typeType::Taskwait) {
                opening_pragma = "#pragma omp taskwait ";
            } else if (x.m_region == ASR::omp_region_typeType::Master) {
                opening_pragma = "#pragma omp master ";
            } else if (x.m_region == ASR::omp_region_typeType::ParallelDo) {
                opening_pragma = "#pragma omp parallel for ";
            } else if (x.m_region == ASR::omp_region_typeType::ParallelSections) {
                opening_pragma = "#pragma omp parallel sections ";
            } else if (x.m_region == ASR::omp_region_typeType::Taskloop) {
                opening_pragma = "#pragma omp taskloop ";
            } else if (x.m_region == ASR::omp_region_typeType::Target) {
                opening_pragma = "#pragma omp target ";
            } else if (x.m_region == ASR::omp_region_typeType::Teams) {
                opening_pragma = "#pragma omp teams ";
            } else if (x.m_region == ASR::omp_region_typeType::DistributeParallelDo) {
                opening_pragma = "#pragma omp distribute parallel for ";
            } else if (x.m_region == ASR::omp_region_typeType::Distribute) {
                opening_pragma = "#pragma omp distribute ";
            } else {
                throw CodeGenError("Unsupported OpenMP region type: " + std::to_string((int)x.m_region));
            }

            std::string clauses;
            for(size_t i=0;i<x.n_clauses;i++) {
                ASR::omp_clause_t* clause = x.m_clauses[i];
                if (ASR::is_a<ASR::OMPPrivate_t>(*clause)) {
                    ASR::OMPPrivate_t* c = ASR::down_cast<ASR::OMPPrivate_t>(clause);
                    clauses += " private(";
                    std::string vars;
                    for (size_t j=0; j<c->n_vars; j++) {
                        visit_expr(*c->m_vars[j]);
                        vars += src;
                        if (j < c->n_vars - 1) {
                            vars += ", ";
                        }
                    }
                    clauses += vars + ")";
                } else if (ASR::is_a<ASR::OMPShared_t>(*clause)) {
                    ASR::OMPShared_t* c = ASR::down_cast<ASR::OMPShared_t>(clause);
                    clauses += " shared(";
                    std::string vars;
                    for (size_t j=0; j<c->n_vars; j++) {
                        visit_expr(*c->m_vars[j]);
                        vars += src;
                        if (j < c->n_vars - 1) {
                            vars += ", ";
                        }
                    }
                    clauses += vars + ")";
                } else if (ASR::is_a<ASR::OMPNumTeams_t>(*clause)) {
                    ASR::OMPNumTeams_t* c = ASR::down_cast<ASR::OMPNumTeams_t>(clause);
                    clauses += " num_teams(";
                    visit_expr(*c->m_num_teams);
                    clauses += src + ")";
                } else if (ASR::is_a<ASR::OMPThreadLimit_t>(*clause)) {
                    ASR::OMPThreadLimit_t* c = ASR::down_cast<ASR::OMPThreadLimit_t>(clause);
                    clauses += " thread_limit(";
                    visit_expr(*c->m_thread_limit);
                    clauses += src + ")";
                } else if (ASR::is_a<ASR::OMPSchedule_t>(*clause)) {
                    ASR::OMPSchedule_t* c = ASR::down_cast<ASR::OMPSchedule_t>(clause);
                    clauses += " schedule(";
                    if (c->m_kind == ASR::schedule_typeType::Static) {
                        clauses += "static";
                    } else if (c->m_kind == ASR::schedule_typeType::Dynamic) {
                        clauses += "dynamic";
                    } else if (c->m_kind == ASR::schedule_typeType::Guided) {
                        clauses += "guided";
                    } else if (c->m_kind == ASR::schedule_typeType::Auto) {
                        clauses += "auto";
                    } else if (c->m_kind == ASR::schedule_typeType::Runtime) {
                        clauses += "runtime";
                    }
                    if (c->m_chunk_size) {
                        clauses += ", ";
                        visit_expr(*c->m_chunk_size);
                        clauses += src;
                    }
                    clauses += ")";
                } else if (ASR::is_a<ASR::OMPReduction_t>(*clause)) {
                    ASR::OMPReduction_t* c = ASR::down_cast<ASR::OMPReduction_t>(clause);
                    clauses += " reduction(";
                    std::string op;
                    if(c->m_operator == ASR::reduction_opType::ReduceAdd) {
                        op += "+";
                    } else if(c->m_operator == ASR::reduction_opType::ReduceMul) {
                        op += "*";
                    } else if(c->m_operator == ASR::reduction_opType::ReduceSub) {
                        op += "-";
                    } else if(c->m_operator == ASR::reduction_opType::ReduceMAX) {
                        op += "max";
                    } else if(c->m_operator == ASR::reduction_opType::ReduceMIN) {
                        op += "min";
                    } else {
                        throw CodeGenError("Unsupported OpenMP reduction operator: " +
                            std::to_string((int)c->m_operator));
                    }
                    clauses += op + ": ";
                    std::string vars;
                    for (size_t j=0; j<c->n_vars; j++) {
                        visit_expr(*c->m_vars[j]);
                        vars += src;
                        if (j < c->n_vars - 1) {
                            vars += ", ";
                        }
                    }
                    clauses += vars + ")";
                } else if (ASR::is_a<ASR::OMPDevice_t>(*clause)) {
                    ASR::OMPDevice_t* c = ASR::down_cast<ASR::OMPDevice_t>(clause);
                    clauses += " device(";
                    visit_expr(*c->m_device);
                    clauses += src + ")";
                } else if (ASR::is_a<ASR::OMPMap_t>(*clause)) {
                    ASR::OMPMap_t* m = ASR::down_cast<ASR::OMPMap_t>(clause);
                    std::string map_clauses = generate_map_clauses(m);
                    clauses += map_clauses;
                }
            }
            
            opening_pragma += clauses;
            
            if (x.m_region == ASR::omp_region_typeType::Barrier || 
                x.m_region == ASR::omp_region_typeType::Taskwait) {
                // These are standalone directives
                src = opening_pragma + "\n";
                return;
            }
            

            std::string body;
            for(size_t i=0;i<x.n_body;i++) {
                this->visit_stmt(*x.m_body[i]);
                body += src;
            }
            if(x.m_region != ASR::omp_region_typeType::Target && x.m_region != ASR::omp_region_typeType::Teams &&
               x.m_region != ASR::omp_region_typeType::DistributeParallelDo &&
               x.m_region != ASR::omp_region_typeType::Distribute) {
                src = opening_pragma + "{\n" + body + "}\n";
            } else {
                src =  opening_pragma + "\n" + body + "\n";
            }
        } else {
            // CodeGen for --target-offload: Generate CUDA code
            if (x.m_region == ASR::omp_region_typeType::Target) {
                map_vars.clear();
                std::string target_code;
                std::string kernel_decls;
                std::string kernel_wrappers;
                std::string struct_copies;

                // Collect map clauses
                for (size_t i = 0; i < x.n_clauses; i++) {
                    if (ASR::is_a<ASR::OMPMap_t>(*x.m_clauses[i])) {
                        ASR::OMPMap_t* m = ASR::down_cast<ASR::OMPMap_t>(x.m_clauses[i]);
                        for (size_t j = 0; j < m->n_vars; j++) {
                            visit_expr(*m->m_vars[j]);
                            map_vars.push_back({src, m});
                        }
                    }
                }

                // Declare device pointers for data
                for (const auto &mv : map_vars) {
                    target_code += indent() + "float *d_" + mv.first + "_data = NULL;\n";
                }
                target_code += indent() + "cudaError_t err;\n";
                // Allocate device memory for data
                for (const auto &mv : map_vars) {
                    target_code += indent() + "size_t " + mv.first + "_data_size = " + mv.first + "->dims[0].length * sizeof(float);\n";
                    target_code += indent() + "err = cudaMalloc((void**)&d_" + mv.first + "_data, " + mv.first + "_data_size);\n";
                    target_code += indent() + "if (err != cudaSuccess) {\n";
                    target_code += indent() + "    fprintf(stderr, \"cudaMalloc failed for " + mv.first + "_data: %s\\n\", cudaGetErrorString(err));\n";
                    target_code += indent() + "    exit(1);\n";
                    target_code += indent() + "}\n";
                }

                // Copy data to device (based on map type)
                for (const auto &mv : map_vars) {
                    if (mv.second->m_type == ASR::map_typeType::To || mv.second->m_type == ASR::map_typeType::ToFrom) {
                        target_code += indent() + "err = cudaMemcpy(d_" + mv.first + "_data, " + mv.first + "->data, " + mv.first + "_data_size, cudaMemcpyHostToDevice);\n";
                        target_code += indent() + "if (err != cudaSuccess) {\n";
                        target_code += indent() + "    fprintf(stderr, \"cudaMemcpy H2D failed for " + mv.first + "_data: %s\\n\", cudaGetErrorString(err));\n";
                        target_code += indent() + "    exit(1);\n";
                        target_code += indent() + "}\n";
                    }
                }

                // Create host struct copies with device data pointers
                for (const auto &mv : map_vars) {
                    target_code += indent() + "struct r32 h_" + mv.first + "_copy = *" + mv.first + ";\n";
                    target_code += indent() + "h_" + mv.first + "_copy.data = d_" + mv.first + "_data;\n";
                }

                // Declare and allocate device struct pointers
                for (const auto &mv : map_vars) {
                    target_code += indent() + "struct r32 *d_" + mv.first + "_struct = NULL;\n";
                    target_code += indent() + "err = cudaMalloc((void**)&d_" + mv.first + "_struct, sizeof(struct r32));\n";
                    target_code += indent() + "if (err != cudaSuccess) {\n";
                    target_code += indent() + "    fprintf(stderr, \"cudaMalloc failed for d_" + mv.first + "_struct: %s\\n\", cudaGetErrorString(err));\n";
                    target_code += indent() + "    exit(1);\n";
                    target_code += indent() + "}\n";
                }

                // Copy host struct copies to device
                for (const auto &mv : map_vars) {
                    target_code += indent() + "err = cudaMemcpy(d_" + mv.first + "_struct, &h_" + mv.first + "_copy, sizeof(struct r32), cudaMemcpyHostToDevice);\n";
                    target_code += indent() + "if (err != cudaSuccess) {\n";
                    target_code += indent() + "    fprintf(stderr, \"cudaMemcpy H2D failed for d_" + mv.first + "_struct: %s\\n\", cudaGetErrorString(err));\n";
                    target_code += indent() + "    exit(1);\n";
                    target_code += indent() + "}\n";
                }

                // Process nested constructs (teams, distribute parallel do)
                for (size_t i = 0; i < x.n_body; i++) {
                    this->visit_stmt(*x.m_body[i]);
                    target_code += src;
                }

                // Copy back from device (data only, structs are metadata)
                for (const auto &mv : map_vars) {
                    if (mv.second->m_type == ASR::map_typeType::From || mv.second->m_type == ASR::map_typeType::ToFrom) {
                        target_code += indent() + "err = cudaMemcpy(" + mv.first + "->data, d_" + mv.first + "_data, " + mv.first + "_data_size, cudaMemcpyDeviceToHost);\n";
                        target_code += indent() + "if (err != cudaSuccess) {\n";
                        target_code += indent() + "    fprintf(stderr, \"cudaMemcpy D2H failed for " + mv.first + "_data: %s\\n\", cudaGetErrorString(err));\n";
                        target_code += indent() + "    exit(1);\n";
                        target_code += indent() + "}\n";
                    }
                }

                // Free device memory (data and structs)
                for (const auto &mv : map_vars) {
                    target_code += indent() + "cudaFree(d_" + mv.first + "_data);\n";
                    target_code += indent() + "cudaFree(d_" + mv.first + "_struct);\n";
                }

                src = kernel_decls + "\n" + target_code + "\n" + kernel_wrappers;
            } else if (x.m_region == ASR::omp_region_typeType::Teams) {
                // Teams: Set up grid dimensions (handled in distribute parallel do)
                std::string teams_code;
                for (size_t i = 0; i < x.n_body; i++) {
                    this->visit_stmt(*x.m_body[i]);
                    teams_code += src;
                }
                src = teams_code;
            } else if (x.m_region == ASR::omp_region_typeType::DistributeParallelDo ||
                       x.m_region == ASR::omp_region_typeType::Distribute ||
                       x.m_region == ASR::omp_region_typeType::ParallelDo) {
                // Parallel Do: Generate kernel and launch
                if (x.n_body != 1 || !ASR::is_a<ASR::DoLoop_t>(*x.m_body[0])) {
                    throw CodeGenError("Distribute Parallel Do must contain a single DoLoop");
                }

                ASR::DoLoop_t* loop = ASR::down_cast<ASR::DoLoop_t>(x.m_body[0]);
                std::string kernel_name = "compute_kernel_" + std::to_string(kernel_counter++);
                current_kernel_name = kernel_name;
                kernel_func_names.push_back(kernel_name);

                // Extract loop head
                std::string idx_var;
                visit_expr(*loop->m_head.m_v);
                idx_var = src;
                std::string n;
                visit_expr(*loop->m_head.m_end);
                n = src;

                // Generate kernel (pass struct pointers)
                std::string kernel_code = "#ifdef USE_GPU\n__global__\n#endif\n";
                kernel_code += "void " + kernel_name + "(";
                for (const auto &mv : map_vars) {
                    kernel_code += "struct r32 *" + mv.first + ", ";
                }
                kernel_code += "int " + idx_var + "_n) {\n";
                kernel_code += "    int " + idx_var + " = blockIdx.x * blockDim.x + threadIdx.x + 1;\n";
                kernel_code += "    if (" + idx_var + " <= " + idx_var + "_n) {\n";
                for (size_t i = 0; i < loop->n_body; i++) {
                    visit_stmt(*loop->m_body[i]);
                    kernel_code += "        " + src;
                }
                kernel_code += "    }\n}\n";

                // Generate CPU wrapper
                std::string wrapper_code = "#ifndef USE_GPU\n";
                wrapper_code += "void " + kernel_name + "_wrapper(void **args) {\n";
                int arg_idx = 0;
                for (const auto &mv : map_vars) {
                    wrapper_code += "    struct r32 *" + mv.first + " = *(struct r32**)args[" + std::to_string(arg_idx++) + "];\n";
                }
                wrapper_code += "    int " + idx_var + "_n = *(int*)args[" + std::to_string(arg_idx++) + "];\n";
                wrapper_code += "    " + kernel_name + "(";
                for (const auto &mv : map_vars) {
                    wrapper_code += mv.first + ", ";
                }
                wrapper_code += idx_var + "_n);\n";
                wrapper_code += "}\n#endif\n";

                // Generate kernel launch
                std::string launch_code = indent() + "int " + idx_var + "_n = " + n + ";\n";
                launch_code += indent() + "int threads_per_block = 256;\n";
                launch_code += indent() + "int blocks = (" + idx_var + "_n + threads_per_block - 1) / threads_per_block;\n";
                launch_code += indent() + "dim3 grid_dim = {blocks, 1, 1};\n";
                launch_code += indent() + "dim3 block_dim = {threads_per_block, 1, 1};\n";
                launch_code += indent() + "void *kernel_args[] = {";
                for (const auto &mv : map_vars) {
                    launch_code += "&d_" + mv.first + "_struct, ";
                }
                launch_code += "&" + idx_var + "_n};\n";
                launch_code += indent() + "err = cudaLaunchKernel((void*)" + kernel_name + ", grid_dim, block_dim, kernel_args, 0, NULL);\n";
                launch_code += indent() + "if (err != cudaSuccess) {\n";
                launch_code += indent() + "    fprintf(stderr, \"cudaLaunchKernel failed: %s\\n\", cudaGetErrorString(err));\n";
                launch_code += indent() + "    exit(1);\n";
                launch_code += indent() + "}\n";
                launch_code += indent() + "err = cudaDeviceSynchronize();\n";
                launch_code += indent() + "if (err != cudaSuccess) {\n";
                launch_code += indent() + "    fprintf(stderr, \"cudaDeviceSynchronize failed: %s\\n\", cudaGetErrorString(err));\n";
                launch_code += indent() + "    exit(1);\n";
                launch_code += indent() + "}\n";

                kernel_func_code = kernel_code + "\n" + wrapper_code + "\n" ;
                src = launch_code;
            } else {
                std::string body;
                for(size_t i=0;i<x.n_body;i++) {
                    this->visit_stmt(*x.m_body[i]);
                    body += src;
                }
                src = body;
            }
        }
    }

};

Result<std::string> asr_to_c(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co,
    int64_t default_lower_bound)
{
    ASRToCVisitor v(diagnostics, co, default_lower_bound);
    try {
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
