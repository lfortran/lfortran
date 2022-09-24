#include "libasr/asr.h"
#include "libasr/diagnostics.h"
#include <libasr/codegen/asr_to_julia.h>

namespace LFortran
{
class ASRToJuliaVisitor : public ASR::BaseVisitor<ASRToJuliaVisitor>
{
public:
    diag::Diagnostics& diag;
    std::string src;
    int indentation_level;
    int indentation_spaces;
    int last_expr_precedence;
    bool intrinsic_module = false;
    const ASR::Function_t* current_function = nullptr;

    SymbolTable* global_scope;
    std::map<uint64_t, SymbolInfo> sym_info;
    std::string from_std_vector_helper;

    ASRToJuliaVisitor(diag::Diagnostics& diag)
        : diag{ diag }
    {
    }

    std::string convert_variable_decl(const ASR::Variable_t& v)
    {
        return v.m_name;
    }

    // Returns the declaration, no semi colon at the end
    std::string get_function_declaration(const ASR::Function_t& x)
    {
        std::string sub, inl, ret_type;
        if (x.m_inline) {
            inl = "@inline ";
        }
        if (x.m_return_var) {
            ASR::Variable_t* return_var = LFortran::ASRUtils::EXPR2VAR(x.m_return_var);
            if (ASRUtils::is_integer(*return_var->m_type)) {
                int kind = ASR::down_cast<ASR::Integer_t>(return_var->m_type)->m_kind;
                switch (kind) {
                    case (1):
                        ret_type = "Int8";
                        break;
                    case (2):
                        ret_type = "Int16";
                        break;
                    case (4):
                        ret_type = "Int32";
                        break;
                    case (8):
                        ret_type = "Int64";
                        break;
                }
            } else if (ASRUtils::is_real(*return_var->m_type)) {
                bool is_float = ASR::down_cast<ASR::Real_t>(return_var->m_type)->m_kind == 4;
                if (is_float) {
                    ret_type = "Float32";
                } else {
                    ret_type = "Float64";
                }
            } else if (ASRUtils::is_logical(*return_var->m_type)) {
                ret_type = "Bool";
            } else if (ASRUtils::is_character(*return_var->m_type)) {
                ret_type = "String";
            } else if (ASRUtils::is_complex(*return_var->m_type)) {
                bool is_float = ASR::down_cast<ASR::Complex_t>(return_var->m_type)->m_kind == 4;
                if (is_float) {
                    ret_type = "ComplexF32";
                } else {
                    ret_type = "ComplexF64";
                }
            } else if (ASR::is_a<ASR::CPtr_t>(*return_var->m_type)) {
                ret_type = "Ptr{Cvoid}";
            } else {
                throw CodeGenError("Return type not supported in function '" + std::string(x.m_name)
                                       + +"'",
                                   return_var->base.base.loc);
            }
        }
        std::string sym_name = x.m_name;
        if (sym_name == "main") {
            sym_name = "_xx_lcompilers_changed_main_xx";
        }
        if (sym_name == "exit") {
            sym_name = "_xx_lcompilers_changed_exit_xx";
        }
        std::string func = inl + "function " + sym_name + "(";
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Variable_t* arg = LFortran::ASRUtils::EXPR2VAR(x.m_args[i]);
            LFORTRAN_ASSERT(LFortran::ASRUtils::is_arg_dummy(arg->m_intent));
            func += this->convert_variable_decl(*arg);
            if (i < x.n_args - 1)
                func += ", ";
        }
        func += ")";
        if (!ret_type.empty())
            func += "::" + ret_type;

        return func;
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t& x)
    {
        global_scope = x.m_global_scope;

        // All loose statements must be converted to a function, so the items
        // must be empty:
        LFORTRAN_ASSERT(x.n_items == 0);
        std::string unit_src = "";
        indentation_level = 0;
        indentation_spaces = 4;

        std::string headers = R"()";
        unit_src += headers;


        {
            // Process intrinsic modules in the right order
            std::vector<std::string> build_order
                = LFortran::ASRUtils::determine_module_dependencies(x);
            for (auto& item : build_order) {
                LFORTRAN_ASSERT(x.m_global_scope->get_scope().find(item)
                                != x.m_global_scope->get_scope().end());
                if (startswith(item, "lfortran_intrinsic")) {
                    ASR::symbol_t* mod = x.m_global_scope->get_symbol(item);
                    this->visit_symbol(*mod);
                    unit_src += src;
                }
            }
        }

        // Process procedures first:
        for (auto& item : x.m_global_scope->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                this->visit_symbol(*item.second);
                unit_src += src;
            }
        }

        // Then do all the modules in the right order
        std::vector<std::string> build_order = LFortran::ASRUtils::determine_module_dependencies(x);
        for (auto& item : build_order) {
            LFORTRAN_ASSERT(x.m_global_scope->get_scope().find(item)
                            != x.m_global_scope->get_scope().end());
            if (!startswith(item, "lfortran_intrinsic")) {
                ASR::symbol_t* mod = x.m_global_scope->get_symbol(item);
                this->visit_symbol(*mod);
                unit_src += src;
            }
        }

        // Then the main program:
        for (auto& item : x.m_global_scope->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*item.second)) {
                this->visit_symbol(*item.second);
                unit_src += src;
            }
        }

        src = unit_src;
    }

    void visit_Module(const ASR::Module_t& x)
    {
        if (startswith(x.m_name, "lfortran_intrinsic_")) {
            intrinsic_module = true;
        } else {
            intrinsic_module = false;
        }

        std::string contains;

        // Generate the bodies of subroutines
        for (auto& item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t* s = ASR::down_cast<ASR::Function_t>(item.second);
                this->visit_Function(*s);
                contains += src;
            }
        }
        src = contains;
        intrinsic_module = false;
    }

    void visit_Program(const ASR::Program_t& x)
    {
        // Generate code for nested subroutines and functions first:
        std::string contains;
        for (auto& item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t* s = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*s);
                contains += src;
            }
        }

        // Generate code for the main program
        indentation_level += 1;
        std::string indent1(indentation_level * indentation_spaces, ' ');
        indentation_level += 1;
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string decl;
        for (auto& item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
                decl += this->convert_variable_decl(*v) + "\n";
            }
        }

        std::string body;
        for (size_t i = 0; i < x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            body += src;
        }

        src = contains + "function main()\n{\n" + decl + body + "end\n" + "main()\n";
        indentation_level -= 2;
    }

    void visit_Function(const ASR::Function_t& x)
    {
        if (std::string(x.m_name) == "size" && intrinsic_module) {
            // Intrinsic function `size`
            SymbolInfo s;
            s.intrinsic_function = true;
            sym_info[get_hash((ASR::asr_t*) &x)] = s;
            src.clear();
            return;
        } else if ((std::string(x.m_name) == "int" || std::string(x.m_name) == "char"
                    || std::string(x.m_name) == "present" || std::string(x.m_name) == "len"
                    || std::string(x.m_name) == "not")
                   && intrinsic_module) {
            // Intrinsic function `int`
            SymbolInfo s;
            s.intrinsic_function = true;
            sym_info[get_hash((ASR::asr_t*) &x)] = s;
            src.clear();
            return;
        } else {
            SymbolInfo s;
            s.intrinsic_function = false;
            sym_info[get_hash((ASR::asr_t*) &x)] = s;
        }
        std::string sub = get_function_declaration(x);
        if (x.m_abi == ASR::abiType::BindC && x.m_deftype == ASR::deftypeType::Interface) {
        } else {
            indentation_level += 1;
            std::string indent(indentation_level * indentation_spaces, ' ');
            std::string decl;
            for (auto& item : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
                    if (v->m_intent == LFortran::ASRUtils::intent_local
                        || v->m_intent == LFortran::ASRUtils::intent_return_var) {
                        decl += indent + "local " + this->convert_variable_decl(*v) + "\n";
                    }
                }
            }

            current_function = &x;
            std::string body;

            for (size_t i = 0; i < x.n_body; i++) {
                this->visit_stmt(*x.m_body[i]);
                body += src;
            }

            current_function = nullptr;
            bool visited_return = false;

            if (x.n_body > 0 && ASR::is_a<ASR::Return_t>(*x.m_body[x.n_body - 1])) {
                visited_return = true;
            }

            if (!visited_return && x.m_return_var) {
                body += indent + "return " + LFortran::ASRUtils::EXPR2VAR(x.m_return_var)->m_name
                        + "\n";
            }

            if (decl.size() > 0 || body.size() > 0) {
                sub += "\n" + decl + body + "end\n";
            } else {
                sub += "end\n";
            }
            indentation_level -= 1;
        }
        sub += "\n";
        src = sub;
    }

    void visit_Assignment(const ASR::Assignment_t& x)
    {
        std::string target;
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            visit_Var(*ASR::down_cast<ASR::Var_t>(x.m_target));
            target = src;
            if (ASRUtils::is_array(ASRUtils::expr_type(x.m_target))) {
                target += "->data";
            }
        } else if (ASR::is_a<ASR::ArrayItem_t>(*x.m_target)) {
            this->visit_ArrayItem(*ASR::down_cast<ASR::ArrayItem_t>(x.m_target));
            target = src;
        } else if (ASR::is_a<ASR::DerivedRef_t>(*x.m_target)) {
            visit_DerivedRef(*ASR::down_cast<ASR::DerivedRef_t>(x.m_target));
            target = src;
        } else {
            LFORTRAN_ASSERT(false)
        }
        from_std_vector_helper.clear();
        this->visit_expr(*x.m_value);
        std::string value = src;
        std::string indent(indentation_level * indentation_spaces, ' ');
        if (!from_std_vector_helper.empty()) {
            src = from_std_vector_helper;
        } else {
            src.clear();
        }
        src += indent + target + " = " + value + "\n";
        from_std_vector_helper.clear();
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t& x)
    {
        src = std::to_string(x.m_n);
        last_expr_precedence = 2;
    }

    void visit_RealConstant(const ASR::RealConstant_t& x)
    {
        src = double_to_scientific(x.m_r);
        last_expr_precedence = 2;
    }


    void visit_StringConstant(const ASR::StringConstant_t& x)
    {
        src = "\"";
        std::string s = x.m_s;
        for (size_t idx = 0; idx < s.size(); idx++) {
            if (s[idx] == '\n') {
                src += "\\n";
            } else if (s[idx] == '\\') {
                src += "\\\\";
            } else if (s[idx] == '\"') {
                src += "\\\"";
            } else {
                src += s[idx];
            }
        }
        src += "\"";
        last_expr_precedence = 2;
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t& x)
    {
        if (x.m_value == true) {
            src = "true";
        } else {
            src = "false";
        }
        last_expr_precedence = 2;
    }

    void visit_Var(const ASR::Var_t& x)
    {
        const ASR::symbol_t* s = ASRUtils::symbol_get_past_external(x.m_v);
        ASR::Variable_t* sv = ASR::down_cast<ASR::Variable_t>(s);
        if ((sv->m_intent == ASRUtils::intent_in || sv->m_intent == ASRUtils::intent_inout)
            && ASRUtils::is_array(sv->m_type) && ASRUtils::is_pointer(sv->m_type)) {
            src = "(*" + std::string(ASR::down_cast<ASR::Variable_t>(s)->m_name) + ")";
        } else {
            src = std::string(ASR::down_cast<ASR::Variable_t>(s)->m_name);
        }
        last_expr_precedence = 2;
    }

    void visit_Cast(const ASR::Cast_t& x)
    {
        this->visit_expr(*x.m_arg);
        switch (x.m_kind) {
            case (ASR::cast_kindType::IntegerToReal): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                switch (dest_kind) {
                    case 4:
                        src = "Float32(" + src + ")";
                        break;
                    case 8:
                        src = "Float64(" + src + ")";
                        break;
                    default:
                        throw CodeGenError("Cast IntegerToReal: Unsupported Kind "
                                           + std::to_string(dest_kind));
                }
                break;
            }
            case (ASR::cast_kindType::RealToInteger): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src = "Int" + std::to_string(dest_kind * 8) + "(" + src + ")";
                break;
            }
            case (ASR::cast_kindType::RealToReal): {
                // In C++, we do not need to cast float to float explicitly:
                // src = src;
                break;
            }
            case (ASR::cast_kindType::IntegerToInteger): {
                // In C++, we do not need to cast int <-> long long explicitly:
                // src = src;
                break;
            }
            case (ASR::cast_kindType::ComplexToComplex): {
                break;
            }
            case (ASR::cast_kindType::IntegerToComplex): {
                src = "complex(" + src + ")";
                break;
            }
            case (ASR::cast_kindType::ComplexToReal): {
                src = "real(" + src + ")";
                break;
            }
            case (ASR::cast_kindType::RealToComplex): {
                src = "complex(" + src + ")";
                break;
            }
            case (ASR::cast_kindType::LogicalToInteger): {
                src = "Int32(" + src + ")";
                break;
            }
            case (ASR::cast_kindType::IntegerToLogical): {
                src = "Bool(" + src + ")";
                break;
            }
            default:
                throw CodeGenError("Cast kind " + std::to_string(x.m_kind) + " not implemented",
                                   x.base.base.loc);
        }
        last_expr_precedence = 2;
    }
};

Result<std::string>
asr_to_julia(ASR::TranslationUnit_t& asr, diag::Diagnostics& diag)
{
    ASRToJuliaVisitor v(diag);
    try {
        v.visit_asr((ASR::asr_t&) asr);
    } catch (const CodeGenError& e) {
        diag.diagnostics.push_back(e.d);
        return Error();
    } catch (const Abort&) {
        return Error();
    }
    return v.src;
};

}
