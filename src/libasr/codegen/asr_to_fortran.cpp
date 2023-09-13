#include <libasr/asr.h>
#include <libasr/codegen/asr_to_fortran.h>

using LCompilers::ASR::is_a;
using LCompilers::ASR::down_cast;

namespace LCompilers {

namespace {

    std::string binop2str(const ASR::binopType type)
    {
        switch (type) {
            case (ASR::binopType::Add) : return " + ";
            case (ASR::binopType::Sub) : return " - ";
            case (ASR::binopType::Mul) : return " * ";
            case (ASR::binopType::Div) : return " / ";
            case (ASR::binopType::Pow) : return " ** ";
            default : throw LCompilersException("Binop type not implemented");
        }
    }
}

class ASRToFortranVisitor : public ASR::BaseVisitor<ASRToFortranVisitor>
{
public:
    std::string s;
    bool use_colors;
    int indent_level;
    std::string indent;
    int indent_spaces;

public:
    ASRToFortranVisitor(bool _use_colors, int _indent)
        : use_colors{_use_colors}, indent_level{0},
            indent_spaces{_indent}
        { }

    /*********************************** Utils *********************************/
    void inc_indent() {
        indent_level++;
        indent = std::string(indent_level*indent_spaces, ' ');
    }

    void dec_indent() {
        indent_level--;
        indent = std::string(indent_level*indent_spaces, ' ');
    }

    /*********************************** Unit **********************************/
    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        std::string r = "";
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Module_t>(*item.second)) {
                visit_symbol(*item.second);
            r += s;
            }
        }
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
            r += s;
            }
        }
        s = r;
    }

    /********************************** Symbol *********************************/
    void visit_Program(const ASR::Program_t &x) {
        std::string r;
        r = "program";
        r += " ";
        r.append(x.m_name);
        r += "\n";
        inc_indent();
        r += indent + "implicit none"; // TODO: Handle implicit
        r += "\n\n";
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                visit_symbol(*item.second);
                r += s;
            }
        }

        for (size_t i = 0; i < x.n_body; i++) {
            if (i == 0) r += "\n";
            visit_stmt(*x.m_body[i]);
            r += s;
        }

        bool prepend_contains_keyword = true;
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                if (prepend_contains_keyword) {
                    prepend_contains_keyword = false;
                    r += "\n";
                    r += "contains";
                    r += "\n\n";
                }
                visit_symbol(*item.second);
                r += s;
            }
        }
        dec_indent();
        r += "end program";
        r += " ";
        r.append(x.m_name);
        r += "\n";
        s = r;
    }

    void visit_Module(const ASR::Module_t &x) {
        std::string r;
        r = "module";
        r += " ";
        r.append(x.m_name);
        r += "\n";

        r += "end module";
        r += " ";
        r.append(x.m_name);
        r += "\n";
        s = r;
    }

    void visit_Function(const ASR::Function_t &x) {
        std::string r = indent;
        r += "function";
        r += " ";
        r.append(x.m_name);
        r += "\n";

        inc_indent();
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                visit_symbol(*item.second);
                r += s;
            }
        }

        for (size_t i = 0; i < x.n_body; i++) {
            if (i == 0) r += "\n";
            visit_stmt(*x.m_body[i]);
            r += s;
        }
        dec_indent();
        r += indent;
        r += "end function";
        r += " ";
        r.append(x.m_name);
        r += "\n";
        s = r;
    }

    void visit_Variable(const ASR::Variable_t &x) {
        std::string r = indent;
        std::string dims = "(";
        switch (x.m_type->type) {
            case ASR::ttypeType::Integer: {
                ASR::Integer_t *i = down_cast<ASR::Integer_t>(x.m_type);
                r += "integer(";
                r += std::to_string(i->m_kind);
                r += ")";
                break;
            }
            default:
                throw LCompilersException("Type not implemented");
        }
        switch (x.m_intent) {
            case ASR::intentType::In : {
                r += ", intent(in)";
                break;
            } case ASR::intentType::InOut : {
                r += ", intent(inout)";
                break;
            } case ASR::intentType::Out : {
                r += ", intent(out)";
                break;
            } case ASR::intentType::Local : {
                // Pass
                break;
            } case ASR::intentType::ReturnVar : {
                r += ", intent(out)";
                break;
            }
            default:
                throw LCompilersException("Intent type is not handled");
        }
        r += " :: ";
        r.append(x.m_name);
        r += "\n";
        s = r;
    }

    /*********************************** Stmt **********************************/
    void visit_Assignment(const ASR::Assignment_t &x) {
        std::string r = indent;
        visit_expr(*x.m_target);
        r += s;
        r += " = ";
        visit_expr(*x.m_value);
        r += s;
        r += "\n";
        s = r;
    }

    void visit_Print(const ASR::Print_t &x) {
        std::string r = indent;
        r += "print";
        r += " ";
        if (!x.m_fmt) {
            r += "*, ";
        }
        for (size_t i = 0; i < x.n_values; i++) {
            visit_expr(*x.m_values[i]);
            r += s;
            if (i < x.n_values-1) r += ", ";
        }
        r += "\n";
        s = r;
    }

    /*********************************** Expr **********************************/
    void visit_Var(const ASR::Var_t &x) {
        s = ASRUtils::symbol_name(x.m_v);
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        std::string r;
        visit_expr(*x.m_left);
        r = s;
        r += binop2str(x.m_op);
        visit_expr(*x.m_right);
        r += s;
        s = r;
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        s = std::to_string(x.m_n);
    }
};

std::string asr_to_fortran(ASR::TranslationUnit_t &asr,
        bool color, int indent) {
    ASRToFortranVisitor v(color, indent);
    v.visit_TranslationUnit(asr);
    return v.s;
}

} // namespace LCompilers
