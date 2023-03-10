#include <string>

#include <lfortran/pickle.h>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/parser.tab.hh>
#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/string_utils.h>

using LCompilers::LFortran::AST::ast_t;
using LCompilers::LFortran::AST::Declaration_t;
using LCompilers::LFortran::AST::expr_t;
using LCompilers::LFortran::AST::stmt_t;
using LCompilers::LFortran::AST::Name_t;
using LCompilers::LFortran::AST::Num_t;
using LCompilers::LFortran::AST::BinOp_t;
using LCompilers::LFortran::AST::UnaryOp_t;
using LCompilers::LFortran::AST::Compare_t;
using LCompilers::LFortran::AST::If_t;
using LCompilers::LFortran::AST::Assignment_t;
using LCompilers::LFortran::AST::WhileLoop_t;
using LCompilers::LFortran::AST::Exit_t;
using LCompilers::LFortran::AST::Return_t;
using LCompilers::LFortran::AST::Cycle_t;
using LCompilers::LFortran::AST::DoLoop_t;
using LCompilers::LFortran::AST::Subroutine_t;
using LCompilers::LFortran::AST::Function_t;
using LCompilers::LFortran::AST::Program_t;
using LCompilers::LFortran::AST::astType;
using LCompilers::LFortran::AST::exprType;
using LCompilers::LFortran::AST::stmtType;
using LCompilers::LFortran::AST::operatorType;
using LCompilers::LFortran::AST::unaryopType;
using LCompilers::LFortran::AST::cmpopType;
using LCompilers::LFortran::AST::TranslationUnit_t;
using LCompilers::LFortran::AST::PickleBaseVisitor;


namespace LCompilers::LFortran {

std::string pickle(int token, const LFortran::YYSTYPE &yystype,
        bool /* colors */)
{
    std::string t;
    t += "(";
    if (token >= yytokentype::TK_NAME && token <= TK_FALSE) {
        t += "TOKEN";
    } else if (token == yytokentype::TK_NEWLINE) {
        t += "NEWLINE";
        t += ")";
        return t;
    } else if (token == yytokentype::END_OF_FILE) {
        t += "EOF";
        t += ")";
        return t;
    } else {
        t += "KEYWORD";
    }
    t += " \"";
    t += token2text(token);
    t += "\"";
    if (token == yytokentype::TK_LABEL) {
        t += " " + std::to_string(yystype.n) + " ";
    }

    if (token == yytokentype::TK_NAME) {
        t += " " + yystype.string.str();
    } else if (token == yytokentype::TK_INTEGER) {
        t += " " + yystype.int_suffix.int_n.str();
        if (yystype.int_suffix.int_kind.p) {
            t += "_" + yystype.int_suffix.int_kind.str();
        }
    } else if (token == yytokentype::TK_STRING) {
        t = t + " " + "\"" + yystype.string.str() + "\"";
    } else if (token == yytokentype::TK_BOZ_CONSTANT) {
        t += " " + yystype.string.str();
    }
    t += ")";
    return t;
}

std::string op2str(const operatorType type)
{
    switch (type) {
        case (operatorType::Add) : return "+";
        case (operatorType::Sub) : return "-";
        case (operatorType::Mul) : return "*";
        case (operatorType::Div) : return "/";
        case (operatorType::Pow) : return "**";
    }
    throw std::runtime_error("Unknown type");
}

std::string unop2str(const unaryopType type)
{
    switch (type) {
        case (unaryopType::Invert) : return "inv";
        case (unaryopType::Not) : return "not";
        case (unaryopType::UAdd) : return "u+";
        case (unaryopType::USub) : return "u-";
    }
    throw std::runtime_error("Unknown type");
}

std::string compare2str(const cmpopType type)
{
    switch (type) {
        case (cmpopType::Eq) : return "==";
        case (cmpopType::NotEq) : return "/=";
        case (cmpopType::Lt) : return "<";
        case (cmpopType::LtE) : return "<=";
        case (cmpopType::Gt) : return ">";
        case (cmpopType::GtE) : return ">=";
    }
    throw std::runtime_error("Unknown type");
}

/********************** AST Pickle *******************/
class ASTPickleVisitor : public PickleBaseVisitor<ASTPickleVisitor>
{
public:
    void visit_BinOp(const BinOp_t &x) {
        s.append("(");
        // We do not print BinOp +, but rather just +. It is still uniquely
        // determined that + means BinOp's +.
        /*
        s.append(expr2str(x.base.type));
        s.append(" ");
        */
        s.append(op2str(x.m_op));
        s.append(" ");
        this->visit_expr(*x.m_left);
        s.append(" ");
        this->visit_expr(*x.m_right);
        s.append(")");
    }
    void visit_UnaryOp(const UnaryOp_t &x) {
        s.append("(");
        s.append(unop2str(x.m_op));
        s.append(" ");
        this->visit_expr(*x.m_operand);
        s.append(")");
    }
    void visit_Compare(const Compare_t &x) {
        s.append("(");
        s.append(compare2str(x.m_op));
        s.append(" ");
        this->visit_expr(*x.m_left);
        s.append(" ");
        this->visit_expr(*x.m_right);
        s.append(")");
    }
    void visit_Name(const Name_t &x) {
        if (use_colors) {
            s.append(color(fg::yellow));
        }
        s.append(x.m_id);
        if (use_colors) {
            s.append(color(fg::reset));
        }
        for (size_t i=0; i<x.n_member; i++) {
            if (i == 0) s.append(" [");
            this->visit_struct_member(x.m_member[i]);
            if (i < x.n_member-1) s.append(" ");
            if (i == x.n_member-1) s.append("]");
        }
    }
    void visit_Num(const Num_t &x) {
        if (use_colors) {
            s.append(color(fg::cyan));
        }
        s.append(BigInt::int_to_str(x.m_n));
        if (use_colors) {
            s.append(color(fg::reset));
        }
        if (x.m_kind) {
            s += "_";
            s += x.m_kind;
        }
    }
    std::string get_str() {
        return s;
    }
};

std::string pickle(LFortran::AST::ast_t &ast, bool colors, bool indent) {
    ASTPickleVisitor v;
    v.use_colors = colors;
    v.indent = indent;
    v.visit_ast(ast);
    return v.get_str();
}

std::string pickle(AST::TranslationUnit_t &ast, bool colors, bool indent) {
    return pickle((AST::ast_t&)(ast), colors, indent);
}

/********************** ASR Pickle *******************/
class ASRPickleVisitor :
    public ASR::PickleBaseVisitor<ASRPickleVisitor>
{
public:
    bool show_intrinsic_modules;

    std::string get_str() {
        return s;
    }
    void visit_symbol(const ASR::symbol_t &x) {
        s.append(ASRUtils::symbol_parent_symtab(&x)->get_counter());
        s.append(" ");
        if (use_colors) {
            s.append(color(fg::yellow));
        }
        s.append(ASRUtils::symbol_name(&x));
        if (use_colors) {
            s.append(color(fg::reset));
        }
    }
    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        s.append("(");
        if (use_colors) {
            s.append(color(style::bold));
            s.append(color(fg::magenta));
        }
        s.append("IntegerConstant");
        if (use_colors) {
            s.append(color(fg::reset));
            s.append(color(style::reset));
        }
        s.append(" ");
        if (use_colors) {
            s.append(color(fg::cyan));
        }
        s.append(std::to_string(x.m_n));
        if (use_colors) {
            s.append(color(fg::reset));
        }
        s.append(" ");
        this->visit_ttype(*x.m_type);
        s.append(")");
    }
    void visit_Module(const ASR::Module_t &x) {
        if (!show_intrinsic_modules &&
                    startswith(x.m_name, "lfortran_intrinsic_")) {
            s.append("(");
            if (use_colors) {
                s.append(color(style::bold));
                s.append(color(fg::magenta));
            }
            s.append("IntrinsicModule");
            if (use_colors) {
                s.append(color(fg::reset));
                s.append(color(style::reset));
            }
            s.append(" ");
            s.append(x.m_name);
            s.append(")");
        } else {
            ASR::PickleBaseVisitor<ASRPickleVisitor>::visit_Module(x);
        };
    }

#define INTRINSIC_NAME_CASE(X)                                                 \
            case (static_cast<int64_t>(ASRUtils::IntrinsicFunctions::X)) : {   \
                s.append(#X);                                                \
                break;                                                         \
            }

    std::string convert_intrinsic_id(int x) {
        std::string s;
        if (use_colors) {
            s.append(color(style::bold));
            s.append(color(fg::green));
        }
        switch (x) {
            INTRINSIC_NAME_CASE(Sin)
            INTRINSIC_NAME_CASE(Cos)
            INTRINSIC_NAME_CASE(Gamma)
            INTRINSIC_NAME_CASE(LogGamma)
            default : {
                throw LCompilersException("pickle: intrinsic_id not implemented");
            }
        }
        if (use_colors) {
            s.append(color(fg::reset));
            s.append(color(style::reset));
        }
        return s;
    }
};

std::string pickle(ASR::asr_t &asr, bool colors, bool indent,
        bool show_intrinsic_modules) {
    ASRPickleVisitor v;
    v.use_colors = colors;
    v.indent = indent;
    v.show_intrinsic_modules = show_intrinsic_modules;
    v.visit_asr(asr);
    return v.get_str();
}

std::string pickle(ASR::TranslationUnit_t &asr, bool colors, bool indent, bool show_intrinsic_modules) {
    return pickle((ASR::asr_t &)asr, colors, indent, show_intrinsic_modules);
}

/********************** AST Pickle Tree *******************/
class ASTTreeVisitor : public AST::TreeBaseVisitor<ASTTreeVisitor>
{
public:
    std::string get_str() {
        return s;
    }
};

std::string pickle_tree(AST::ast_t &ast, bool colors) {
    ASTTreeVisitor v;
    v.use_colors = colors;
    v.visit_ast(ast);
    return v.get_str();
}

std::string pickle_tree(AST::TranslationUnit_t &ast, bool colors) {
    return pickle_tree((AST::ast_t &)ast, colors);
}

/********************** ASR Pickle Tree *******************/
class ASRTreeVisitor :
    public ASR::TreeBaseVisitor<ASRTreeVisitor>
{
public:
    bool show_intrinsic_modules;

    std::string get_str() {
        return s;
    }

};

std::string pickle_tree(ASR::asr_t &asr, bool colors, bool show_intrinsic_modules) {
    ASRTreeVisitor v;
    v.use_colors = colors;
    v.show_intrinsic_modules = show_intrinsic_modules;
    v.visit_asr(asr);
    return v.get_str();
}

std::string pickle_tree(ASR::TranslationUnit_t &asr, bool colors, bool show_intrinsic_modules) {
    return pickle_tree((ASR::asr_t &)asr, colors, show_intrinsic_modules);
}

/********************** AST Pickle Json *******************/
class ASTJsonVisitor :
    public LFortran::AST::JsonBaseVisitor<ASTJsonVisitor>
{
public:
    using LFortran::AST::JsonBaseVisitor<ASTJsonVisitor>::JsonBaseVisitor;

    std::string get_str() {
        return s;
    }
};

std::string pickle_json(LFortran::AST::ast_t &ast, LocationManager &lm) {
    ASTJsonVisitor v(lm);
    v.visit_ast(ast);
    return v.get_str();
}

std::string pickle_json(LFortran::AST::TranslationUnit_t &ast, LocationManager &lm) {
    return pickle_json((LFortran::AST::ast_t &)ast, lm);
}

/********************** ASR Pickle Json *******************/
class ASRJsonVisitor :
    public ASR::JsonBaseVisitor<ASRJsonVisitor>
{
public:
    using ASR::JsonBaseVisitor<ASRJsonVisitor>::JsonBaseVisitor;

    std::string get_str() {
        return s;
    }

    void visit_symbol(const ASR::symbol_t &x) {
        s.append("\"");
        s.append(ASRUtils::symbol_name(&x));
        s.append(" (SymbolTable");
        s.append(ASRUtils::symbol_parent_symtab(&x)->get_counter());
        s.append(")\"");
    }

    void visit_Module(const ASR::Module_t &x) {
        s.append("{");
        inc_indent(); s.append("\n" + indtd);
        s.append("\"node\": \"Module\"");
        s.append(",\n" + indtd);
        s.append("\"fields\": {");
        inc_indent(); s.append("\n" + indtd);
        s.append("\"name\": ");
        s.append("\"" + std::string(x.m_name) + "\"");
        s.append(",\n" + indtd);
        s.append("\"dependencies\": ");
        s.append("[");
        if (x.n_dependencies > 0) {
            inc_indent(); s.append("\n" + indtd);
            for (size_t i=0; i<x.n_dependencies; i++) {
                s.append("\"" + std::string(x.m_dependencies[i]) + "\"");
                if (i < x.n_dependencies-1) {
                    s.append(",\n" + indtd);
                };
            }
            dec_indent(); s.append("\n" + indtd);
        }
        s.append("]");
        s.append(",\n" + indtd);
        s.append("\"loaded_from_mod\": ");
        if (x.m_loaded_from_mod) {
            s.append("true");
        } else {
            s.append("false");
        }
        s.append(",\n" + indtd);
        s.append("\"intrinsic\": ");
        if (x.m_intrinsic) {
            s.append("true");
        } else {
            s.append("false");
        }
        dec_indent(); s.append("\n" + indtd);
        s.append("}");
        s.append(",\n" + indtd);
        append_location(s, x.base.base.loc.first, x.base.base.loc.last);
        dec_indent(); s.append("\n" + indtd);
        s.append("}");
    }
};

std::string pickle_json(ASR::asr_t &asr, LocationManager &lm) {
    ASRJsonVisitor v(lm);
    v.visit_asr(asr);
    return v.get_str();
}

std::string pickle_json(ASR::TranslationUnit_t &asr, LocationManager &lm) {
    return pickle_json((ASR::asr_t &)asr, lm);
}

} // namespace LCompilers::LFortran
