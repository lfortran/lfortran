#include <string>

#include <lfortran/pickle.h>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/parser.tab.hh>
#include <libasr/asr_utils.h>
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
        t = t + " " + "\"" + str_escape_c(yystype.string.str()) + "\"";
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

std::string pickle_json(LFortran::AST::ast_t &ast, LocationManager &lm, bool no_loc) {
    ASTJsonVisitor v(lm);
    v.no_loc = no_loc;
    v.visit_ast(ast);
    return v.get_str();
}

std::string pickle_json(LFortran::AST::TranslationUnit_t &ast, LocationManager &lm, bool no_loc) {
    return pickle_json((LFortran::AST::ast_t &)ast, lm, no_loc);
}

} // namespace LCompilers::LFortran
