#include "libasr/asr.h"
#include "libasr/diagnostics.h"
#include <libasr/codegen/asr_to_julia.h>

namespace LFortran
{

/*
Julia operator precedence:
https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity

Can also be queried by `Base.operator_precedence()`.

Different from C++, the larger the number, the higher the precedence. To follow LFortran's
convention, we need to revert the precedence table.
*/
enum julia_prec {
    Base = 2,
    Pow,
    BitShift,
    Mul,
    Add,
    Comp,
    LogicalAnd,
    LogicalOr,
    Cond,
    Assign,
};

static inline std::string
binop_to_str_julia(const ASR::binopType t)
{
    switch (t) {
        case (ASR::binopType::Add): {
            return " + ";
        }
        case (ASR::binopType::Sub): {
            return " - ";
        }
        case (ASR::binopType::Mul): {
            return " * ";
        }
        case (ASR::binopType::Div): {
            return " / ";
        }
        case (ASR::binopType::Pow): {
            return " ^ ";
        }
        case (ASR::binopType::BitAnd): {
            return " & ";
        }
        case (ASR::binopType::BitOr): {
            return " | ";
        }
        case (ASR::binopType::BitXor): {
            return " ⊻ ";
        }
        case (ASR::binopType::BitLShift): {
            return " << ";
        }
        case (ASR::binopType::BitRShift): {
            return " >> ";
        }
        default:
            throw LCompilersException("Cannot represent the binary operator as a string");
    }
}

static inline std::string
logicalbinop_to_str_julia(const ASR::logicalbinopType t)
{
    switch (t) {
        case (ASR::logicalbinopType::And): {
            return " && ";
        }
        case (ASR::logicalbinopType::Or): {
            return " || ";
        }
        case (ASR::logicalbinopType::Eqv): {
            return " == ";
        }
        case (ASR::logicalbinopType::NEqv): {
            return " ≠ ";
        }
        default:
            throw LCompilersException("Cannot represent the boolean operator as a string");
    }
}

static inline std::string
cmpop_to_str_julia(const ASR::cmpopType t)
{
    switch (t) {
        case (ASR::cmpopType::Eq): {
            return " == ";
        }
        case (ASR::cmpopType::NotEq): {
            return " ≠ ";
        }
        case (ASR::cmpopType::Lt): {
            return " < ";
        }
        case (ASR::cmpopType::LtE): {
            return " ≤ ";
        }
        case (ASR::cmpopType::Gt): {
            return " > ";
        }
        case (ASR::cmpopType::GtE): {
            return " ≥ ";
        }
        default:
            throw LCompilersException("Cannot represent the comparison as a string");
    }
}

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

    // TODO: implement full feature
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

    // TODO:
    void visit_Module(const ASR::Module_t& x)
    {
        std::string module = "module " + std::string(x.m_name) + "\n\n";
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
        module += contains + "end\n\n";
        src = module;
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
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string decl;
        for (auto& item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
                decl += indent + "local " + this->convert_variable_decl(*v) + "\n";
            }
        }

        std::string body;
        for (size_t i = 0; i < x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            body += src;
        }

        src = contains + "function main()\n" + decl + body + "end\n\n" + "main()\n";
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

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t& x)
    {
        handle_BinOp(x);
    }

    void visit_RealBinOp(const ASR::RealBinOp_t& x)
    {
        handle_BinOp(x);
    }

    void visit_ComplexBinOp(const ASR::ComplexBinOp_t& x)
    {
        handle_BinOp(x);
    }

    template <typename T>
    void handle_BinOp(const T& x)
    {
        this->visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        this->visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        switch (x.m_op) {
            case (ASR::binopType::Add):
            case (ASR::binopType::Sub): {
                last_expr_precedence = julia_prec::Add;
                break;
            }
            case (ASR::binopType::Mul):
            case (ASR::binopType::Div):
            case (ASR::binopType::BitAnd):
            case (ASR::binopType::BitOr):
            case (ASR::binopType::BitXor): {
                last_expr_precedence = julia_prec::Mul;
                break;
            }
            case (ASR::binopType::BitLShift):
            case (ASR::binopType::BitRShift): {
                last_expr_precedence = julia_prec::BitShift;
                break;
            }
            case (ASR::binopType::Pow): {
                last_expr_precedence = julia_prec::Pow;
                break;
            }
            default:
                throw CodeGenError("BinOp: " + std::to_string(x.m_op)
                                   + " operator not implemented yet");
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
        src += binop_to_str_julia(x.m_op);
        if (right_precedence == 3) {
            src += "(" + right + ")";
        } else if (x.m_op == ASR::binopType::Sub) {
            if (right_precedence < last_expr_precedence) {
                src += right;
            } else {
                src += "(" + right + ")";
            }
        } else {
            if (right_precedence <= last_expr_precedence) {
                src += right;
            } else {
                src += "(" + right + ")";
            }
        }
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t& x)
    {
        this->visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        this->visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        switch (x.m_op) {
            case (ASR::logicalbinopType::And): {
                last_expr_precedence = julia_prec::LogicalAnd;
                break;
            }
            case (ASR::logicalbinopType::Or): {
                last_expr_precedence = julia_prec::LogicalOr;
                break;
            }
            case (ASR::logicalbinopType::NEqv): {
                last_expr_precedence = julia_prec::Comp;
                break;
            }
            case (ASR::logicalbinopType::Eqv): {
                last_expr_precedence = julia_prec::Comp;
                break;
            }
            default:
                throw CodeGenError("Unhandled switch case");
        }

        if (left_precedence <= last_expr_precedence) {
            src += left;
        } else {
            src += "(" + left + ")";
        }
        src += logicalbinop_to_str_julia(x.m_op);
        if (right_precedence <= last_expr_precedence) {
            src += right;
        } else {
            src += "(" + right + ")";
        }
    }

    void visit_DoLoop(const ASR::DoLoop_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "for ";
        ASR::Variable_t* loop_var = LFortran::ASRUtils::EXPR2VAR(x.m_head.m_v);
        std::string lvname = loop_var->m_name;
        ASR::expr_t* a = x.m_head.m_start;
        ASR::expr_t* b = x.m_head.m_end;
        ASR::expr_t* c = x.m_head.m_increment;
        LFORTRAN_ASSERT(a);
        LFORTRAN_ASSERT(b);
        int increment;
        if (!c) {
            increment = 1;
        } else {
            if (c->type == ASR::exprType::IntegerConstant) {
                increment = ASR::down_cast<ASR::IntegerConstant_t>(c)->m_n;
            } else if (c->type == ASR::exprType::IntegerUnaryMinus) {
                ASR::IntegerUnaryMinus_t* ium = ASR::down_cast<ASR::IntegerUnaryMinus_t>(c);
                increment = -ASR::down_cast<ASR::IntegerConstant_t>(ium->m_arg)->m_n;
            } else {
                throw CodeGenError("Do loop increment type not supported");
            }
        }

        out += lvname + " ∈ ";
        this->visit_expr(*a);
        out += src + ":" + (increment == 1 ? "" : (std::to_string(increment) + ":"));
        this->visit_expr(*b);
        out += src + "\n";
        indentation_level += 1;
        for (size_t i = 0; i < x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            out += src;
        }
        out += indent + "end\n";
        indentation_level -= 1;
        src = out;
    }

    void visit_If(const ASR::If_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "if ";
        this->visit_expr(*x.m_test);
        out += src + "\n";
        indentation_level += 1;
        for (size_t i = 0; i < x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            out += src;
        }
        out += indent;
        if (x.n_orelse == 0) {
            out += "end\n";
        } else {
            out += "else\n";
            for (size_t i = 0; i < x.n_orelse; i++) {
                this->visit_stmt(*x.m_orelse[i]);
                out += src;
            }
            out += indent + "end\n";
        }
        indentation_level -= 1;
        src = out;
    }

    void visit_IfExp(const ASR::IfExp_t& x)
    {
        // IfExp is like a ternary operator in Julia
        // test ? body : orelse;
        std::string out = "(";
        this->visit_expr(*x.m_test);
        out += src + ") ? (";
        this->visit_expr(*x.m_body);
        out += src + ") : (";
        this->visit_expr(*x.m_orelse);
        out += src + ")";
        src = out;
        last_expr_precedence = julia_prec::Cond;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        ASR::Function_t* s = ASR::down_cast<ASR::Function_t>(
            LFortran::ASRUtils::symbol_get_past_external(x.m_name));
        // TODO: use a mapping with a hash(s) instead:
        std::string sym_name = s->m_name;
        if (sym_name == "exit") {
            sym_name = "_xx_lcompilers_changed_exit_xx";
        }
        std::string out = indent + sym_name + "(";
        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                ASR::Variable_t* arg = LFortran::ASRUtils::EXPR2VAR(x.m_args[i].m_value);
                std::string arg_name = arg->m_name;
                if (ASRUtils::is_array(arg->m_type) && ASRUtils::is_pointer(arg->m_type)) {
                    out += "&" + arg_name;
                } else {
                    out += arg_name;
                }
            } else {
                this->visit_expr(*x.m_args[i].m_value);
                if (ASR::is_a<ASR::ArrayItem_t>(*x.m_args[i].m_value)
                    && ASR::is_a<ASR::Derived_t>(*ASRUtils::expr_type(x.m_args[i].m_value))) {
                    out += "&" + src;
                } else {
                    out += src;
                }
            }
            if (i < x.n_args - 1)
                out += ", ";
        }
        out += ")\n";
        src = out;
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t& x)
    {
        src = std::to_string(x.m_n);
        last_expr_precedence = julia_prec::Base;
    }

    void visit_RealConstant(const ASR::RealConstant_t& x)
    {
        src = double_to_scientific(x.m_r);
        last_expr_precedence = julia_prec::Base;
    }

    void visit_ComplexConstructor(const ASR::ComplexConstructor_t& x)
    {
        this->visit_expr(*x.m_re);
        std::string re = src;
        this->visit_expr(*x.m_im);
        std::string im = src;
        src = "ComplexF32(" + re + ", " + im + ")";
        if (ASRUtils::extract_kind_from_ttype_t(x.m_type) == 8) {
            src = "ComplexF64(" + re + ", " + im + ")";
        }
        last_expr_precedence = julia_prec::Base;
    }

    void visit_ComplexConstant(const ASR::ComplexConstant_t& x)
    {
        std::string re = std::to_string(x.m_re);
        std::string im = std::to_string(x.m_im);
        src = "ComplexF32(" + re + ", " + im + ")";
        if (ASRUtils::extract_kind_from_ttype_t(x.m_type) == 8) {
            src = "ComplexF64(" + re + ", " + im + ")";
        }
        last_expr_precedence = julia_prec::Base;
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t& x)
    {
        if (x.m_value == true) {
            src = "true";
        } else {
            src = "false";
        }
        last_expr_precedence = julia_prec::Base;
    }

    void visit_SetConstant(const ASR::SetConstant_t& x)
    {
        std::string out = "Set(";
        for (size_t i = 0; i < x.n_elements; i++) {
            visit_expr(*x.m_elements[i]);
            out += src;
            if (i != x.n_elements - 1)
                out += ", ";
        }
        out += ")";
        src = out;
        last_expr_precedence = julia_prec::Base;
    }

    void visit_DictConstant(const ASR::DictConstant_t& x)
    {
        LFORTRAN_ASSERT(x.n_keys == x.n_values);
        std::string out = "Dict(";
        for (size_t i = 0; i < x.n_keys; i++) {
            visit_expr(*x.m_keys[i]);
            out += src + " => ";
            visit_expr(*x.m_values[i]);
            if (i != x.n_keys - 1)
                out += ", ";
        }
        out += ")";
        src = out;
        last_expr_precedence = julia_prec::Base;
    }

    // void visit_ArrayConstant(const ASR::ArrayConstant_t &x) {
    //     std::string indent(indentation_level * indentation_spaces, ' ');
    //     from_std_vector_helper = indent + "Kokkos::View<float*> r;\n";
    //     std::string out = "from_std_vector<float>({";
    //     for (size_t i=0; i<x.n_args; i++) {
    //         this->visit_expr(*x.m_args[i]);
    //         out += src;
    //         if (i < x.n_args-1) out += ", ";
    //     }
    //     out += "})";
    //     from_std_vector_helper += indent + "r = " + out + ";\n";
    //     src = "&r";
    //     last_expr_precedence = julia_prec::Base;
    // }

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
        last_expr_precedence = julia_prec::Base;
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
        last_expr_precedence = julia_prec::Base;
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
                // In Julia, we do not need to cast float to float explicitly:
                // src = src;
                break;
            }
            case (ASR::cast_kindType::IntegerToInteger): {
                // In Julia, we do not need to cast int <-> long long explicitly:
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
        last_expr_precedence = julia_prec::Base;
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t& x)
    {
        handle_Compare(x);
    }

    void visit_RealCompare(const ASR::RealCompare_t& x)
    {
        handle_Compare(x);
    }

    void visit_ComplexCompare(const ASR::ComplexCompare_t& x)
    {
        handle_Compare(x);
    }

    void visit_LogicalCompare(const ASR::LogicalCompare_t& x)
    {
        handle_Compare(x);
    }

    void visit_StringCompare(const ASR::StringCompare_t& x)
    {
        handle_Compare(x);
    }

    template <typename T>
    void handle_Compare(const T& x)
    {
        this->visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        this->visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        last_expr_precedence = julia_prec::Comp;
        if (left_precedence <= last_expr_precedence) {
            src += left;
        } else {
            src += "(" + left + ")";
        }
        src += cmpop_to_str_julia(x.m_op);
        if (right_precedence <= last_expr_precedence) {
            src += right;
        } else {
            src += "(" + right + ")";
        }
    }

    void visit_StringLen(const ASR::StringLen_t& x)
    {
        std::string out = "length(";
        this->visit_expr(*x.m_arg);
        out += src + ")";
        src = out;
    }

    void visit_Print(const ASR::Print_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "println(", sep;
        if (x.m_separator) {
            this->visit_expr(*x.m_separator);
            sep = src;
        } else {
            sep = "\" \"";
        }
        for (size_t i = 0; i < x.n_values; i++) {
            this->visit_expr(*x.m_values[i]);
            out += src;
            if (i + 1 != x.n_values) {
                out += ", " + sep + ", ";
            }
        }
        if (x.m_end) {
            this->visit_expr(*x.m_end);
            out += src;
        }

        out += ")\n";
        src = out;
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
