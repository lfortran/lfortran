#include "libasr/asr.h"
#include "libasr/asr_utils.h"
#include "libasr/diagnostics.h"
#include <libasr/codegen/asr_to_rust.h>

namespace LFortran
{

/*
Rust operator precedence:
https://docs.rustlang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity
*/
enum rust_prec {
    Assign = 1,  // =
    Range = 3,   // ..
    LogicalOr,   // ||
    LogicalAnd,  // &&
    Comp,        // ==, !=, <, <=, >, >=
    BitOr,       // |
    BitXor,      // ^
    BitAnd,      // &
    BitShift,    // <<, >>
    Add,         // +, -
    Mul,         // *, /, %
    As,          // as, :
    Unary,       // (-), !, *(dereference), &, &mut
    Question,    // ?
    Base = 20
};

enum rust_borrow { MoveOrCopy, Borrow, BorrowMut };

static inline bool
is_right_associated_rust(int prec)
{
    return prec == rust_prec::Assign;
}

static inline std::string
binop_to_str_rust(const ASR::binopType t)
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
        case (ASR::binopType::BitAnd): {
            return " & ";
        }
        case (ASR::binopType::BitOr): {
            return " | ";
        }
        case (ASR::binopType::BitXor): {
            return " ^ ";
        }
        case (ASR::binopType::BitLShift): {
            return " << ";
        }
        case (ASR::binopType::BitRShift): {
            return " >> ";
        }
        case (ASR::binopType::Pow): {
            return "";
        }
        default:
            throw LCompilersException("Cannot represent the binary operator as a string");
    }
}

static inline std::string
logicalbinop_to_str_rust(const ASR::logicalbinopType t)
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
            return " != ";
        }
        default:
            throw LCompilersException("Cannot represent the boolean operator as a string");
    }
}

static inline std::string
cmpop_to_str_rust(const ASR::cmpopType t)
{
    switch (t) {
        case (ASR::cmpopType::Eq): {
            return " == ";
        }
        case (ASR::cmpopType::NotEq): {
            return " != ";
        }
        case (ASR::cmpopType::Lt): {
            return " < ";
        }
        case (ASR::cmpopType::LtE): {
            return " <= ";
        }
        case (ASR::cmpopType::Gt): {
            return " > ";
        }
        case (ASR::cmpopType::GtE): {
            return " >= ";
        }
        default:
            throw LCompilersException("Cannot represent the comparison as a string");
    }
}

class ASRToRustVisitor : public ASR::BaseVisitor<ASRToRustVisitor>
{
public:
    Allocator& al;
    diag::Diagnostics& diag;
    std::string src;
    int indentation_level;
    int indentation_spaces;
    int last_expr_precedence;
    bool intrinsic_module = false;
    const ASR::Function_t* current_function = nullptr;

    SymbolTable* global_scope;
    std::map<uint64_t, SymbolInfo> sym_info;
    std::map<std::string, std::set<std::string>> dependencies;

    ASRToRustVisitor(Allocator& al, diag::Diagnostics& diag)
        : al{ al }
        , diag{ diag }
    {
    }

    std::string format_type(const std::string& type,
                            const std::string& name,
                            rust_borrow borrow,
                            const std::string& default_value = "")
    {
        std::string fmt = name + ": ";
        switch (borrow) {
            case rust_borrow::MoveOrCopy:
                fmt += type;
                break;
            case rust_borrow::Borrow:
                fmt += "&" + type;
                break;
            case rust_borrow::BorrowMut:
                fmt += "&mut " + type;
                break;
        }

        if (!default_value.empty())
            fmt += " = " + default_value;

        return fmt;
    }

    std::string format_dependencies()
    {
        std::string fmt;
        if (dependencies.empty())
            return fmt;

        for (auto& p : dependencies) {
            fmt += "use " + p.first + "::{";
            for (auto it = p.second.begin(); it != p.second.end(); it++) {
                fmt += *it;
                if (std::next(it) != p.second.end())
                    fmt += ", ";
            }
            fmt += "};\n";
        }
        fmt += "\n";

        return fmt;
    }

    std::string format_binop(const std::string& left,
                             const std::string& op,
                             const std::string& right,
                             int left_precedence,
                             int right_precedence,
                             bool is_sub = false)
    {
        std::string out;
        if (is_right_associated_rust(left_precedence)) {
            out += "(" + left + ")";
        } else {
            if (left_precedence >= last_expr_precedence) {
                out += left;
            } else {
                out += "(" + left + ")";
            }
        }
        out += op;
        if (is_right_associated_rust(right_precedence)) {
            out += "(" + right + ")";
        } else if (is_sub) {
            if (right_precedence >= last_expr_precedence) {
                out += right;
            } else {
                out += "(" + right + ")";
            }
        } else {
            if (right_precedence > last_expr_precedence) {
                out += right;
            } else {
                out += "(" + right + ")";
            }
        }

        return out;
    }

    std::string get_primitive_type_name(ASR::Variable_t* farg)
    {
        std::string type_name;
        if (ASRUtils::is_integer(*farg->m_type)) {
            ASR::Integer_t* t = ASR::down_cast<ASR::Integer_t>(farg->m_type);
            type_name = "i" + std::to_string(t->m_kind * 8);
        } else if (ASRUtils::is_real(*farg->m_type)) {
            ASR::Real_t* t = ASR::down_cast<ASR::Real_t>(farg->m_type);
            type_name = "f32";
            if (t->m_kind == 8)
                type_name = "f64";
        } else if (ASRUtils::is_complex(*farg->m_type)) {
            ASR::Complex_t* t = ASR::down_cast<ASR::Complex_t>(farg->m_type);
            type_name = "Complex<f32>";
            if (t->m_kind == 8)
                type_name = "Complex<f64>";
        }
        return type_name;
    }

    void generate_array_decl(std::string& sub,
                             std::string v_m_name,
                             std::string& type_name,
                             std::string& dims,
                             ASR::dimension_t* m_dims,
                             int n_dims,
                             rust_borrow borrow,
                             bool init_default = false,
                             bool is_allocate = false)
    {
        std::string borrow_str;
        switch (borrow) {
            case rust_borrow::Borrow:
                borrow_str = "&";
                break;
            case rust_borrow::BorrowMut:
                borrow_str = "&mut ";
                break;
            default:
                break;
        }

        if (!init_default) {
            sub += v_m_name + ": " + borrow_str + "Array" + std::to_string(n_dims) + "<" + type_name
                   + ">";
        } else {
            sub += v_m_name + " = Array" + std::to_string(n_dims) + "::<" + type_name
                   + ">::default(";
            if (n_dims > 1)
                sub += "(";
            if (is_allocate)
                return;
        }
        for (int i = 0; i < n_dims; i++) {
            if (m_dims[i].m_length) {
                visit_expr(*m_dims[i].m_length);
                if (init_default)
                    sub += src;
                else
                    dims += src;
                if (i < n_dims - 1) {
                    if (init_default)
                        sub += ", ";
                    else
                        dims += ", ";
                }
            }
        }
        if (init_default) {
            sub += ")";
            if (n_dims > 1)
                sub += ")";
        }
    }


    std::string convert_variable_decl(const ASR::Variable_t& v, bool is_argument = false)
    {
        std::string sub, indent(indentation_level * indentation_spaces, ' ');
        bool is_array = ASRUtils::is_array(v.m_type);
        bool use_ref = (v.m_intent == LFortran::ASRUtils::intent_out
                        || v.m_intent == LFortran::ASRUtils::intent_inout);
        rust_borrow borrow = use_ref ? rust_borrow::BorrowMut : rust_borrow::MoveOrCopy;
        std::string dims;

        // TODO: take care of pointers
        bool init_default = !is_argument && v.m_storage != ASR::storage_typeType::Allocatable;
        if (ASRUtils::is_integer(*v.m_type)) {
            ASR::Integer_t* t = ASR::down_cast<ASR::Integer_t>(v.m_type);
            std::string type_name = "i" + std::to_string(t->m_kind * 8);
            if (is_array) {
                generate_array_decl(sub,
                                    std::string(v.m_name),
                                    type_name,
                                    dims,
                                    t->m_dims,
                                    t->n_dims,
                                    borrow,
                                    init_default);
            } else {
                sub = format_type(type_name, v.m_name, borrow, init_default ? "0" : "");
            }
        } else if (ASRUtils::is_real(*v.m_type)) {
            ASR::Real_t* t = ASR::down_cast<ASR::Real_t>(v.m_type);
            std::string type_name = "f32";
            if (t->m_kind == 8)
                type_name = "f64";
            if (is_array) {
                generate_array_decl(sub,
                                    std::string(v.m_name),
                                    type_name,
                                    dims,
                                    t->m_dims,
                                    t->n_dims,
                                    borrow,
                                    init_default);
            } else {
                sub = format_type(type_name, v.m_name, borrow, init_default ? "0.0" : "");
            }
        } else if (ASRUtils::is_complex(*v.m_type)) {
            ASR::Complex_t* t = ASR::down_cast<ASR::Complex_t>(v.m_type);
            std::string f = "f32";
            std::string type_name = "Complex<f32>";
            if (t->m_kind == 8) {
                f = "f64";
                type_name = "Complex<f64>";
            }
            if (is_array) {
                generate_array_decl(sub,
                                    std::string(v.m_name),
                                    type_name,
                                    dims,
                                    t->m_dims,
                                    t->n_dims,
                                    borrow,
                                    init_default);
            } else {
                sub = format_type(
                    type_name,
                    v.m_name,
                    borrow,
                    init_default ? "Complex::<" + f + ">::new(0.0_" + f + ", 0.0_" + f + ")" : "");
            }
        } else if (ASRUtils::is_logical(*v.m_type)) {
            std::string type_name = "bool";
            ASR::Logical_t* t = ASR::down_cast<ASR::Logical_t>(v.m_type);
            if (is_array) {
                generate_array_decl(sub,
                                    std::string(v.m_name),
                                    type_name,
                                    dims,
                                    t->m_dims,
                                    t->n_dims,
                                    borrow,
                                    init_default);
            } else {
                sub = format_type(type_name, v.m_name, borrow, init_default ? "false" : "");
            }
        } else if (ASRUtils::is_character(*v.m_type)) {
            std::string type_name = "String";
            ASR::Character_t* t = ASR::down_cast<ASR::Character_t>(v.m_type);
            if (is_array) {
                generate_array_decl(sub,
                                    std::string(v.m_name),
                                    type_name,
                                    dims,
                                    t->m_dims,
                                    t->n_dims,
                                    borrow,
                                    init_default);
            } else {
                sub = format_type(type_name, v.m_name, borrow, init_default ? "String::new()" : "");
            }
        } else if (ASR::is_a<ASR::Derived_t>(*v.m_type)) {
            // TODO: handle this
            ASR::Derived_t* t = ASR::down_cast<ASR::Derived_t>(v.m_type);
            std::string der_type_name = ASRUtils::symbol_name(t->m_derived_type);
            if (is_array) {
                generate_array_decl(sub,
                                    std::string(v.m_name),
                                    der_type_name,
                                    dims,
                                    t->m_dims,
                                    t->n_dims,
                                    borrow,
                                    init_default);
            } else {
                sub = format_type(der_type_name, v.m_name, borrow);
            }
        } else {
            diag.codegen_error_label("Type number '" + std::to_string(v.m_type->type)
                                         + "' not supported",
                                     { v.base.base.loc },
                                     "");
            throw Abort();
        }
        // if (dims.size() == 0 && v.m_storage == ASR::storage_typeType::Save) {
        //     sub = "static " + sub;
        // }
        if (v.m_symbolic_value) {
            visit_expr(*v.m_symbolic_value);
            std::string init = src;
            if (is_array && !ASR::is_a<ASR::ArrayConstant_t>(*v.m_symbolic_value)) {
                sub += ";\n" + indent + v.m_name + ".fill(" + init + ")";
            } else {
                sub += ";\n" + indent + v.m_name + " = " + init;
            }
        }

        return sub;
    }

    // Returns the declaration, no semi colon at the end
    std::string get_function_declaration(const ASR::Function_t& x)
    {
        std::string sub, inl, ret_type;
        if (x.m_inline) {
            inl = "#[inline(always)]\n";
        }
        if (x.m_return_var) {
            ASR::Variable_t* return_var = LFortran::ASRUtils::EXPR2VAR(x.m_return_var);
            if (ASRUtils::is_integer(*return_var->m_type)) {
                int kind = ASR::down_cast<ASR::Integer_t>(return_var->m_type)->m_kind;
                switch (kind) {
                    case (1):
                        ret_type = "i8";
                        break;
                    case (2):
                        ret_type = "i16";
                        break;
                    case (4):
                        ret_type = "i32";
                        break;
                    case (8):
                        ret_type = "i64";
                        break;
                }
            } else if (ASRUtils::is_real(*return_var->m_type)) {
                bool is_float = ASR::down_cast<ASR::Real_t>(return_var->m_type)->m_kind == 4;
                if (is_float) {
                    ret_type = "f32";
                } else {
                    ret_type = "f64";
                }
            } else if (ASRUtils::is_logical(*return_var->m_type)) {
                ret_type = "bool";
            } else if (ASRUtils::is_character(*return_var->m_type)) {
                ret_type = "String";
            } else if (ASRUtils::is_complex(*return_var->m_type)) {
                bool is_float = ASR::down_cast<ASR::Complex_t>(return_var->m_type)->m_kind == 4;
                if (is_float) {
                    ret_type = "Complex<f32>";
                } else {
                    ret_type = "Complex<f64>";
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
        std::string func = inl + "pub fn " + sym_name + "(";
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Variable_t* arg = LFortran::ASRUtils::EXPR2VAR(x.m_args[i]);
            LFORTRAN_ASSERT(LFortran::ASRUtils::is_arg_dummy(arg->m_intent));
            func += this->convert_variable_decl(*arg, true);
            if (i < x.n_args - 1)
                func += ", ";
        }
        func += ")";
        if (!ret_type.empty())
            func += " -> " + ret_type;
        func += " {";

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

        std::string headers = R"(use num::complex::Complex;
use num::traits::Pow;
use ndarray::{array, Array1, Array2, Array3, Array4, Array5, Array6};

)";
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
                    visit_symbol(*mod);
                    unit_src += src;
                }
            }
        }

        // Process procedures first:
        for (auto& item : x.m_global_scope->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
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
                visit_symbol(*mod);
                unit_src += src;
            }
        }

        // Then the main program:
        for (auto& item : x.m_global_scope->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
                unit_src += src;
            }
        }

        src = unit_src;
    }

    void visit_Module(const ASR::Module_t& x)
    {
        dependencies.clear();
        std::string module = "mod " + std::string(x.m_name) + " {\n";
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
                visit_Function(*s);
                contains += src;
            }
        }

        module += format_dependencies() + contains + "}\n\n";
        src = module;
        intrinsic_module = false;
    }

    void visit_Program(const ASR::Program_t& x)
    {
        dependencies.clear();

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
                decl += indent + "let mut " + this->convert_variable_decl(*v) + ";\n";
            }
        }

        std::string body;
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
            body += src;
        }

        src = format_dependencies() + contains + "fn main() {\n" + decl + body + "}\n";
        indentation_level -= 2;
    }

    void visit_BlockCall(const ASR::BlockCall_t& x)
    {
        LFORTRAN_ASSERT(ASR::is_a<ASR::Block_t>(*x.m_m));
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(x.m_m);
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string decl, body;
        std::string open_paranthesis = indent + "{\n";
        std::string close_paranthesis = indent + "}\n";
        indent += std::string(indentation_spaces, ' ');
        indentation_level += 1;
        for (auto& item : block->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
                decl += indent + "let mut " + this->convert_variable_decl(*v) + ";\n";
            }
        }
        for (size_t i = 0; i < block->n_body; i++) {
            this->visit_stmt(*block->m_body[i]);
            body += src;
        }
        src = open_paranthesis + decl + body + close_paranthesis;
        indentation_level -= 1;
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
                        decl += indent + "let mut " + this->convert_variable_decl(*v) + ";\n";
                    }
                }
            }

            current_function = &x;
            std::string body;

            for (size_t i = 0; i < x.n_body; i++) {
                visit_stmt(*x.m_body[i]);
                body += src;
            }

            current_function = nullptr;
            bool visited_return = false;

            if (x.n_body > 0 && ASR::is_a<ASR::Return_t>(*x.m_body[x.n_body - 1])) {
                visited_return = true;
            }

            if (!visited_return && x.m_return_var) {
                body += indent + "return " + LFortran::ASRUtils::EXPR2VAR(x.m_return_var)->m_name
                        + ";\n";
            }

            if (decl.size() > 0 || body.size() > 0) {
                sub += "\n" + decl + body + "}\n";
            } else {
                sub += "}\n";
            }
            indentation_level -= 1;
        }
        sub += "\n";
        src = sub;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x)
    {
        // Add dependencies
        if (x.m_name->type == ASR::symbolType::ExternalSymbol) {
            ASR::ExternalSymbol_t* e = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
            dependencies[std::string(e->m_module_name)].insert(std::string(e->m_name));
        }

        ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(
            LFortran::ASRUtils::symbol_get_past_external(x.m_name));
        std::string fn_name = fn->m_name;
        if (sym_info[get_hash((ASR::asr_t*) fn)].intrinsic_function) {
            if (fn_name == "size") {
                // TODO: implement this properly
                LFORTRAN_ASSERT(x.n_args > 0);
                visit_expr(*x.m_args[0].m_value);
                std::string var_name = src;
                std::string args;
                if (x.n_args == 1) {
                    args = "0";
                } else {
                    for (size_t i = 1; i < x.n_args; i++) {
                        visit_expr(*x.m_args[i].m_value);
                        args += src + "-1";
                        if (i < x.n_args - 1)
                            args += ", ";
                    }
                }
                src = var_name + ".extent(" + args + ")";
            } else {
                throw CodeGenError("Intrinsic function '" + fn_name + "' not implemented");
            }
        } else {
            std::string args;
            for (size_t i = 0; i < x.n_args; i++) {
                ASR::Variable_t* farg = ASRUtils::EXPR2VAR(fn->m_args[i]);
                bool use_ref = (farg->m_intent == ASR::intentType::Out
                                || farg->m_intent == ASR::intentType::InOut);

                std::string type_name, suffix;
                if (!use_ref) {
                    type_name = get_primitive_type_name(farg);
                    if (!type_name.empty()) {
                        suffix = " as " + type_name;
                    }
                }

                if (ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                    ASR::Variable_t* arg = ASRUtils::EXPR2VAR(x.m_args[i].m_value);
                    std::string arg_name = arg->m_name;
                    bool is_ref = (arg->m_intent == ASR::intentType::Out
                                   || arg->m_intent == ASR::intentType::InOut)
                                  && !ASRUtils::is_array(arg->m_type);
                    if (use_ref && !is_ref) {
                        args += "&mut " + arg_name;
                    } else if (!use_ref && is_ref) {
                        args += "*" + arg_name;
                    } else {
                        args += arg_name;
                    }
                } else {
                    visit_expr(*x.m_args[i].m_value);
                    if (!suffix.empty()) {
                        if (last_expr_precedence < rust_prec::As)
                            args += "(" + src + ")" + suffix;
                        else
                            args += src + suffix;
                    } else
                        args += src;
                }
                if (i < x.n_args - 1)
                    args += ", ";
            }
            src = fn_name + "(" + args + ")";
        }
        last_expr_precedence = rust_prec::Base;
    }

    void visit_Assignment(const ASR::Assignment_t& x)
    {
        std::string target, op = " = ";
        visit_expr(*x.m_target);
        target = src;
        visit_expr(*x.m_value);
        std::string value = src;
        std::string indent(indentation_level * indentation_spaces, ' ');
        src.clear();
        src += indent + target + op + value + ";\n";
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
        visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        std::string op = binop_to_str_rust(x.m_op);
        switch (x.m_op) {
            case (ASR::binopType::Add):
            case (ASR::binopType::Sub): {
                last_expr_precedence = rust_prec::Add;
                break;
            }
            case (ASR::binopType::Mul): {
                last_expr_precedence = rust_prec::Mul;
                break;
            }
            case (ASR::binopType::BitAnd): {
                last_expr_precedence = rust_prec::BitAnd;
                break;
            }
            case (ASR::binopType::BitOr): {
                last_expr_precedence = rust_prec::BitOr;
                break;
            }
            case (ASR::binopType::BitXor): {
                last_expr_precedence = rust_prec::BitXor;
                break;
            }
            case (ASR::binopType::Div): {
                last_expr_precedence = rust_prec::Mul;
                break;
            }
            case (ASR::binopType::BitLShift):
            case (ASR::binopType::BitRShift): {
                last_expr_precedence = rust_prec::BitShift;
                break;
            }
            // TODO:
            case (ASR::binopType::Pow): {
                std::string left_type;
                if (x.m_left->type == ASR::exprType::IntegerConstant) {
                    ASR::IntegerConstant_t* left_arg
                        = ASR::down_cast<ASR::IntegerConstant_t>(x.m_left);
                    left_type = "i"
                                + std::to_string(
                                    ASRUtils::extract_kind_from_ttype_t(left_arg->m_type) * 8);
                } else if (x.m_left->type == ASR::exprType::RealConstant) {
                    ASR::RealConstant_t* left_arg = ASR::down_cast<ASR::RealConstant_t>(x.m_left);
                    left_type = "f"
                                + std::to_string(
                                    ASRUtils::extract_kind_from_ttype_t(left_arg->m_type) * 8);
                }

                src = "(" + left;
                if (!left_type.empty())
                    src += " as " + left_type;
                src += ").pow(" + right + ")";
                last_expr_precedence = rust_prec::Base;
                return;
            }
            default:
                throw CodeGenError("BinOp: " + std::to_string(x.m_op)
                                   + " operator not implemented yet");
        }
        src = format_binop(
            left, op, right, left_precedence, right_precedence, x.m_op == ASR::binopType::Sub);
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t& x)
    {
        visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        switch (x.m_op) {
            case (ASR::logicalbinopType::And): {
                last_expr_precedence = rust_prec::LogicalAnd;
                break;
            }
            case (ASR::logicalbinopType::Or): {
                last_expr_precedence = rust_prec::LogicalOr;
                break;
            }
            case (ASR::logicalbinopType::NEqv):
            case (ASR::logicalbinopType::Eqv): {
                last_expr_precedence = rust_prec::Comp;
                break;
            }
            default:
                throw CodeGenError("Unhandled switch case");
        }

        if (left_precedence > last_expr_precedence) {
            src += left;
        } else {
            src += "(" + left + ")";
        }
        src += logicalbinop_to_str_rust(x.m_op);
        if (right_precedence > last_expr_precedence) {
            src += right;
        } else {
            src += "(" + right + ")";
        }
    }

    void visit_Allocate(const ASR::Allocate_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out, _dims;
        for (size_t i = 0; i < x.n_args; i++) {
            const ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(x.m_args[i].m_a));

            // Skip pointer allocation
            if (!ASRUtils::is_array(v->m_type))
                continue;

            out += indent;
            ASR::dimension_t* dims = x.m_args[i].m_dims;
            size_t n_dims = x.m_args[i].n_dims;

            if (ASRUtils::is_integer(*v->m_type)) {
                ASR::Integer_t* t = ASR::down_cast<ASR::Integer_t>(v->m_type);
                std::string type_name = "i" + std::to_string(t->m_kind * 8);
                generate_array_decl(out,
                                    std::string(v->m_name),
                                    type_name,
                                    _dims,
                                    nullptr,
                                    n_dims,
                                    rust_borrow::MoveOrCopy,
                                    true,
                                    true);
            } else if (ASRUtils::is_real(*v->m_type)) {
                ASR::Real_t* t = ASR::down_cast<ASR::Real_t>(v->m_type);
                std::string type_name = "f32";
                if (t->m_kind == 8)
                    type_name = "f64";
                generate_array_decl(out,
                                    std::string(v->m_name),
                                    type_name,
                                    _dims,
                                    nullptr,
                                    n_dims,
                                    rust_borrow::MoveOrCopy,
                                    true,
                                    true);
            } else if (ASRUtils::is_complex(*v->m_type)) {
                ASR::Complex_t* t = ASR::down_cast<ASR::Complex_t>(v->m_type);
                std::string type_name = "Complex<f32>";
                if (t->m_kind == 8)
                    type_name = "ComplexF<f64>";
                generate_array_decl(out,
                                    std::string(v->m_name),
                                    type_name,
                                    _dims,
                                    nullptr,
                                    n_dims,
                                    rust_borrow::MoveOrCopy,
                                    true,
                                    true);
            } else if (ASRUtils::is_logical(*v->m_type)) {
                std::string type_name = "bool";
                generate_array_decl(out,
                                    std::string(v->m_name),
                                    type_name,
                                    _dims,
                                    nullptr,
                                    n_dims,
                                    rust_borrow::MoveOrCopy,
                                    true,
                                    true);
            } else if (ASRUtils::is_character(*v->m_type)) {
                std::string type_name = "String";
                generate_array_decl(out,
                                    std::string(v->m_name),
                                    type_name,
                                    _dims,
                                    nullptr,
                                    n_dims,
                                    rust_borrow::MoveOrCopy,
                                    true,
                                    true);
            } else if (ASR::is_a<ASR::Derived_t>(*v->m_type)) {
                ASR::Derived_t* t = ASR::down_cast<ASR::Derived_t>(v->m_type);
                std::string der_type_name = ASRUtils::symbol_name(t->m_derived_type);
                generate_array_decl(out,
                                    std::string(v->m_name),
                                    der_type_name,
                                    _dims,
                                    nullptr,
                                    n_dims,
                                    rust_borrow::MoveOrCopy,
                                    true,
                                    true);
            } else {
                diag.codegen_error_label("Type number '" + std::to_string(v->m_type->type)
                                             + "' not supported",
                                         { v->base.base.loc },
                                         "");
                throw Abort();
            }


            for (size_t j = 0; j < n_dims; j++) {
                if (dims[j].m_length) {
                    visit_expr(*dims[j].m_length);
                    out += src;
                }
                if (j < n_dims - 1)
                    out += ", ";
            }
            out += ")\n";
        }
        src = out;
    }

    void visit_Assert(const ASR::Assert_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent;
        out += "assert!(";
        this->visit_expr(*x.m_test);
        out += src + ")";
        if (x.m_msg) {
            out += " ";
            this->visit_expr(*x.m_msg);
            out += src;
        }
        src = out;
    }

    // We do not need to manually deallocate in Julia.
    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& /* x */)
    {
        src.clear();
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& /* x */)
    {
        src.clear();
    }

    void visit_Select(const ASR::Select_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        this->visit_expr(*x.m_test);
        std::string var = std::move(src);
        std::string out = indent + "if ";

        for (size_t i = 0; i < x.n_body; i++) {
            if (i > 0)
                out += indent + "} else if ";
            ASR::case_stmt_t* stmt = x.m_body[i];
            if (stmt->type == ASR::case_stmtType::CaseStmt) {
                ASR::CaseStmt_t* case_stmt = ASR::down_cast<ASR::CaseStmt_t>(stmt);
                for (size_t j = 0; j < case_stmt->n_test; j++) {
                    if (j > 0)
                        out += " || ";
                    this->visit_expr(*case_stmt->m_test[j]);
                    out += var + " == " + src;
                }
                out += " {\n";
                indentation_level += 1;
                for (size_t j = 0; j < case_stmt->n_body; j++) {
                    this->visit_stmt(*case_stmt->m_body[j]);
                    out += src;
                }
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
                    out += left + " <= " + var + " && " + var + " <= " + right;
                }
                out += " {\n";
                indentation_level += 1;
                for (size_t j = 0; j < case_stmt_range->n_body; j++) {
                    this->visit_stmt(*case_stmt_range->m_body[j]);
                    out += src;
                }
                indentation_level -= 1;
            }
        }
        if (x.n_default) {
            out += indent + "} else {\n";
            indentation_level += 1;
            for (size_t i = 0; i < x.n_default; i++) {
                this->visit_stmt(*x.m_default[i]);
                out += src;
            }
            indentation_level -= 1;
        }

        out += indent + "}\n";
        src = out;
    }

    void visit_WhileLoop(const ASR::WhileLoop_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "while ";
        this->visit_expr(*x.m_test);
        out += src + " {\n";
        indentation_level += 1;
        for (size_t i = 0; i < x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
            out += src;
        }
        out += indent + "}\n";
        indentation_level -= 1;
        src = out;
    }

    void visit_Exit(const ASR::Exit_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = indent + "break\n";
    }

    void visit_Cycle(const ASR::Cycle_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = indent + "continue\n";
    }

    void visit_Return(const ASR::Return_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        if (current_function && current_function->m_return_var) {
            src = indent + "return "
                  + LFortran::ASRUtils::EXPR2VAR(current_function->m_return_var)->m_name + "\n";
        } else {
            src = indent + "return\n";
        }
    }

    void visit_GoTo(const ASR::GoTo_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = indent + "/* WARNING: GOTO is not supported in Rust! */\n";
    }

    void visit_GoToTarget(const ASR::GoToTarget_t& /* x */)
    {
    }

    void visit_Stop(const ASR::Stop_t& x)
    {
        if (x.m_code) {
            this->visit_expr(*x.m_code);
        } else {
            src = "0";
        }
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = indent + "std::process::exit(" + src + ");\n";
    }

    void visit_ErrorStop(const ASR::ErrorStop_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        src = indent + "eprintln!(\"ERROR STOP\");\n";
        src += indent + "std::process::exit(1);\n";
    }

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t& /*x*/)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + " /* FIXME: implied do loop */ ";
        src = out;
        last_expr_precedence = rust_prec::Base;
    }

    void visit_DoLoop(const ASR::DoLoop_t& x, bool concurrent = false)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent;
        if (concurrent) {
            // TODO: handle concurrent loop
        }
        out += "for ";
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

        out += lvname + " in ";
        visit_expr(*a);
        std::string start = std::move(src);
        visit_expr(*b);
        std::string end = std::move(src);
        if (increment < 0)
            std::swap(start, end);
        std::string range = start + "..(" + end + " + 1)";
        if (abs(increment) != 1) {
            range = "(" + range + ").step_by(" + std::to_string(abs(increment)) + ")";
            if (increment < 0)
                range += ".rev()";
        } else {
            if (increment < 0)
                range = "(" + range + ").rev()";
        }

        out += range + " {\n";
        indentation_level += 1;
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
            out += src;
        }
        out += indent + "}\n";
        indentation_level -= 1;
        src = out;
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t& x)
    {
        const ASR::DoLoop_t do_loop = ASR::DoLoop_t{ x.base, x.m_head, x.m_body, x.n_body };
        visit_DoLoop(do_loop, true);
    }

    void visit_If(const ASR::If_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "if ";
        visit_expr(*x.m_test);
        out += src + " {\n";
        indentation_level += 1;
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
            out += src;
        }
        out += indent;
        if (x.n_orelse == 0) {
            out += "}\n";
        } else {
            out += "} else {\n";
            for (size_t i = 0; i < x.n_orelse; i++) {
                visit_stmt(*x.m_orelse[i]);
                out += src;
            }
            out += indent + "}\n";
        }
        indentation_level -= 1;
        src = out;
    }

    void visit_IfExp(const ASR::IfExp_t& x)
    {
        std::string out = "if ";
        visit_expr(*x.m_test);
        out += src + "{ ";
        visit_expr(*x.m_body);
        out += src + "} else {";
        visit_expr(*x.m_orelse);
        out += src + "}";
        src = out;
        last_expr_precedence = rust_prec::Base;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x)
    {
        // Add dependencies
        if (x.m_name->type == ASR::symbolType::ExternalSymbol) {
            ASR::ExternalSymbol_t* e = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
            dependencies[std::string(e->m_module_name)].insert(std::string(e->m_name));
        }

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
            ASR::Variable_t* sarg = ASRUtils::EXPR2VAR(s->m_args[i]);
            bool use_ref = (sarg->m_intent == ASR::intentType::Out
                            || sarg->m_intent == ASR::intentType::InOut);

            std::string type_name, suffix;
            if (!use_ref) {
                type_name = get_primitive_type_name(sarg);
                if (!type_name.empty()) {
                    suffix = " as " + type_name;
                }
            }

            if (ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                ASR::Variable_t* arg = ASRUtils::EXPR2VAR(x.m_args[i].m_value);
                std::string arg_name = arg->m_name;
                bool is_ref = (arg->m_intent == ASR::intentType::Out
                               || arg->m_intent == ASR::intentType::InOut)
                              && !ASRUtils::is_array(arg->m_type);
                if (use_ref && !is_ref) {
                    out += "&mut " + arg_name;
                } else if (!use_ref && is_ref) {
                    out += "*" + arg_name;
                } else {
                    out += arg_name;
                }
            } else {
                visit_expr(*x.m_args[i].m_value);
                if (!suffix.empty()) {
                    if (last_expr_precedence < rust_prec::As)
                        out += "(" + src + ")" + suffix;
                    else
                        out += src + suffix;
                } else
                    out += src;
            }
            if (i < x.n_args - 1)
                out += ", ";
        }
        out += ");\n";
        src = out;
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t& x)
    {
        src = std::to_string(x.m_n);
        last_expr_precedence = rust_prec::Base;
    }

    void visit_RealConstant(const ASR::RealConstant_t& x)
    {
        src = double_to_scientific(x.m_r);
        last_expr_precedence = rust_prec::Base;
    }

    void visit_ComplexConstructor(const ASR::ComplexConstructor_t& x)
    {
        std::string f = ASRUtils::extract_kind_from_ttype_t(x.m_type) == 8 ? "f64" : "f32";
        visit_expr(*x.m_re);
        std::string re = std::move(src);
        if (last_expr_precedence >= rust_prec::As)
            re += " as " + f;
        else
            re = "(" + re + ") as " + f;
        visit_expr(*x.m_im);
        std::string im = std::move(src);
        if (last_expr_precedence >= rust_prec::As)
            im += " as " + f;
        else
            im = "(" + im + ") as " + f;
        src = "Complex::<" + f + ">::new(" + re + ", " + im + ")";
        last_expr_precedence = rust_prec::Base;
    }

    void visit_ComplexConstant(const ASR::ComplexConstant_t& x)
    {
        std::string f = ASRUtils::extract_kind_from_ttype_t(x.m_type) == 8 ? "f64" : "f32";
        std::string re = std::to_string(x.m_re);
        std::string im = std::to_string(x.m_im);
        src = "Complex::<" + f + ">::new(" + re + "_" + f + ", " + im + "_" + f + ")";
        last_expr_precedence = rust_prec::Base;
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t& x)
    {
        if (x.m_value == true) {
            src = "true";
        } else {
            src = "false";
        }
        last_expr_precedence = rust_prec::Base;
    }

    void visit_TupleConstant(const ASR::TupleConstant_t& x)
    {
        std::string out = "(";
        for (size_t i = 0; i < x.n_elements; i++) {
            visit_expr(*x.m_elements[i]);
            out += src;
            if (i != x.n_elements - 1)
                out += ", ";
        }
        out += ")";
        src = out;
        last_expr_precedence = rust_prec::Base;
    }

    void visit_ArrayConstant(const ASR::ArrayConstant_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = "array![";
        for (size_t i = 0; i < x.n_args; i++) {
            visit_expr(*x.m_args[i]);
            out += src;
            if (i < x.n_args - 1)
                out += ", ";
        }
        out += "]";
        src = out;
        last_expr_precedence = rust_prec::Base;
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
        src += "\".to_string()";
        last_expr_precedence = rust_prec::Base;
    }

    void visit_Var(const ASR::Var_t& x)
    {
        const ASR::symbol_t* s = ASRUtils::symbol_get_past_external(x.m_v);
        ASR::Variable_t* sv = ASR::down_cast<ASR::Variable_t>(s);
        src = std::string(sv->m_name);
        bool use_ref = (sv->m_intent == LFortran::ASRUtils::intent_out
                        || sv->m_intent == LFortran::ASRUtils::intent_inout)
                       && !ASRUtils::is_array(sv->m_type);
        if (use_ref)
            src = "*" + src;
        last_expr_precedence = rust_prec::Base;
    }

    void visit_DerivedRef(const ASR::DerivedRef_t& x)
    {
        std::string der_expr, member;
        this->visit_expr(*x.m_v);
        der_expr = std::move(src);
        member = ASRUtils::symbol_name(x.m_m);
        src = der_expr + "." + member;
    }

    void visit_Cast(const ASR::Cast_t& x)
    {
        visit_expr(*x.m_arg);
        switch (x.m_kind) {
            case (ASR::cast_kindType::IntegerToReal): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                switch (dest_kind) {
                    case 4:
                        src += " as f32";
                        break;
                    case 8:
                        src += " as f64";
                        break;
                    default:
                        throw CodeGenError("Cast IntegerToReal: Unsupported Kind "
                                           + std::to_string(dest_kind));
                }
                break;
            }
            case (ASR::cast_kindType::IntegerToInteger): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src += " as i" + std::to_string(dest_kind * 8);
                break;
            }
            case (ASR::cast_kindType::RealToInteger): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src += ".trunc() as i" + std::to_string(dest_kind * 8);
                break;
            }
            case (ASR::cast_kindType::RealToReal): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src += " as f" + std::to_string(dest_kind * 8);
                break;
            }
            case (ASR::cast_kindType::ComplexToComplex): {
                break;
            }
            case (ASR::cast_kindType::IntegerToComplex): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                std::string type_name = "f" + std::to_string(dest_kind * 8);
                src = "Complex::<" + type_name + ">::new(" + src + " as " + type_name + ", 0.0)";
                break;
            }
            case (ASR::cast_kindType::ComplexToReal): {
                src += ".re()";
                break;
            }
            case (ASR::cast_kindType::RealToComplex): {
                int dest_kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                src = "Complex::<f" + std::to_string(dest_kind * 8) + ">::new(" + src + ", 0.0)";
                break;
            }
            case (ASR::cast_kindType::LogicalToInteger): {
                src += " as i32";
                break;
            }
            case (ASR::cast_kindType::IntegerToLogical): {
                src += " as bool";
                break;
            }
            default:
                throw CodeGenError("Cast kind " + std::to_string(x.m_kind) + " not implemented",
                                   x.base.base.loc);
        }
        last_expr_precedence = rust_prec::Base;
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
        visit_expr(*x.m_left);
        std::string left = std::move(src);
        int left_precedence = last_expr_precedence;
        visit_expr(*x.m_right);
        std::string right = std::move(src);
        int right_precedence = last_expr_precedence;
        last_expr_precedence = rust_prec::Comp;
        if (left_precedence > last_expr_precedence) {
            src += left;
        } else {
            src += "(" + left + ")";
        }
        src += cmpop_to_str_rust(x.m_op);
        if (right_precedence > last_expr_precedence) {
            src += right;
        } else {
            src += "(" + right + ")";
        }
    }

    void visit_IntegerBitNot(const ASR::IntegerBitNot_t& x)
    {
        visit_expr(*x.m_arg);
        int expr_precedence = last_expr_precedence;
        last_expr_precedence = rust_prec::Unary;
        if (expr_precedence > last_expr_precedence) {
            src = "!" + src;
        } else {
            src = "!(" + src + ")";
        }
    }

    void visit_IntegerUnaryMinus(const ASR::IntegerUnaryMinus_t& x)
    {
        handle_UnaryMinus(x);
    }

    void visit_RealUnaryMinus(const ASR::RealUnaryMinus_t& x)
    {
        handle_UnaryMinus(x);
    }

    void visit_ComplexUnaryMinus(const ASR::ComplexUnaryMinus_t& x)
    {
        handle_UnaryMinus(x);
    }

    template <typename T>
    void handle_UnaryMinus(const T& x)
    {
        visit_expr(*x.m_arg);
        int expr_precedence = last_expr_precedence;
        last_expr_precedence = rust_prec::Unary;
        if (expr_precedence > last_expr_precedence) {
            src = "-" + src;
        } else {
            src = "-(" + src + ")";
        }
    }

    void visit_LogicalNot(const ASR::LogicalNot_t& x)
    {
        visit_expr(*x.m_arg);
        int expr_precedence = last_expr_precedence;
        last_expr_precedence = rust_prec::Unary;
        if (expr_precedence > last_expr_precedence) {
            src = "!" + src;
        } else {
            src = "!(" + src + ")";
        }
    }

    void visit_ComplexRe(const ASR::ComplexRe_t& x)
    {
        visit_expr(*x.m_arg);
        src += ".re()";
    }

    void visit_ComplexIm(const ASR::ComplexIm_t& x)
    {
        visit_expr(*x.m_arg);
        src += ".im()";
    }

    void visit_StringItem(const ASR::StringItem_t& x)
    {
        this->visit_expr(*x.m_idx);
        std::string idx = std::move(src);
        if (last_expr_precedence < rust_prec::Add)
            idx = "((" + idx + ") - 1) as usize";
        else
            idx = "(" + idx + " - 1) as usize";
        this->visit_expr(*x.m_arg);
        std::string str = std::move(src);
        src = str + ".char().nth(" + idx + ").unwrap()";
    }

    void visit_StringLen(const ASR::StringLen_t& x)
    {
        visit_expr(*x.m_arg);
        src = src + ".len()";
    }

    // TODO: We need a pass to preprocess ArraySize calls to indicate that a cast is needed.
    // For now, we force convert them to i32.
    void visit_ArraySize(const ASR::ArraySize_t& x)
    {
        this->visit_expr(*x.m_v);
        std::string var_name = src;
        std::string args = "";
        if (x.m_dim == nullptr) {
            src = var_name + ".len() as i32";
        } else {
            this->visit_expr(*x.m_dim);
            std::string index = std::move(src);
            src = var_name + ".shape()[";
            if (last_expr_precedence >= rust_prec::Add)
                src += "(" + index + " - 1) as usize] as i32";
            else
                src += "((" + index + ") - 1) as usize] as i32";
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t& x)
    {
        visit_expr(*x.m_v);
        std::string out = src;
        ASR::dimension_t* m_dims;
        ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(x.m_v), m_dims);
        out += "[";
        if (x.n_args > 1)
            out += "(";
        std::string index = "";
        for (size_t i = 0; i < x.n_args; i++) {
            std::string index = "";
            if (x.m_args[i].m_right) {
                visit_expr(*x.m_args[i].m_right);
            } else {
                src = "/* FIXME right index */";
            }
            if (last_expr_precedence >= rust_prec::Add) {
                index += src + " - 1";
            } else {
                index += "(" + src + ") - 1";
            }

            out += "(" + index + ") as usize";
            if (i < x.n_args - 1) {
                out += ", ";
            }
        }
        if (x.n_args > 1)
            out += ")";
        out += "]";
        last_expr_precedence = rust_prec::Base;
        src = out;
    }

    void visit_SetLen(const ASR::SetLen_t& x)
    {
        visit_expr(*x.m_arg);
        src += ".len()";
    }

    void visit_DictLen(const ASR::DictLen_t& x)
    {
        visit_expr(*x.m_arg);
        src += ".len()";
    }

    void visit_Print(const ASR::Print_t& x)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "println!(\"", sep;
        if (x.m_separator) {
            visit_expr(*x.m_separator);
            sep = src;
        } else {
            sep = " ";
        }
        for (size_t i = 0; i < x.n_values; i++) {
            out += "{}";
            if (i + 1 != x.n_values) {
                out += sep;
            }
        }
        out += "\", ";
        for (size_t i = 0; i < x.n_values; i++) {
            visit_expr(*x.m_values[i]);
            out += src;
            if (i + 1 != x.n_values) {
                out += ", ";
            }
        }
        if (x.m_end) {
            visit_expr(*x.m_end);
            out += src;
        }

        out += ");\n";
        src = out;
    }

    // TODO: implement real file write
    void visit_FileWrite(const ASR::FileWrite_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "// FIXME: File Write\n";
        src = out;
    }

    // TODO: implement real file read
    void visit_FileRead(const ASR::FileRead_t& /* x */)
    {
        std::string indent(indentation_level * indentation_spaces, ' ');
        std::string out = indent + "// FIXME: File Read\n";
        src = out;
    }
};

Result<std::string>
asr_to_rust(Allocator& al, ASR::TranslationUnit_t& asr, diag::Diagnostics& diag)
{
    ASRToRustVisitor v(al, diag);
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
