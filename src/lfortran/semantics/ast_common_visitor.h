#ifndef LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H
#define LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H

#include <libasr/assert.h>
#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <lfortran/ast.h>
#include <libasr/bigint.h>
#include <libasr/string_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/intrinsic_subroutine_registry.h>
#include <lfortran/utils.h>
#include <lfortran/semantics/comptime_eval.h>
#include <lfortran/semantics/asr_implicit_cast_rules.h>
#include <libasr/pass/instantiate_template.h>
#include <string>
#include <set>
#include <map>
#include <limits>

using LCompilers::diag::Level;
using LCompilers::diag::Stage;
using LCompilers::diag::Label;
using LCompilers::diag::Diagnostic;

namespace LCompilers::LFortran {

template <typename T>
void extract_bind(T &x, ASR::abiType &abi_type, char *&bindc_name, diag::Diagnostics &diag) {
    if (x.m_bind) {
        AST::Bind_t *bind = AST::down_cast<AST::Bind_t>(x.m_bind);
        if (bind->n_args == 1) {
            if (AST::is_a<AST::Name_t>(*bind->m_args[0])) {
                AST::Name_t *name = AST::down_cast<AST::Name_t>(
                    bind->m_args[0]);
                if (to_lower(std::string(name->m_id)) == "c") {
                    abi_type=ASR::abiType::BindC;
                } else if (to_lower(std::string(name->m_id)) == "js") {
                    abi_type=ASR::abiType::BindJS;
                } else {
                    diag.add(diag::Diagnostic(
                        "Unsupported language in bind()",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            } else {
                    diag.add(diag::Diagnostic(
                        "Language name must be specified in bind() as plain text",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
            }
        } else {
            diag.add(diag::Diagnostic(
                "At least one argument needed in bind()",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        if (bind->n_kwargs == 1) {
            char *arg = bind->m_kwargs[0].m_arg;
            AST::expr_t *value = bind->m_kwargs[0].m_value;
            if (to_lower(std::string(arg)) == "name") {
                if (AST::is_a<AST::String_t>(*value)) {
                    AST::String_t *name = AST::down_cast<AST::String_t>(value);
                    bindc_name = name->m_s;
                } else {
                    diag.add(diag::Diagnostic(
                        "The value of the 'name' keyword argument in bind(c) must be a string",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            } else {
                diag.add(diag::Diagnostic(
                    "Unsupported keyword argument in bind()",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
    }
}

uint64_t static inline get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

#define LFORTRAN_STMT_LABEL_TYPE(x) \
        case AST::stmtType::x: { return AST::down_cast<AST::x##_t>(f)->m_label; }

static inline int64_t stmt_label(AST::stmt_t *f)
{
    switch (f->type) {
        LFORTRAN_STMT_LABEL_TYPE(Allocate)
        LFORTRAN_STMT_LABEL_TYPE(Assign)
        LFORTRAN_STMT_LABEL_TYPE(Assignment)
        LFORTRAN_STMT_LABEL_TYPE(Associate)
        LFORTRAN_STMT_LABEL_TYPE(Backspace)
        LFORTRAN_STMT_LABEL_TYPE(Close)
        LFORTRAN_STMT_LABEL_TYPE(Continue)
        LFORTRAN_STMT_LABEL_TYPE(Cycle)
        LFORTRAN_STMT_LABEL_TYPE(Deallocate)
        LFORTRAN_STMT_LABEL_TYPE(Endfile)
        LFORTRAN_STMT_LABEL_TYPE(Entry)
        LFORTRAN_STMT_LABEL_TYPE(ErrorStop)
        LFORTRAN_STMT_LABEL_TYPE(EventPost)
        LFORTRAN_STMT_LABEL_TYPE(EventWait)
        LFORTRAN_STMT_LABEL_TYPE(Exit)
        LFORTRAN_STMT_LABEL_TYPE(Flush)
        LFORTRAN_STMT_LABEL_TYPE(ForAllSingle)
        LFORTRAN_STMT_LABEL_TYPE(Format)
        LFORTRAN_STMT_LABEL_TYPE(DataStmt)
        LFORTRAN_STMT_LABEL_TYPE(FormTeam)
        LFORTRAN_STMT_LABEL_TYPE(GoTo)
        LFORTRAN_STMT_LABEL_TYPE(Include)
        LFORTRAN_STMT_LABEL_TYPE(Inquire)
        LFORTRAN_STMT_LABEL_TYPE(Nullify)
        LFORTRAN_STMT_LABEL_TYPE(Open)
        LFORTRAN_STMT_LABEL_TYPE(Return)
        LFORTRAN_STMT_LABEL_TYPE(Pragma)
        LFORTRAN_STMT_LABEL_TYPE(Print)
        LFORTRAN_STMT_LABEL_TYPE(Read)
        LFORTRAN_STMT_LABEL_TYPE(Rewind)
        LFORTRAN_STMT_LABEL_TYPE(Stop)
        LFORTRAN_STMT_LABEL_TYPE(SubroutineCall)
        LFORTRAN_STMT_LABEL_TYPE(SyncAll)
        LFORTRAN_STMT_LABEL_TYPE(SyncImages)
        LFORTRAN_STMT_LABEL_TYPE(SyncMemory)
        LFORTRAN_STMT_LABEL_TYPE(SyncTeam)
        LFORTRAN_STMT_LABEL_TYPE(Write)
        LFORTRAN_STMT_LABEL_TYPE(AssociateBlock)
        LFORTRAN_STMT_LABEL_TYPE(Block)
        LFORTRAN_STMT_LABEL_TYPE(ChangeTeam)
        LFORTRAN_STMT_LABEL_TYPE(Critical)
        LFORTRAN_STMT_LABEL_TYPE(DoConcurrentLoop)
        LFORTRAN_STMT_LABEL_TYPE(DoLoop)
        LFORTRAN_STMT_LABEL_TYPE(ForAll)
        LFORTRAN_STMT_LABEL_TYPE(If)
        LFORTRAN_STMT_LABEL_TYPE(IfArithmetic)
        LFORTRAN_STMT_LABEL_TYPE(Select)
        LFORTRAN_STMT_LABEL_TYPE(SelectRank)
        LFORTRAN_STMT_LABEL_TYPE(SelectType)
        LFORTRAN_STMT_LABEL_TYPE(Where)
        LFORTRAN_STMT_LABEL_TYPE(WhileLoop)
        default : throw LCompilersException("Unhandled type in stmt_label");
    }
}

struct IntrinsicSignature {
    std::vector<std::string> kwarg_names;
    int positional_args, max_args;

    IntrinsicSignature(std::vector<std::string> kwarg_names_ = {},
        int positional_args_ = 0, int max_args_ = 0): kwarg_names(kwarg_names_),
        positional_args(positional_args_), max_args(max_args_) {}
};

class ImpliedDoLoopValuesVisitor : public ASR::BaseWalkVisitor<ImpliedDoLoopValuesVisitor> {
    public:
    Allocator &al;
    std::vector<ASR::symbol_t*>& loop_vars;
    std::vector<int>& loop_indices;
    ASR::expr_t* value;
    ASR::ttype_t* type;
    diag::Diagnostics& diag;
    const std::map<ASRUtils::IntrinsicElementalFunctions, size_t> name2signature_varargs = {
        // max0 can accept any arbitrary number of arguments 2<=x<=100
        {ASRUtils::IntrinsicElementalFunctions::Max, 100},
        // min0 can accept any arbitrary number of arguments 2<=x<=100
        {ASRUtils::IntrinsicElementalFunctions::Min, 100},
        {ASRUtils::IntrinsicElementalFunctions::Aint, 2},
        {ASRUtils::IntrinsicElementalFunctions::Nint, 2},
        {ASRUtils::IntrinsicElementalFunctions::Anint, 2},
        {ASRUtils::IntrinsicElementalFunctions::StringContainsSet, 4},
        {ASRUtils::IntrinsicElementalFunctions::StringFindSet, 4},
        {ASRUtils::IntrinsicElementalFunctions::SubstrIndex, 4},
        {ASRUtils::IntrinsicElementalFunctions::Floor, 2},
        {ASRUtils::IntrinsicElementalFunctions::Ceiling, 2},
        {ASRUtils::IntrinsicElementalFunctions::Maskr, 2},
        {ASRUtils::IntrinsicElementalFunctions::Maskl, 2},
        {ASRUtils::IntrinsicElementalFunctions::SelectedRealKind, 3},
        {ASRUtils::IntrinsicElementalFunctions::Ishftc, 3},
        {ASRUtils::IntrinsicElementalFunctions::Ichar, 2},
        {ASRUtils::IntrinsicElementalFunctions::Char, 2},
        {ASRUtils::IntrinsicElementalFunctions::Achar, 2},
        {ASRUtils::IntrinsicElementalFunctions::Logical, 2},
        {ASRUtils::IntrinsicElementalFunctions::Real, 2},
        {ASRUtils::IntrinsicElementalFunctions::StorageSize, 2},
        {ASRUtils::IntrinsicElementalFunctions::OutOfRange, 3},
        {ASRUtils::IntrinsicElementalFunctions::StringLenTrim, 2},
        {ASRUtils::IntrinsicElementalFunctions::Int, 2},
    };

    ImpliedDoLoopValuesVisitor(Allocator& al, std::vector<ASR::symbol_t*>& loop_vars, std::vector<int>& loop_indices, ASR::expr_t* value_,
        ASR::ttype_t* type, diag::Diagnostics& diag) :
        al(al), loop_vars(loop_vars), loop_indices(loop_indices), value(value_), type(type), diag(diag) {}

    void visit_Var(const ASR::Var_t &x) {
        int loop_var_index = std::find(loop_vars.begin(), loop_vars.end(), x.m_v) - loop_vars.begin();
        // check if loop_var_index is valid
        if (loop_var_index >= (int) loop_vars.size()) {
            // this is compiletime value
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(
                                        ASRUtils::symbol_get_past_external(x.m_v));
            this->visit_expr(*var->m_value);
        } else {
            value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, loop_indices[loop_var_index], ASRUtils::symbol_type(x.m_v)));
        }
    }

    void visit_IntegerCompare( const ASR::IntegerCompare_t &x ) {
        int left_val, right_val;
        this->visit_expr(*x.m_left);
        left_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        this->visit_expr(*x.m_right);
        right_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        bool res;
        switch (x.m_op) {
            case ASR::cmpopType::Eq:
                res = left_val == right_val;
                break;
            case ASR::cmpopType::NotEq:
                res = left_val != right_val;
                break;
            case ASR::cmpopType::Gt:
                res = left_val > right_val;
                break;
            case ASR::cmpopType::LtE:
                res = left_val <= right_val;
                break;
            case ASR::cmpopType::Lt:
                res = left_val < right_val;
                break;
            case ASR::cmpopType::GtE:
                res = left_val >= right_val;
                break;
            default:
                diag.add(Diagnostic("Unsupported comparison operation in implied do loop",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
        }
        value = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, x.base.base.loc, res, x.m_type));
    }

    void visit_RealCompare( const ASR::RealCompare_t &x ) {
        double left_val, right_val;
        this->visit_expr(*x.m_left);
        if (ASR::is_a<ASR::RealConstant_t>(*value)) {
            left_val = ASR::down_cast<ASR::RealConstant_t>(value)->m_r;
        } else {
            left_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        }
        this->visit_expr(*x.m_right);
        if (ASR::is_a<ASR::RealConstant_t>(*value)) {
            right_val = ASR::down_cast<ASR::RealConstant_t>(value)->m_r;
        } else {
            right_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        }
        bool res;
        switch (x.m_op) {
            case ASR::cmpopType::Eq:
                res = left_val == right_val;
                break;
            case ASR::cmpopType::NotEq:
                res = left_val != right_val;
                break;
            case ASR::cmpopType::Gt:
                res = left_val > right_val;
                break;
            case ASR::cmpopType::LtE:
                res = left_val <= right_val;
                break;
            case ASR::cmpopType::Lt:
                res = left_val < right_val;
                break;
            case ASR::cmpopType::GtE:
                res = left_val >= right_val;
                break;
            default:
                diag.add(Diagnostic("Unsupported comparison operation in implied do loop",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
        }
        value = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, x.base.base.loc, res, x.m_type));
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, x.m_n, x.m_type));
    }

    void visit_RealConstant(const ASR::RealConstant_t &x) {
        value = ASRUtils::EXPR(ASR::make_RealConstant_t(al, x.base.base.loc, x.m_r, x.m_type));
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        value = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, x.base.base.loc, x.m_value, x.m_type));
    }

    void visit_ComplexConstant(const ASR::ComplexConstant_t &x) {
        diag.add(Diagnostic("Complex constant in compiletime evaluation implied do loop not supported",
                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }

    void visit_StringConstant(const ASR::StringConstant_t &x) {
        diag.add(Diagnostic("String constant in compiletime evaluation implied do loop not supported",
                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        int left_val, right_val;
        this->visit_expr(*x.m_left);
        left_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        this->visit_expr(*x.m_right);
        right_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        int res;
        switch (x.m_op) {
            case ASR::binopType::Mul:
                res = left_val * right_val;
                break;
            case ASR::binopType::Add:
                res = left_val + right_val;
                break;
            case ASR::binopType::Sub:
                res = left_val - right_val;
                break;
            case ASR::binopType::Div:
                res = left_val / right_val;
                break;
            case ASR::binopType::Pow:
                res = std::pow(left_val, right_val);
                break;
            default:
                diag.add(Diagnostic("Unsupported binary operation in implied do loop",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
        }
        value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, res, x.m_type));
    }

    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        double left_val, right_val;
        this->visit_expr(*x.m_left);
        if (ASR::is_a<ASR::RealConstant_t>(*value)) {
            left_val = ASR::down_cast<ASR::RealConstant_t>(value)->m_r;
        } else {
            left_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        }
        this->visit_expr(*x.m_right);
        if (ASR::is_a<ASR::RealConstant_t>(*value)) {
            right_val = ASR::down_cast<ASR::RealConstant_t>(value)->m_r;
        } else {
            right_val = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        }
        double res;
        switch (x.m_op) {
            case ASR::binopType::Mul:
                res = left_val * right_val;
                break;
            case ASR::binopType::Add:
                res = left_val + right_val;
                break;
            case ASR::binopType::Sub:
                res = left_val - right_val;
                break;
            case ASR::binopType::Div:
                res = left_val / right_val;
                break;
            case ASR::binopType::Pow:
                res = std::pow(left_val, right_val);
                break;
            default:
                diag.add(Diagnostic("Unsupported binary operation in implied do loop",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
        }
        value = ASRUtils::EXPR(ASR::make_RealConstant_t(al, x.base.base.loc, res, x.m_type));
    }

     inline size_t get_max_args(ASRUtils::IntrinsicElementalFunctions id) {
        if( name2signature_varargs.find(id) == name2signature_varargs.end() ) {
            return 1;
        }
        return name2signature_varargs.at(id);
    }
    // handle intrinsic elemental function
    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t &x) {
        Vec<ASR::expr_t*> args; args.reserve(al, x.n_args);
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::ttype_t* arg_type = ASRUtils::expr_type(x.m_args[i]);
            this->visit_expr(*x.m_args[i]);
            // TODO: handle multiple types
            if (ASRUtils::is_real(*arg_type) || ASRUtils::is_integer(*arg_type) || ASRUtils::is_logical(*arg_type)) {
                args.push_back(al, value);
            } else {
                diag.add(Diagnostic("Unsupported argument type in compiletime evaluation of intrinsics in implied do loop",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
        std::string intrinsic_name = to_lower(ASRUtils::get_intrinsic_name(x.m_intrinsic_id));
        size_t max_args = get_max_args(static_cast<ASRUtils::IntrinsicElementalFunctions>(x.m_intrinsic_id));
        for (size_t i = x.n_args; i < max_args; i++) args.push_back(al, nullptr);
        ASRUtils::create_intrinsic_function create_func =
                ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(intrinsic_name);
        ASR::expr_t* intrinsic_expr = ASRUtils::EXPR(create_func(al, x.base.base.loc, args, diag));
        ASR::IntrinsicElementalFunction_t *intrinsic_func = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(intrinsic_expr);
        this->visit_expr(*intrinsic_func->m_value);
    }
};


class CommonVisitorMethods {
public:


static inline ASR::expr_t* compare_helper(Allocator &al, ASR::expr_t* left_value, ASR::expr_t* right_value, ASR::cmpopType asr_op, const Location loc, diag::Diagnostics &diag) {
    if (ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(left_value))) {
        int64_t left_val = ASR::down_cast<ASR::IntegerConstant_t>(left_value)->m_n;
        int64_t right_val = ASR::down_cast<ASR::IntegerConstant_t>(right_value)->m_n;
        bool result = true;
        switch (asr_op) {
            case (ASR::cmpopType::Eq):  { result = result && (left_val == right_val); break; }
            case (ASR::cmpopType::Gt): { result = result && (left_val > right_val); break; }
            case (ASR::cmpopType::GtE): { result = result && (left_val >= right_val); break; }
            case (ASR::cmpopType::Lt): { result = result && (left_val < right_val); break; }
            case (ASR::cmpopType::LtE): { result = result && (left_val <= right_val); break; }
            case (ASR::cmpopType::NotEq): { result = result && (left_val != right_val); break; }
            default: {
                diag.add(diag::Diagnostic(
                    "Comparison operator not implemented",
                    Level::Error, Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        return ASRUtils::EXPR(ASR::make_LogicalConstant_t(
            al, loc, result, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4))));

    } else if (ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(left_value))) {
        double left_val = ASR::down_cast<ASR::RealConstant_t>(left_value)->m_r;
        double right_val = ASR::down_cast<ASR::RealConstant_t>(right_value)->m_r;
        bool result = true;
        switch (asr_op) {
            case (ASR::cmpopType::Eq):  { result = result && (left_val == right_val); break; }
            case (ASR::cmpopType::Gt): { result = result && (left_val > right_val); break; }
            case (ASR::cmpopType::GtE): { result = result && (left_val >= right_val); break; }
            case (ASR::cmpopType::Lt): { result = result && (left_val < right_val); break; }
            case (ASR::cmpopType::LtE): { result = result && (left_val <= right_val); break; }
            case (ASR::cmpopType::NotEq): { result = result && (left_val != right_val); break; }
            default: {
                diag.add(diag::Diagnostic(
                    "Comparison operator not implemented",
                    Level::Error, Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        return ASRUtils::EXPR(ASR::make_LogicalConstant_t(
            al, loc, result, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4))));

    } else if (ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(left_value))) {
        ASR::ComplexConstant_t *left0
            = ASR::down_cast<ASR::ComplexConstant_t>(left_value);
        ASR::ComplexConstant_t *right0
            = ASR::down_cast<ASR::ComplexConstant_t>(right_value);
        std::complex<double> left_val(left0->m_re, left0->m_im);
        std::complex<double> right_val(right0->m_re, right0->m_im);
        bool result = true;
        switch (asr_op) {
            case (ASR::cmpopType::Eq) : {
                result = result && (left_val.real() == right_val.real() &&
                        left_val.imag() == right_val.imag());
                break;
            }
            case (ASR::cmpopType::NotEq) : {
                result = result && (left_val.real() != right_val.real() ||
                        left_val.imag() != right_val.imag());
                break;
            }
            default: {
                diag.add(diag::Diagnostic(
                    "'" + ASRUtils::cmpop_to_str(asr_op) +
                    "' comparison is not supported between complex numbers",
                    Level::Error, Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        return ASRUtils::EXPR(ASR::make_LogicalConstant_t(
            al, loc, result, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4))));

    } else if (ASR::is_a<ASR::Logical_t>(*ASRUtils::expr_type(left_value))) {

        bool left_val = ASR::down_cast<ASR::LogicalConstant_t>(
                                left_value)->m_value;
        bool right_val = ASR::down_cast<ASR::LogicalConstant_t>(
                                right_value)->m_value;
        bool result = true;
        switch (asr_op) {
            case (ASR::cmpopType::Eq):  { result = result && (left_val == right_val); break; }
            case (ASR::cmpopType::Gt): { result = result && (left_val > right_val); break; }
            case (ASR::cmpopType::GtE): { result = result && (left_val >= right_val); break; }
            case (ASR::cmpopType::Lt): { result = result && (left_val < right_val); break; }
            case (ASR::cmpopType::LtE): { result = result && (left_val <= right_val); break; }
            case (ASR::cmpopType::NotEq): { result = result && (left_val != right_val); break; }
            default: {
                diag.add(diag::Diagnostic(
                    "Comparison operator not implemented",
                    Level::Error, Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        return ASRUtils::EXPR(ASR::make_LogicalConstant_t(
            al, loc, result, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4))));
    } else if (ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(left_value))) {
        char* left_val = ASR::down_cast<ASR::StringConstant_t>(
                                left_value)->m_s;
        char* right_val = ASR::down_cast<ASR::StringConstant_t>(
                                right_value)->m_s;
        std::string left_str = ASRUtils::remove_trailing_white_spaces(std::string(left_val));
        std::string right_str = ASRUtils::remove_trailing_white_spaces(std::string(right_val));
        int8_t strcmp = left_str.compare(right_str);
        bool result = true;
        switch (asr_op) {
            case (ASR::cmpopType::Eq) : {
                result = (strcmp == 0);
                break;
            }
            case (ASR::cmpopType::NotEq) : {
                result = (strcmp != 0);
                break;
            }
            case (ASR::cmpopType::Gt) : {
                result = (strcmp > 0);
                break;
            }
            case (ASR::cmpopType::GtE) : {
                result = (strcmp > 0 || strcmp == 0);
                break;
            }
            case (ASR::cmpopType::Lt) : {
                result = (strcmp < 0);
                break;
            }
            case (ASR::cmpopType::LtE) : {
                result = (strcmp < 0 || strcmp == 0);
                break;
            }
            default: LCOMPILERS_ASSERT(false); // should never happen
        }
        return ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
            al, loc, result, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4))));
    } else {
        diag.add(diag::Diagnostic(
            "Comparison operator not implemented",
            Level::Error, Stage::Semantic, {
            diag::Label("", {loc})}));
        throw SemanticAbort();
        return nullptr;
    }
}


static inline ASR::expr_t* evaluate_compiletime_values(Allocator &al, std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> &compiletime_values,
                            ASR::expr_t* left, ASR::expr_t* right, ASR::cmpopType asr_op, ASR::ttype_t* type, const Location loc, diag::Diagnostics &diag) {

    if (compiletime_values.size() == 0) {
        return compare_helper(al, ASRUtils::expr_value(left), ASRUtils::expr_value(right), asr_op, loc, diag);
    } else {
        Vec<ASR::expr_t*> args; args.reserve(al, compiletime_values.size());
        for (size_t i = 0; i < compiletime_values.size(); i++) {
            ASR::expr_t* left_val = compiletime_values[i].first;
            ASR::expr_t* right_val = compiletime_values[i].second;
            args.push_back(al, compare_helper(al, left_val, right_val, asr_op, loc, diag));
        }
        return ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc, args.p, args.size(), type, ASR::arraystorageType::ColMajor));
    }
}

static inline void populate_compiletime_values(Allocator &al, std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> &compiletime_values,
                                    ASR::expr_t* left, ASR::expr_t* right) {
    int fixed_size_left_array = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(ASRUtils::expr_value(left))); //0
    int fixed_size_right_array = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(ASRUtils::expr_value(right))); //0

    if (ASR::is_a<ASR::ArrayConstant_t>(*ASRUtils::expr_value(left))) {
        ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(left));
        for (size_t i=0; i<(size_t) ASRUtils::get_fixed_size_of_array(array->m_type); i++) {
            compiletime_values.push_back({ASRUtils::fetch_ArrayConstant_value(al, array, i), nullptr});
        }
    }
    if (ASR::is_a<ASR::ArrayConstant_t>(*ASRUtils::expr_value(right))) {
        ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(right));
        for (size_t i=0; i<(size_t) ASRUtils::get_fixed_size_of_array(array->m_type); i++) {
            if(compiletime_values.size() > i) {
                compiletime_values[i].second = ASRUtils::fetch_ArrayConstant_value(al, array, i);
            } else {
                compiletime_values.push_back({nullptr, ASRUtils::fetch_ArrayConstant_value(al, array, i)});
            }
        }
    }

    if (fixed_size_left_array == 0 && fixed_size_right_array == 0) {
        return;
    } else {
        for (size_t i = 0; i < compiletime_values.size(); i++) {
            if (compiletime_values[i].first == nullptr) {
                compiletime_values[i].first = ASRUtils::expr_value(left);
            }
            if (compiletime_values[i].second == nullptr) {
                compiletime_values[i].second = ASRUtils::expr_value(right);
            }
        }
    }
}

inline static void visit_Compare(Allocator &al, const AST::Compare_t &x,
                                   ASR::expr_t *&left, ASR::expr_t *&right,
                                   ASR::asr_t *&asr, std::string& intrinsic_op_name,
                                   SymbolTable* curr_scope,
                                   SetChar& current_function_dependencies,
                                   SetChar& current_module_dependencies,
                                   const CompilerOptions &compiler_options, diag::Diagnostics &diag) {
    if(ASRUtils::is_descriptorString(ASRUtils::expr_type(left))){
        left = ASRUtils::cast_string_descriptor_to_pointer(al, left);
    }
    if (ASRUtils::is_descriptorString(ASRUtils::expr_type(right))){
        right = ASRUtils::cast_string_descriptor_to_pointer(al, right);
    }
    ASR::cmpopType asr_op;
    switch (x.m_op) {
        case (AST::cmpopType::Eq): {
        asr_op = ASR::cmpopType::Eq;
        break;
        }
        case (AST::cmpopType::Gt): {
        asr_op = ASR::cmpopType::Gt;
        break;
        }
        case (AST::cmpopType::GtE): {
        asr_op = ASR::cmpopType::GtE;
        break;
        }
        case (AST::cmpopType::Lt): {
        asr_op = ASR::cmpopType::Lt;
        break;
        }
        case (AST::cmpopType::LtE): {
        asr_op = ASR::cmpopType::LtE;
        break;
        }
        case (AST::cmpopType::NotEq): {
        asr_op = ASR::cmpopType::NotEq;
        break;
        }
        default: {
        diag.add(diag::Diagnostic(
            "Comparison operator not implemented",
            Level::Error, Stage::Semantic, {
            diag::Label("", {x.base.base.loc})}));
        throw SemanticAbort();
        }
    }
    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(left));
    ASR::ttype_t *right_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(right));
    ASR::ttype_t *left_type2 = ASRUtils::type_get_past_array(left_type);
    ASR::ttype_t *right_type2 = ASRUtils::type_get_past_array(right_type);

    ASR::expr_t *overloaded = nullptr;
    if ( ASRUtils::use_overloaded(left, right, asr_op,
        intrinsic_op_name, curr_scope, asr, al,
        x.base.base.loc, current_function_dependencies,
        current_module_dependencies,
        [&](const std::string &msg, const Location &loc) {
                diag.add(diag::Diagnostic(
                    msg,
                    Level::Error, Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
            }) ) {
        overloaded = ASRUtils::EXPR(asr);
    }

    if (ASRUtils::is_type_parameter(*left_type) || ASRUtils::is_type_parameter(*right_type)) {
        // if overloaded is not found, then reject
        if (overloaded == nullptr) {
            std::string op_str = "==";
            switch (asr_op) {
                case (ASR::cmpopType::Eq):
                    break;
                case (ASR::cmpopType::Gt):
                    op_str = ">";
                    break;
                case (ASR::cmpopType::GtE):
                    op_str = ">=";
                    break;
                case (ASR::cmpopType::Lt):
                    op_str = "<";
                    break;
                case (ASR::cmpopType::LtE):
                    op_str = "<=";
                    break;
                case (ASR::cmpopType::NotEq):
                    op_str = "/=";
                    break;
                default:
                    LCOMPILERS_ASSERT(false);
            }
            diag.add(Diagnostic("Operator `" + op_str + "` undefined for the types in the expression `" + ASRUtils::type_to_str_fortran(left_type)
                                + " " +  op_str + " " + ASRUtils::type_to_str_fortran(right_type) + "`", Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
    }
    ASR::ttype_t *source_type = nullptr;
    ASR::ttype_t *dest_type = nullptr;

    if (((left_type2->type != ASR::ttypeType::Real &&
         left_type2->type != ASR::ttypeType::Integer) &&
        (right_type2->type != ASR::ttypeType::Real &&
         right_type2->type != ASR::ttypeType::Integer) &&
        ((left_type2->type != ASR::ttypeType::Complex ||
          right_type2->type != ASR::ttypeType::Complex) &&
         x.m_op != AST::cmpopType::Eq && x.m_op != AST::cmpopType::NotEq) &&
         (left_type2->type != ASR::ttypeType::String ||
          right_type2->type != ASR::ttypeType::String))
         && overloaded == nullptr) {
        diag.add(diag::Diagnostic(
            "Compare: only Integer or Real can be on the LHS and RHS. "
            "If operator is .eq. or .neq. then Complex type is also acceptable",
            Level::Error, Stage::Semantic, {
            diag::Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    } else {
      dest_type = right_type;
      source_type = left_type;
      if( overloaded == nullptr ) {
        ASR::expr_t **conversion_cand = &left;
        ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                    right_type, conversion_cand,
                                                    &source_type, &dest_type);

        ImplicitCastRules::set_converted_value(
            al, x.base.base.loc, conversion_cand, source_type, dest_type, diag);
      }
    }

    if( overloaded == nullptr ) {
        if (!ASRUtils::check_equal_type(ASRUtils::expr_type(left),
                                    ASRUtils::expr_type(right))) {
            diag.add(diag::Diagnostic(
                "Operands of comparison operator are of different types",
                Level::Error, Stage::Semantic, {
                diag::Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
    }
    size_t left_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(left));
    size_t right_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(right));
    ASR::dimension_t* result_shape = nullptr;
    size_t result_dims = 0;
    if( left_dims == 0 && right_dims == 0 ) {

    } else if( left_dims == 0 ) {
        ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(right), result_shape);
        result_dims = right_dims;
    } else if( right_dims == 0 ) {
        ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(left), result_shape);
        result_dims = left_dims;
    } else {
        LCOMPILERS_ASSERT(left_dims == right_dims);
        ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(left), result_shape);
        result_dims = left_dims;
    }
    ASR::ttype_t *type = ASRUtils::TYPE(
        ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
    type = ASRUtils::make_Array_t_util(al, x.base.base.loc,
            type, result_shape, result_dims);

    ASR::expr_t *value = nullptr;
    if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
        std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> comptime_values;
        populate_compiletime_values(al, comptime_values, left, right);
        value = evaluate_compiletime_values(al, comptime_values, left, right, asr_op, type, x.base.base.loc, diag);
    }
    if (ASRUtils::is_integer(*dest_type)) {
        asr = ASR::make_IntegerCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);
    } else if (ASRUtils::is_real(*dest_type)) {
        asr = ASR::make_RealCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);
    } else if (ASRUtils::is_complex(*dest_type)) {
        asr = ASR::make_ComplexCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);
    } else if (ASRUtils::is_logical(*dest_type)) {
        asr = ASR::make_LogicalCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);
    } else if (ASRUtils::is_character(*dest_type)) {
        if( (ASRUtils::is_allocatable(left) && ASRUtils::is_array(ASRUtils::expr_type(left)))
            || (ASRUtils::is_allocatable(right) && ASRUtils::is_array(ASRUtils::expr_type(right))) ) {
            type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, x.base.base.loc,
                ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(type))));
        }
        asr = ASR::make_StringCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);
    }
    if (overloaded != nullptr) {
        asr = ASR::make_OverloadedCompare_t(al, x.base.base.loc, left, asr_op, right, type,
            value, overloaded);
    }
}

inline static bool get_boolean_comparison_value(Location loc, ASR::logicalbinopType op,
                                                bool left_value, bool right_value, diag::Diagnostics &diag) {
    bool result;
    switch (op) {
        case (ASR::And):
            result = left_value && right_value;
            break;
        case (ASR::Or):
            result = left_value || right_value;
            break;
        case (ASR::NEqv):
            result = left_value != right_value;
            break;
        case (ASR::Eqv):
            result = left_value == right_value;
            break;
        default:
            diag.add(diag::Diagnostic(
                R"""(Only .and., .or., .neqv., .eqv.
                implemented for logical type operands.)""",
                Level::Error, Stage::Semantic, {
                diag::Label("", {loc})}));
            throw SemanticAbort();
    }
    return result;
}

template <typename T>
static inline ASR::expr_t* create_boolean_result_array(Allocator &al, Location loc, ASR::logicalbinopType op,
                                                        size_t arr_size,
                                                        ASR::ArrayConstant_t* left,
                                                        T right,
                                                        bool is_right_logical_constant, diag::Diagnostics &diag) {
    void* arr_data = nullptr;
    bool* array = al.allocate<bool>(arr_size);

    for (size_t i = 0; i < arr_size; i++) {
        bool right_value = is_right_logical_constant ? (bool) right : (bool) (((bool*) ((ASR::ArrayConstant_t*)right)->m_data)[i]);
        array[i] = get_boolean_comparison_value(loc, op, ((bool*) left->m_data)[i], right_value, diag);
    }

    arr_data = array;
    ASR::expr_t* result_arr_const = ASRUtils::EXPR(
                                        ASR::make_ArrayConstant_t(
                                                al, loc,
                                                left->m_n_data,arr_data,
                                                left->m_type,
                                                left->m_storage_format));
    return result_arr_const;
}

inline static void visit_BoolOp(Allocator &al, const AST::BoolOp_t &x,
                                  ASR::expr_t *&left, ASR::expr_t *&right,
                                  ASR::asr_t *&asr, diag::Diagnostics &diag) {
    ASR::logicalbinopType op;
    switch (x.m_op) {
        case (AST::And):
            op = ASR::And;
            break;
        case (AST::Or):
            op = ASR::Or;
            break;
        case (AST::Xor):
            op = ASR::Xor;
            diag.semantic_warning_label(
                ".xor. is an LFortran extension", { x.base.base.loc }, "LFortran extension");
            break;
        case (AST::NEqv):
            op = ASR::NEqv;
            break;
        case (AST::Eqv):
            op = ASR::Eqv;
            break;
        default:
            diag.add(diag::Diagnostic(
                R"""(Only .and., .or., .xor., .neqv., .eqv.
                                    implemented for logical type operands.)""",
                Level::Error, Stage::Semantic, {
                diag::Label("", {x.base.base.loc})}));
            throw SemanticAbort();
    }

    ASR::ttype_t *left_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(left));
    ASR::ttype_t *right_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(right));

    LCOMPILERS_ASSERT(
        ASRUtils::check_equal_type(ASRUtils::expr_type(left), ASRUtils::expr_type(right)));

    ASR::expr_t *value = nullptr;
    ASR::expr_t* left_expr_value = ASRUtils::expr_value(left);
    ASR::expr_t* right_expr_value = ASRUtils::expr_value(right);

    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (left_expr_value != nullptr && right_expr_value != nullptr) {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Logical_t>(*ASRUtils::extract_type(left_type)));

        if (ASR::is_a<ASR::LogicalBinOp_t>(*left)) {
            left_expr_value = ASR::down_cast<ASR::LogicalBinOp_t>(left)->m_value;
            left_type = ASRUtils::expr_type(left_expr_value);
        }
        if (ASR::is_a<ASR::LogicalBinOp_t>(*right)) {
            right_expr_value = ASR::down_cast<ASR::LogicalBinOp_t>(right)->m_value;
            right_type = ASRUtils::expr_type(right_expr_value);
        }

        if (ASRUtils::is_array(left_type) && !ASRUtils::is_array(right_type)) {
            ASR::ArrayConstant_t* arr_const;
            bool logical_const;
            arr_const = ASR::down_cast<ASR::ArrayConstant_t>(left_expr_value);
            logical_const = ASR::down_cast<ASR::LogicalConstant_t>(right_expr_value)->m_value;

            size_t arr_size = ASRUtils::get_fixed_size_of_array(left_type);
            value = create_boolean_result_array(al, x.base.base.loc, op, arr_size, arr_const, logical_const, true, diag);

        } else if (!ASRUtils::is_array(left_type) && ASRUtils::is_array(right_type)) {
            ASR::ArrayConstant_t* arr_const;
            bool logical_const;
            arr_const = ASR::down_cast<ASR::ArrayConstant_t>(right_expr_value);
            logical_const = ASR::down_cast<ASR::LogicalConstant_t>(left_expr_value)->m_value;

            size_t arr_size = ASRUtils::get_fixed_size_of_array(right_type);
            value = create_boolean_result_array(al, x.base.base.loc, op, arr_size, arr_const, logical_const, true, diag);

        } else if (ASRUtils::is_array(left_type) && ASRUtils::is_array(right_type)) {
            ASR::ArrayConstant_t* left_arr_const = ASR::down_cast<ASR::ArrayConstant_t>(
                                                        left_expr_value);
            ASR::ArrayConstant_t* right_arr_const = ASR::down_cast<ASR::ArrayConstant_t>(
                                                        right_expr_value);

            if (ASRUtils::get_fixed_size_of_array(left_type) != ASRUtils::get_fixed_size_of_array(right_type)) {
                diag.add(Diagnostic(
                    "Shapes for operands are not conformable",
                    Level::Error, Stage::Semantic, {
                        Label("",{ x.m_left->base.loc, x.m_right->base.loc })
                }));
                throw SemanticAbort();
            }

            size_t arr_size = ASRUtils::get_fixed_size_of_array(left_type);
            value = create_boolean_result_array(al, x.base.base.loc, op, arr_size, left_arr_const, right_arr_const, false, diag);

        } else {
            bool left_value = ASR::down_cast<ASR::LogicalConstant_t>(left_expr_value)->m_value;
            bool right_value = ASR::down_cast<ASR::LogicalConstant_t>(right_expr_value)->m_value;

            bool result = get_boolean_comparison_value(x.base.base.loc, op, left_value, right_value, diag);

            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                al, x.base.base.loc, result, left_type));
        }
    }

    ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
    if ( ASRUtils::is_array(right_type) ) {
        left_type = ASRUtils::duplicate_type(al, right_type);
    }
    asr = ASR::make_LogicalBinOp_t(al, x.base.base.loc, left, op, right, left_type, value);

  }

  inline static void visit_UnaryOp(Allocator &al, const AST::UnaryOp_t &x,
                                   ASR::expr_t *&operand, ASR::asr_t *&asr,
                                   SymbolTable* current_scope,
                                   SetChar& current_function_dependencies,
                                   SetChar& current_module_dependencies, diag::Diagnostics &diag) {

    ASR::ttype_t *operand_type = ASRUtils::expr_type(operand);
    ASR::expr_t *value = nullptr;
    if (x.m_op == AST::unaryopType::UAdd) {

        if (ASRUtils::is_integer(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(operand))->m_n;
                asr = ASR::make_IntegerConstant_t(al, x.base.base.loc, op_value, operand_type);
            }
        }
        else if (ASRUtils::is_real(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                double op_value = ASR::down_cast<ASR::RealConstant_t>(
                                ASRUtils::expr_value(operand))->m_r;
                asr = ASR::make_RealConstant_t(al, x.base.base.loc, op_value, operand_type);
            }
        }
        return;

    } else if (x.m_op == AST::unaryopType::USub) {

        if (ASRUtils::is_integer(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(operand))->m_n;
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, -op_value, operand_type));
            }
            asr = ASR::make_IntegerUnaryMinus_t(al, x.base.base.loc, operand,
                                                    operand_type, value);
            return;
        } else if (ASRUtils::is_real(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                double op_value = ASR::down_cast<ASR::RealConstant_t>(
                                        ASRUtils::expr_value(operand))->m_r;
                value = ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(
                    al, x.base.base.loc, -op_value, operand_type));
            }
            asr = ASR::make_RealUnaryMinus_t(al, x.base.base.loc, operand,
                                             operand_type, value);
            return;
        } else if (ASRUtils::is_complex(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                ASR::ComplexConstant_t *c = ASR::down_cast<ASR::ComplexConstant_t>(
                                    ASRUtils::expr_value(operand));
                std::complex<double> op_value(c->m_re, c->m_im);
                std::complex<double> result;
                result = -op_value;
                value = ASR::down_cast<ASR::expr_t>(
                        ASR::make_ComplexConstant_t(al, x.base.base.loc, std::real(result),
                        std::imag(result), operand_type));
            }
            asr = ASR::make_ComplexUnaryMinus_t(al, x.base.base.loc, operand,
                                                    operand_type, value);
            return;
        } else if( ASR::is_a<ASR::StructType_t>(*operand_type) && !ASRUtils::is_class_type(operand_type) ) {
            ASR::expr_t* overloaded_uminus = nullptr;
            if( ASRUtils::use_overloaded_unary_minus(operand,
                current_scope, asr, al,
                x.base.base.loc, current_function_dependencies,
                current_module_dependencies,
                [&](const std::string &msg, const Location &loc)
                {
                    diag.add(diag::Diagnostic(
                        msg,
                        Level::Error, Stage::Semantic, {
                        diag::Label("", {loc})}));
                    throw SemanticAbort();
                }) ) {
                overloaded_uminus = ASRUtils::EXPR(asr);
            }
            LCOMPILERS_ASSERT(overloaded_uminus != nullptr);
            asr = ASR::make_OverloadedUnaryMinus_t(al, x.base.base.loc,
                operand, ASRUtils::expr_type(overloaded_uminus),
                nullptr, overloaded_uminus);
        } else {
            LCOMPILERS_ASSERT(false);
        }
        return;

    } else if (x.m_op == AST::unaryopType::Invert) {

        if (ASRUtils::is_integer(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(operand))->m_n;
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, ~op_value, operand_type));
            }
            asr = ASR::make_IntegerBitNot_t(al, x.base.base.loc, operand, operand_type, value);
            return;
        }
        else {
            diag.add(diag::Diagnostic(
                "Argument of `not` intrinsic must be INTEGER",
                Level::Error, Stage::Semantic, {
                diag::Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

    } else if (x.m_op == AST::unaryopType::Not) {
        if (ASRUtils::is_logical(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                bool op_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                ASRUtils::expr_value(operand))->m_value;
                value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                    al, x.base.base.loc, !op_value, operand_type));
            }
            asr = ASR::make_LogicalNot_t(al, x.base.base.loc, operand, operand_type, value);
            return;
        }
        else {
            diag.add(diag::Diagnostic(
                "Operand of .not. operator is "+
                std::string(ASRUtils::type_to_str_fortran(operand_type)),
                Level::Error, Stage::Semantic, {
                diag::Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
    }
  }

}; // class CommonVisitorMethods


template <class Derived>
class CommonVisitor : public AST::BaseVisitor<Derived> {
public:
    diag::Diagnostics &diag;
    std::map<AST::operatorType, std::string> binop2str = {
        {AST::operatorType::Mul, "~mul"},
        {AST::operatorType::Add, "~add"},
        {AST::operatorType::Sub, "~sub"},
        {AST::operatorType::Div, "~div"},
    };

    std::map<AST::cmpopType, std::string> cmpop2str = {
        {AST::cmpopType::Eq, "~eq"},
        {AST::cmpopType::NotEq, "~noteq"},
        {AST::cmpopType::Lt, "~lt"},
        {AST::cmpopType::LtE, "~lte"},
        {AST::cmpopType::Gt, "~gt"},
        {AST::cmpopType::GtE, "~gte"}
    };

    std::map<AST::intrinsicopType, std::string> intrinsic2str = {
        {AST::intrinsicopType::STAR, "~mul"},
        {AST::intrinsicopType::PLUS, "~add"},
        {AST::intrinsicopType::DIV, "~div"},
        {AST::intrinsicopType::EQ, "~eq"},
        {AST::intrinsicopType::NOTEQ, "~noteq"},
        {AST::intrinsicopType::LT, "~lt"},
        {AST::intrinsicopType::LTE, "~lte"},
        {AST::intrinsicopType::GT, "~gt"},
        {AST::intrinsicopType::GTE, "~gte"},
        {AST::intrinsicopType::MINUS, "~sub"},
        {AST::intrinsicopType::CONCAT, "~concat"}
    };

    std::map<std::string, IntrinsicSignature> name2signature = {
        {"any", IntrinsicSignature({"mask", "dim"}, 1, 2)},
        {"all", IntrinsicSignature({"mask", "dim"}, 1, 2)},
        {"iany", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"iall", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"norm2", IntrinsicSignature({"array", "dim"}, 1, 2)},
        {"sum", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"product", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"iparity", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"matmul", IntrinsicSignature({"matrix_a", "matrix_b"}, 2, 2)},
        {"dot_product", IntrinsicSignature({"vector_a", "vector_b"}, 2, 2)},
        {"pack", IntrinsicSignature({"array", "mask", "vector"}, 2, 3)},
        {"unpack", IntrinsicSignature({"vector", "mask", "field"}, 3, 3)},
        {"count", IntrinsicSignature({"mask", "dim", "kind"}, 1, 3)},
        {"parity", IntrinsicSignature({"mask", "dim"}, 1, 2)},
        {"maxval", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"maxloc", IntrinsicSignature({"array", "dim", "mask", "kind", "back"}, 1, 5)},
        {"minval", IntrinsicSignature({"array", "dim", "mask"}, 1, 3)},
        {"minloc", IntrinsicSignature({"array", "dim", "mask", "kind", "back"}, 1, 5)},
        {"findloc", IntrinsicSignature({"array", "value", "dim", "mask", "kind", "back"}, 2, 6)},
        // max0 can accept any arbitrary number of arguments 2<=x<=100
        {"max0", IntrinsicSignature({"a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18", "a19", "a20", "a21", "a22", "a23", "a24", "a25", "a26", "a27", "a28", "a29", "a30", "a31", "a32", "a33", "a34", "a35", "a36", "a37", "a38", "a39", "a40", "a41", "a42", "a43", "a44", "a45", "a46", "a47", "a48", "a49", "a50", "a51", "a52", "a53", "a54", "a55", "a56", "a57", "a58", "a59", "a60", "a61", "a62", "a63", "a64", "a65", "a66", "a67", "a68", "a69", "a70", "a71", "a72", "a73", "a74", "a75", "a76", "a77", "a78", "a79", "a80", "a81", "a82", "a83", "a84", "a85", "a86", "a87", "a88", "a89", "a90", "a91", "a92", "a93", "a94", "a95", "a96", "a97", "a98", "a99", "a100"}, 2, 100)},
        // min0 can accept any arbitrary number of arguments 2<=x<=100
        {"min0", IntrinsicSignature({"a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18", "a19", "a20", "a21", "a22", "a23", "a24", "a25", "a26", "a27", "a28", "a29", "a30", "a31", "a32", "a33", "a34", "a35", "a36", "a37", "a38", "a39", "a40", "a41", "a42", "a43", "a44", "a45", "a46", "a47", "a48", "a49", "a50", "a51", "a52", "a53", "a54", "a55", "a56", "a57", "a58", "a59", "a60", "a61", "a62", "a63", "a64", "a65", "a66", "a67", "a68", "a69", "a70", "a71", "a72", "a73", "a74", "a75", "a76", "a77", "a78", "a79", "a80", "a81", "a82", "a83", "a84", "a85", "a86", "a87", "a88", "a89", "a90", "a91", "a92", "a93", "a94", "a95", "a96", "a97", "a98", "a99", "a100"}, 2, 100)},
        {"min", IntrinsicSignature({"a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18", "a19", "a20", "a21", "a22", "a23", "a24", "a25", "a26", "a27", "a28", "a29", "a30", "a31", "a32", "a33", "a34", "a35", "a36", "a37", "a38", "a39", "a40", "a41", "a42", "a43", "a44", "a45", "a46", "a47", "a48", "a49", "a50", "a51", "a52", "a53", "a54", "a55", "a56", "a57", "a58", "a59", "a60", "a61", "a62", "a63", "a64", "a65", "a66", "a67", "a68", "a69", "a70", "a71", "a72", "a73", "a74", "a75", "a76", "a77", "a78", "a79", "a80", "a81", "a82", "a83", "a84", "a85", "a86", "a87", "a88", "a89", "a90", "a91", "a92", "a93", "a94", "a95", "a96", "a97", "a98", "a99", "a100"}, 2, 100)},
        {"max", IntrinsicSignature({"a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18", "a19", "a20", "a21", "a22", "a23", "a24", "a25", "a26", "a27", "a28", "a29", "a30", "a31", "a32", "a33", "a34", "a35", "a36", "a37", "a38", "a39", "a40", "a41", "a42", "a43", "a44", "a45", "a46", "a47", "a48", "a49", "a50", "a51", "a52", "a53", "a54", "a55", "a56", "a57", "a58", "a59", "a60", "a61", "a62", "a63", "a64", "a65", "a66", "a67", "a68", "a69", "a70", "a71", "a72", "a73", "a74", "a75", "a76", "a77", "a78", "a79", "a80", "a81", "a82", "a83", "a84", "a85", "a86", "a87", "a88", "a89", "a90", "a91", "a92", "a93", "a94", "a95", "a96", "a97", "a98", "a99", "a100"}, 2, 100)},
        {"merge", IntrinsicSignature({"tsource", "fsource", "mask"}, 3, 3)},
        {"sign", IntrinsicSignature({"a", "b"}, 2, 2)},
        {"aint", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"nint", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"anint", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"atan2", IntrinsicSignature({"y", "x"}, 2, 2)},
        {"shape", IntrinsicSignature({"source", "kind"}, 1, 2)},
        {"mod", IntrinsicSignature({"a", "p"}, 2, 2)},
        {"repeat", IntrinsicSignature({"string", "ncopies"}, 2, 2)},
        {"verify", IntrinsicSignature({"string", "set", "back", "kind"}, 2, 4)},
        {"scan", IntrinsicSignature({"string", "set", "back", "kind"}, 2, 4)},
        {"index", IntrinsicSignature({"string", "substring", "back", "kind"}, 2, 4)},
        {"hypot", IntrinsicSignature({"x", "y"}, 2, 2)},
        {"shiftr", IntrinsicSignature({"i", "shift"}, 2, 2)},
        {"rshift", IntrinsicSignature({"i", "shift"}, 2, 2)},
        {"shiftl", IntrinsicSignature({"i", "shift"}, 2, 2)},
        {"lshift", IntrinsicSignature({"i", "shift"}, 2, 2)},
        {"ishft", IntrinsicSignature({"i", "shift"}, 2, 2)},
        {"bgt", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"blt", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"bge", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"ble", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"lgt", IntrinsicSignature({"string_A", "string_B"}, 2, 2)},
        {"llt", IntrinsicSignature({"string_A", "string_B"}, 2, 2)},
        {"lge", IntrinsicSignature({"string_A", "string_B"}, 2, 2)},
        {"lle", IntrinsicSignature({"string_A", "string_B"}, 2, 2)},
        {"iand", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"and", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"ior", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"or", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"ieor", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"xor", IntrinsicSignature({"i", "j"}, 2, 2)},
        {"ibclr", IntrinsicSignature({"i", "pos"}, 2, 2)},
        {"ibset", IntrinsicSignature({"i", "pos"}, 2, 2)},
        {"btest", IntrinsicSignature({"i", "pos"}, 2, 2)},
        {"ibits", IntrinsicSignature({"i", "pos", "len"}, 3, 3)},
        {"floor", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"ceiling", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"scale", IntrinsicSignature({"X", "I"}, 2, 2)},
        {"dprod", IntrinsicSignature({"X", "Y"}, 2, 2)},
        {"maskr", IntrinsicSignature({"i", "kind"}, 1, 2)},
        {"maskl", IntrinsicSignature({"i", "kind"}, 1, 2)},
        {"dim", IntrinsicSignature({"X", "Y"}, 2, 2)},
        {"selected_real_kind", IntrinsicSignature({"p", "r", "radix"}, 0, 3)},
        {"nearest", IntrinsicSignature({"x", "s"}, 2, 2)},
        {"_lfortran_compiler_version", IntrinsicSignature({}, 0, 0)},
        {"compiler_options", IntrinsicSignature({}, 0, 0)},
        {"command_argument_count", IntrinsicSignature({}, 0, 0)},
        {"ishftc", IntrinsicSignature({"i", "shift", "size"}, 2, 3)},
        {"ichar", IntrinsicSignature({"C", "kind"}, 1, 2)},
        {"char", IntrinsicSignature({"I", "kind"}, 1, 2)},
        {"achar", IntrinsicSignature({"I", "kind"}, 1, 2)},
        {"set_exponent", IntrinsicSignature({"X", "I"}, 2, 2)},
        {"dshiftl", IntrinsicSignature({"i", "j", "shift"}, 3, 3)},
        {"dshiftr", IntrinsicSignature({"i", "j", "shift"}, 3, 3)},
        {"random_init", IntrinsicSignature({"repeatable", "image"}, 2, 2)},
        {"random_seed", IntrinsicSignature({"size", "put", "get"}, 0, 3)},
        {"get_command", IntrinsicSignature({"command", "length", "status"}, 0, 3)},
        {"get_command_argument", IntrinsicSignature({"number", "value", "length", "status"}, 1, 4)},
        {"system_clock", IntrinsicSignature({"count", "count_rate", "count_max"}, 0, 3)},
        {"date_and_time", IntrinsicSignature({"date", "time", "zone", "values"}, 0, 4)},
        {"get_environment_variable", IntrinsicSignature({"name", "value", "length", "status", "trim_name"}, 1, 5)},
        {"execute_command_line", IntrinsicSignature({"command", "wait", "exitstat", "cmdstat", "cmdmsg"}, 1, 5)},
        {"move_alloc", IntrinsicSignature({"from", "to"}, 2, 2)},
        {"mvbits", IntrinsicSignature({"from", "frompos", "len", "to", "topos"}, 5, 5)},
        {"modulo", IntrinsicSignature({"a", "p"}, 2, 2)},
        {"bessel_jn", IntrinsicSignature({"n", "x"}, 2, 2)},
        {"bessel_yn", IntrinsicSignature({"n", "x"}, 2, 2)},
        {"merge_bits", IntrinsicSignature({"i", "j", "mask"}, 3, 3)},
        {"logical", IntrinsicSignature({"i", "kind"}, 1, 2)},
        {"cshift", IntrinsicSignature({"array", "shift", "dim"}, 2, 3)},
        {"eoshift", IntrinsicSignature({"array", "shift", "boundary", "dim"}, 2, 4)},
        {"real", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"storage_size", IntrinsicSignature({"a", "kind"}, 1, 2)},
        {"spread", IntrinsicSignature({"source", "dim", "ncopies"}, 3, 3)},
        {"out_of_range", IntrinsicSignature({"value", "mold", "round"}, 2, 3)},
        {"same_type_as", IntrinsicSignature({"a", "b"}, 2, 2)},
        {"len_trim", IntrinsicSignature({"String", "Kind"}, 1, 2)},
        {"int", IntrinsicSignature({"i", "kind"}, 1, 2)},
        {"random_number", IntrinsicSignature({"harvest"}, 1, 1)},
        {"abs", IntrinsicSignature({"a"}, 1, 1)},
        {"acos", IntrinsicSignature({"x"}, 1, 1)},
        {"acosd", IntrinsicSignature({"x"}, 1, 1)},
        {"acosh", IntrinsicSignature({"x"}, 1, 1)},
        {"adjustl", IntrinsicSignature({"string"}, 1, 1)},
        {"adjustr", IntrinsicSignature({"string"}, 1, 1)},
        {"aimag", IntrinsicSignature({"z"}, 1, 1)},
        {"asin", IntrinsicSignature({"x"}, 1, 1)},
        {"asind", IntrinsicSignature({"x"}, 1, 1)},
        {"asinh", IntrinsicSignature({"x"}, 1, 1)},
        {"atan", IntrinsicSignature({"x"}, 1, 1)},
        {"atanh", IntrinsicSignature({"x"}, 1, 1)},
        {"bit_size", IntrinsicSignature({"i"}, 1, 1)},
        {"conjg", IntrinsicSignature({"z"}, 1, 1)},
        {"cos", IntrinsicSignature({"x"}, 1, 1)},
        {"cosd", IntrinsicSignature({"x"}, 1, 1)},
        {"cosh", IntrinsicSignature({"x"}, 1, 1)},
        {"erf", IntrinsicSignature({"x"}, 1, 1)},
        {"erfc", IntrinsicSignature({"x"}, 1, 1)},
        {"exp", IntrinsicSignature({"x"}, 1, 1)},
        {"gamma", IntrinsicSignature({"x"}, 1, 1)},
        {"log", IntrinsicSignature({"x"}, 1, 1)},
        {"log10", IntrinsicSignature({"x"}, 1, 1)},
        {"log_gamma", IntrinsicSignature({"x"}, 1, 1)},
        {"sin", IntrinsicSignature({"x"}, 1, 1)},
        {"sinh", IntrinsicSignature({"x"}, 1, 1)},
        {"sqrt", IntrinsicSignature({"x"}, 1, 1)},
        {"tan", IntrinsicSignature({"x"}, 1, 1)},
        {"tanh", IntrinsicSignature({"x"}, 1, 1)},
        {"tand", IntrinsicSignature({"x"}, 1, 1)},
        {"not", IntrinsicSignature({"i"}, 1, 1)},
        {"precision", IntrinsicSignature({"x"}, 1, 1)},
        {"range", IntrinsicSignature({"x"}, 1, 1)},
        {"tiny", IntrinsicSignature({"x"}, 1, 1)},
        {"huge", IntrinsicSignature({"x"}, 1, 1)},
        {"epsilon", IntrinsicSignature({"x"}, 1, 1)},
        {"digits", IntrinsicSignature({"x"}, 1, 1)},
        {"present", IntrinsicSignature({"a"}, 1, 1)},
        {"leadz", IntrinsicSignature({"i"}, 1, 1)},
        {"trailz", IntrinsicSignature({"i"}, 1, 1)},


        // LFortran-specific intrinsics
        {"_lfortran_set_item", IntrinsicSignature({"iterable", "index", "element"}, 3, 3)},
        {"_lfortran_list_append", IntrinsicSignature({"list", "element"}, 2, 2)},
        {"_lfortran_list_reverse", IntrinsicSignature({"list"}, 1, 1)},
        {"_lfortran_set_add", IntrinsicSignature({"set", "element"}, 2, 2)},
    };


    std::map<std::string, std::pair<std::string, std::vector<std::string>>> intrinsic_mapping = {
        {"iabs", {"abs", {"int4"}}},
        {"dabs", {"abs", {"real8"}}},
        {"cabs", {"abs", {"complex4"}}},
        {"zabs", {"abs", {"complex8"}}},
        {"cdabs", {"abs", {"complex8"}}},
        {"dsinh", {"sinh", {"real8"}}},
        {"dcosh", {"cosh", {"real8"}}},
        {"dtanh", {"tanh", {"real8"}}},
        {"dsin", {"sin", {"real8"}}},
        {"derf", {"erf", {"real8"}}},
        {"derfc", {"erfc", {"real8"}}},
        {"lgamma", {"log_gamma", {"real"}}},
        {"algama", {"log_gamma", {"real"}}},
        {"dlgama", {"log_gamma", {"real8"}}},
        {"csin", {"sin", {"complex4"}}},
        {"zsin", {"sin", {"complex8"}}},
        {"cdsin", {"sin", {"complex8"}}},
        {"dcos", {"cos", {"real8"}}},
        {"ccos", {"cos", {"complex4"}}},
        {"zcos", {"cos", {"complex8"}}},
        {"cdcos", {"cos", {"complex8"}}},
        {"dtan", {"tan", {"real8"}}},
        {"datan", {"atan", {"real8"}}},
        {"datan2", {"atan2", {"real8", "real8"}}},
        {"dimag", {"aimag", {"complex8"}}},
        {"imag", {"aimag", {"complex"}}},
        {"imagpart", {"aimag", {"complex"}}},
        {"realpart", {"real", {"complex" ,"int4"}}},
        {"isign", {"sign", {"int4", "int4"}}},
        {"dsign", {"sign", {"real8", "real8"}}},
        {"dgamma", {"gamma", {"real8"}}},
        {"dsqrt", {"sqrt", {"real8"}}},
        {"csqrt", {"sqrt", {"complex4"}}},
        {"zsqrt", {"sqrt", {"complex8"}}},
        {"cdsqrt", {"sqrt", {"complex8"}}},
        {"alog", {"log", {"real4"}}},
        {"dlog", {"log", {"real8"}}},
        {"clog", {"log", {"complex4"}}},
        {"zlog", {"log", {"complex8"}}},
        {"cdlog", {"log", {"complex8"}}},
        {"alog10", {"log10", {"real4"}}},
        {"dlog10", {"log10", {"real8"}}},
        {"dexp", {"exp", {"real8"}}},
        {"cexp", {"exp", {"complex4"}}},
        {"zexp", {"exp", {"complex8"}}},
        {"cdexp", {"exp", {"complex8"}}},
        {"min0", {"min", {"int4"}}},
        {"amin0", {"min", {"int4"}}},
        {"min1", {"min", {"real"}}},
        {"amin1", {"min", {"real4"}}},
        {"dmin1", {"min", {"real"}}},
        {"max0", {"max", {"int4"}}},
        {"amax0", {"max", {"int4"}}},
        {"max1", {"max", {"real"}}},
        {"amax1", {"max", {"real4"}}},
        {"dmax1", {"max", {"real"}}},
        {"dcmplx", {"cmplx", {"any"}}},
        {"dacos", {"acos", {"real8"}}},
        {"dacosh", {"acosh", {"real8"}}},
        {"dint", {"aint", {"real8"}}},
        {"dnint", {"anint", {"real8"}}},
        {"dasin", {"asin", {"real8"}}},
        {"dasinh", {"asinh", {"real8"}}},
        {"datanh", {"atanh", {"real8"}}},
        {"dbesj0", {"bessel_j0", {"real8"}}},
        {"dbesj1", {"bessel_j1", {"real8"}}},
        {"dbesy0", {"bessel_y0", {"real8"}}},
        {"dbesy1", {"bessel_y1", {"real8"}}},
        {"dbesjn", {"bessel_jn", {"int4", "real8"}}},
        {"dbesyn", {"bessel_yn", {"int4", "real8"}}},
        {"dconjg", {"conjg", {"complex"}}},
        {"idim", {"dim", {"int4", "int4"}}},
        {"ddim", {"dim", {"real8", "real8"}}},
        {"amod", {"mod", {"real4", "real4"}}},
        {"dmod", {"mod", {"real8", "real8"}}},
    };

    ASR::asr_t *tmp;
    std::vector<ASR::asr_t *> tmp_vec;
    Allocator &al;
    CompilerOptions &compiler_options;
    SymbolTable *current_scope;
    SymbolTable *implicit_interface_parent_scope = nullptr;
    ASR::Module_t *current_module = nullptr;
    SetChar current_module_dependencies;
    IntrinsicProcedures intrinsic_procedures;
    IntrinsicProceduresAsASRNodes intrinsic_procedures_as_asr_nodes;
    std::set<std::string> intrinsic_module_procedures_as_asr_nodes = {
        "c_loc", "c_f_pointer", "c_associated", "c_funloc"
    };

    ASR::accessType dflt_access = ASR::Public;
    bool in_module = false;
    std::map<SymbolTable*, std::map<AST::decl_attribute_t*, AST::simple_attributeType>> overloaded_ops;
    std::map<SymbolTable*, ASR::accessType> assgn;
    std::map<std::string, ASR::accessType> assgnd_access;
    std::map<std::string, std::pair<ASR::storage_typeType, AST::expr_t*>> assgnd_storage;
    ASR::storage_typeType dflt_storage = ASR::Default;
    ASR::presenceType dflt_presence = ASR::presenceType::Required;
    std::map<std::string, ASR::presenceType> assgnd_presence;
    // Current procedure arguments. Only non-empty for SymbolTableVisitor,
    // empty for BodyVisitor.
    std::vector<std::string> current_procedure_args;
    std::vector<std::string> excluded_from_symtab;
    int64_t current_symbol;
    ASR::abiType current_procedure_abi_type = ASR::abiType::Source;
    bool is_derived_type = false;
    bool is_body_visitor = false;
    bool is_requirement = false;
    bool is_template = false;
    bool is_current_procedure_templated = false;
    bool is_Function = false;
    bool in_Subroutine = false;
    bool _processing_dimensions = false;
    bool _processing_char_len = false;
    bool _declaring_variable = false;
    bool is_implicit_interface = false;
    Vec<ASR::stmt_t*> *current_body = nullptr;

    std::map<std::string, ASR::ttype_t*> implicit_dictionary;
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping;

    using common_block_varsyms = std::map<std::string, std::vector<AST::var_sym_t>>;
    std::map<std::string, std::pair<bool,std::vector<ASR::expr_t*>>> common_block_dictionary;
    std::map<uint64_t, ASR::symbol_t*> &common_variables_hash;

    std::vector<std::map<std::string, ASR::ttype_t*>> implicit_stack;
    std::map<uint64_t, std::vector<std::string>> &external_procedures_mapping;
    // mapping of hash int's of scope to 'explicit_intrinsic_procedures'
    std::map<uint64_t, std::vector<std::string>> &explicit_intrinsic_procedures_mapping;
    std::map<std::string, ASR::symbol_t*> changed_external_function_symbol;
    std::map<std::string, std::vector<AST::stmt_t*>> entry_point_mapping;
    std::vector<std::string> external_procedures;

    // Attributes defined before declaration
    std::map<std::string, ASR::symbol_t*> symbols_having_only_attributes_without_type;

    // procedures explicitly declared with 'intrinsic' attribute
    // e.g. a declaration like: 'intrinsic abs' for an intrinsic
    // elemental function 'abs'
    std::vector<std::string> explicit_intrinsic_procedures;
    std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions;
    std::map<std::string, std::vector<int>> &entry_function_arguments_mapping;
    Vec<char*> data_member_names;
    SetChar current_function_dependencies;
    ASR::ttype_t* current_variable_type_;

    int32_t enum_init_val;
    bool default_storage_save = false;
    // The map stores the symbol names of the variables that are declared earlier
    // for example: integer :: x(n), n
    // if pre_declared_array_dims[key] = 1 (means it's implicitly typed but not yet declared)
    // if pre_declared_array_dims[key] = 2 (means it's declared and so safe to use)
    std::map<std::string, int8_t> pre_declared_array_dims;

    // Stores the strings for format statements inside a function
    std::map<int64_t, std::string> format_statements;

    // fields for generics
    std::map<std::string, std::string> context_map;     // TODO: refactor treatment of context map
    std::map<uint32_t, std::map<std::string, ASR::ttype_t*>> &instantiate_types;
    std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols;
    std::vector<ASR::stmt_t*> &data_structure;
    LCompilers::LocationManager &lm;

    std::map<std::string, std::vector<std::string>> generic_procedures;
    /*
     * A struct to store the information of a postponed call to genericProcedure
     * The information should be consumed by function `evaluate_delayed_generic_procedure_calls`
     */
    struct postponed_genericProcedure_call{
        ASR::expr_t** holder; // Pointer to the expre that should hold the `ASR::FunctionCall`
        SymbolTable* current_scope; // The scope where the `AST::FuncCallOrArray` should've been evaluated. 
        AST::expr_t* func_call; // `AST::FuncCallOrArray`
        const char* var_name; // Name of the `ASR::Variable` that the functionCall is part of [integer :: arr(genericCall())]
        std::function<void(ASR::expr_t*)> check; // Lambda function in case the returning `ASR::expr` should be checked in specific way.
        // Constructor
        postponed_genericProcedure_call( ASR::expr_t** holder, SymbolTable* current_scope,
            AST::expr_t* func_call, const char* var_name,
            std::function<void(ASR::expr_t*)> check
        ) : holder(holder), current_scope(current_scope), func_call(func_call),
            var_name(var_name), check(check) {}
    };
    std::vector<postponed_genericProcedure_call> postponed_genericProcedure_calls_vec;
    // global save variable
    bool is_global_save_enabled = false;

    // implied do loop nesting
    int idl_nesting_level = 0;

    CommonVisitor(Allocator &al, SymbolTable *symbol_table,
        diag::Diagnostics &diagnostics, CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, ASR::ttype_t*>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::vector<ASR::stmt_t*> &data_structure,
            LCompilers::LocationManager &lm
    ): diag{diagnostics}, al{al}, compiler_options{compiler_options},
          current_scope{symbol_table}, implicit_mapping{implicit_mapping},
          common_variables_hash{common_variables_hash},
          external_procedures_mapping{external_procedures_mapping},
          explicit_intrinsic_procedures_mapping{explicit_intrinsic_procedures_mapping},
          entry_functions{entry_functions},entry_function_arguments_mapping{entry_function_arguments_mapping},
          current_variable_type_{nullptr}, instantiate_types{instantiate_types},
          instantiate_symbols{instantiate_symbols}, data_structure{data_structure}, lm{lm}
    {
        current_module_dependencies.reserve(al, 4);
        enum_init_val = 0;
    }

    ASR::symbol_t* resolve_symbol(const Location &loc, const std::string &sub_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *sub = scope->resolve_symbol(sub_name);
        if (!sub) {
            diag.add(Diagnostic(
                "Symbol '" + sub_name + "' not declared",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
        return sub;
    }

    ASR::symbol_t* declare_implicit_variable(const Location &loc,
            const std::string &var_name, ASR::intentType intent, ASR::expr_t* value = nullptr) {
        ASR::ttype_t *type;
        char first_letter = var_name[0];
        // The default implicit typing is:
        // implicit real (a-h,o-z)
        if (first_letter >= 'i' && first_letter <= 'n') {
            // it is an integer
            type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
        } else {
            // it is a real
            type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
        }
        if ( value != nullptr ) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *value_type = ASRUtils::expr_type(value);
            value = b.t2t(value, value_type, type);
        }
        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
            current_scope, s2c(al, var_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), intent, value, value != nullptr ? ASRUtils::expr_value(value) : value,
            ASR::storage_typeType::Default, type, nullptr,
            current_procedure_abi_type, ASR::Public,
            ASR::presenceType::Required, false));
        current_scope->add_symbol(var_name, v);
        return v;
    }

    ASR::symbol_t* declare_implicit_variable2(const Location &loc,
            const std::string &var_name, ASR::intentType intent,
            ASR::ttype_t *type, ASR::expr_t* value = nullptr) {
        if ( value != nullptr ) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *value_type = ASRUtils::expr_type(value);
            value = b.t2t(value, value_type, type);
        }
        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
            current_scope, s2c(al, var_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), intent, value, value != nullptr ? ASRUtils::expr_value(value) : value,
            ASR::storage_typeType::Default, type, nullptr,
            current_procedure_abi_type, ASR::Public,
            ASR::presenceType::Required, false));
        current_scope->add_symbol(var_name, v);
        return v;
    }


    ASR::asr_t* resolve_variable(const Location &loc, const std::string &var_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *v = scope->resolve_symbol(var_name);
        if (compiler_options.implicit_typing) {
            if (!in_Subroutine) {
                if (implicit_mapping.size() != 0) {
                    implicit_dictionary = implicit_mapping[get_hash(current_scope->asr_owner)];
                    if (implicit_dictionary.size() == 0 && is_implicit_interface) {
                        implicit_dictionary = implicit_mapping[get_hash(implicit_interface_parent_scope->asr_owner)];
                    }
                }
            }
        }

        if (var_name == "c_null_ptr" || var_name == "c_null_funptr") {
            // Check if c_null_ptr is imported from iso_c_binding (intrinsic module)
            if (v && ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
                std::string m_name = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_module_name;
                if (startswith(m_name, "lfortran_intrinsic")) {
                    ASR::ttype_t *type_ = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
                    tmp = ASR::make_PointerNullConstant_t(al, loc, type_);
                    return tmp;
                }
            }
        }
        if (!v) {
            if (compiler_options.implicit_typing) {
                std::string first_letter = std::string(1,var_name[0]);
                if (implicit_dictionary.find(first_letter) != implicit_dictionary.end()) {
                    ASR::ttype_t *t = implicit_dictionary[first_letter];
                    if (t == nullptr) {
                        diag.semantic_error_label("Variable '" + var_name
                            + "' is not declared", {loc},
                            "'" + var_name + "' is undeclared");
                        throw SemanticAbort();
                    }
                    ASR::intentType intent;
                    if (std::find(current_procedure_args.begin(),
                            current_procedure_args.end(), var_name) !=
                            current_procedure_args.end()) {
                        intent = ASRUtils::intent_unspecified;
                    } else {
                        intent = ASRUtils::intent_local;
                    }
                    v = declare_implicit_variable2(loc, var_name, intent, t);
                } else {
                    ASR::intentType intent;
                    if (std::find(current_procedure_args.begin(),
                            current_procedure_args.end(), var_name) !=
                            current_procedure_args.end()) {
                        intent = ASRUtils::intent_unspecified;
                    } else {
                        intent = ASRUtils::intent_local;
                    }
                    v = declare_implicit_variable(loc, var_name, intent);
                }
            } else if (_processing_dimensions && !v) {
                // Declare an implicit variable with integer type
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
                SetChar variable_dependencies_vec;
                variable_dependencies_vec.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
                v = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
                    current_scope, s2c(al, var_name), variable_dependencies_vec.p,
                    variable_dependencies_vec.size(), ASRUtils::intent_unspecified, nullptr, nullptr,
                    ASR::storage_typeType::Default, type, nullptr,
                    current_procedure_abi_type, ASR::Public,
                    ASR::presenceType::Required, false));
                pre_declared_array_dims[var_name] = 1;
                current_scope->add_symbol(var_name, v);
            } else {
                diag.semantic_error_label("Variable '" + var_name
                    + "' is not declared", {loc},
                    "'" + var_name + "' is undeclared");
                throw SemanticAbort();
            }
        }

        // The symbol `v` must be a Variable
        ASR::symbol_t *vpast = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Variable_t>(*vpast) || ASR::is_a<ASR::Function_t>(*vpast)) {
            return ASR::make_Var_t(al, loc, v);
        } else {
            std::string sym_type = ASRUtils::symbol_type_name(*vpast);
            diag.diagnostics.push_back(diag::Diagnostic(
                "Symbol '" + var_name + "' must be a variable or a procedure",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("cannot use a '" + sym_type + "' as a variable", {loc}),
                    diag::Label("'" + var_name + "' declared as a '" + sym_type + "' here", {vpast->base.loc}, false),
                }));
            throw SemanticAbort();
        }
    }

    std::string create_getter_function(const Location& loc, ASR::symbol_t* end_sym) {
        SymbolTable* current_scope_copy = current_scope;
        SymbolTable* parent_scope = current_scope->parent; // use parent scope instead of local to avoid unintended wrong manipulation by nested_vars pass.
        ASRUtils::ASRBuilder b(al, loc);
        current_scope = al.make_new<SymbolTable>(parent_scope);

        std::string func_name = parent_scope->get_unique_name("__lcompilers_get_" + std::string(ASRUtils::symbol_name(end_sym)));
        if (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym)) {
            ASRUtils::SymbolDuplicator sd(al);
            sd.duplicate_symbol(end_sym, current_scope);
            end_sym = current_scope->resolve_symbol(ASRUtils::symbol_name(end_sym));
        }
        ASR::expr_t* return_var_expr = b.Variable(current_scope, func_name, ASRUtils::symbol_type(end_sym),
                                ASR::intentType::ReturnVar);
        // populate body
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);
        body.push_back(al, b.Assignment( return_var_expr, b.Var(end_sym) ));

        ASR::symbol_t* func_sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(al, loc,
                                current_scope, s2c(al, func_name), nullptr, 0, nullptr, 0, body.p, body.n,
                                return_var_expr, ASR::abiType::Source,
                                ASR::accessType::Public, ASR::deftypeType::Implementation,
                                nullptr, false, true, false, false, false, nullptr, 0, false, false, false, nullptr));
        current_scope = current_scope_copy;
        parent_scope->add_symbol(func_name,func_sym);

        return func_name;
    }

    ASR::expr_t* get_transformed_function_call(ASR::symbol_t* end_sym) {
        /*
            module y
                contains
                subroutine a(cs)
                    use xx
                    real, dimension(nx), intent(in) :: cs
                end subroutine
            end module y

            transform to:
            module y
                contains
                pure integer function __lcompilers_get_nx()
                use xx
                get_nx = nx
                end function

                subroutine a(cs)
                    use xx
                    real, dimension(__lcompilers_get_nx()), intent(in) :: cs
                end subroutine
            end module y
        */
        SymbolTable* parent_scope = current_scope->parent;
        std::string func_name = create_getter_function(end_sym->base.loc, end_sym);
        ASR::symbol_t* getter_func_sym = parent_scope->resolve_symbol(LCompilers::s2c(al,func_name));
        current_function_dependencies.push_back(al,s2c(al, func_name));

        ASR::expr_t* func_call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, getter_func_sym->base.loc,
                                getter_func_sym, getter_func_sym, nullptr, 0, ASRUtils::symbol_type(end_sym), nullptr, nullptr, false));
        return func_call;
    }

    ASR::expr_t* convert_integer_binop_to_function_call(ASR::expr_t* end, bool is_argument) {
        ASR::IntegerBinOp_t* end_bin_op = ASR::down_cast<ASR::IntegerBinOp_t>(end);
        ASR::expr_t* left = end_bin_op->m_left;
        ASR::expr_t* right = end_bin_op->m_right;
        if (ASR::is_a<ASR::Var_t>(*left) && !ASR::is_a<ASR::Var_t>(*right)) {
            /*
            Handle expressions like `nx + a` where `a` can either be
            an integer or an `IntegerBinOp_t`.

            Examples - nx + 1 and nx + ny * nz
            */
            ASR::Var_t* end_var = ASR::down_cast<ASR::Var_t>(left);
            ASR::symbol_t* end_sym = end_var->m_v;
            SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
            if (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym) ||
                (symbol_scope->counter != current_scope->counter && is_argument &&
                ASRUtils::expr_value(end) == nullptr) ) {
                left = get_transformed_function_call(end_sym);
            }
            if (ASR::is_a<ASR::IntegerBinOp_t>(*right)) {
                right = convert_integer_binop_to_function_call(right, is_argument);
            }
        } else if (!ASR::is_a<ASR::Var_t>(*left) && ASR::is_a<ASR::Var_t>(*right)) {
            /*
            Handle expressions like `a + nx` where `a` can either be
            an integer or an `IntegerBinOp_t`.

            Examples - 1 + nx and ny * nz + nx
            */
            ASR::Var_t* end_var = ASR::down_cast<ASR::Var_t>(right);
            ASR::symbol_t* end_sym = end_var->m_v;
            SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
            if (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym) ||
                (symbol_scope->counter != current_scope->counter && is_argument &&
                ASRUtils::expr_value(end) == nullptr) ) {
                right = get_transformed_function_call(end_sym);
            }
            if (ASR::is_a<ASR::IntegerBinOp_t>(*left)) {
                left = convert_integer_binop_to_function_call(left, is_argument);
            }
        } else if (ASR::is_a<ASR::Var_t>(*left) && ASR::is_a<ASR::Var_t>(*right)) {
            // Handle expressions like `nx + ny` where both `nx` and `ny` are
            // external variables.
            ASR::symbol_t* first_end_sym = ASR::down_cast<ASR::Var_t>(left)->m_v;
            ASR::symbol_t* second_end_sym = ASR::down_cast<ASR::Var_t>(right)->m_v;
            SymbolTable* first_symbol_scope = ASRUtils::symbol_parent_symtab(first_end_sym);
            SymbolTable* second_symbol_scope = ASRUtils::symbol_parent_symtab(second_end_sym);
            if (ASR::is_a<ASR::ExternalSymbol_t>(*first_end_sym) ||
                (first_symbol_scope->counter != current_scope->counter && is_argument &&
                ASRUtils::expr_value(end) == nullptr) ) {
                left = get_transformed_function_call(first_end_sym);
            }
            if (ASR::is_a<ASR::ExternalSymbol_t>(*second_end_sym) ||
                (second_symbol_scope->counter != current_scope->counter && is_argument &&
                ASRUtils::expr_value(end) == nullptr) ) {
                right = get_transformed_function_call(second_end_sym);
            }
        } else {
            /*
            Handle expressions like `a + b` where both `a` and `b` can either be
            an integer or an `IntegerBinOp_t`.

            Examples - 1 + 2 and 1 + nx + ny
            */
            if (ASR::is_a<ASR::IntegerBinOp_t>(*left)) {
                left = convert_integer_binop_to_function_call(left, is_argument);
            }
            if (ASR::is_a<ASR::IntegerBinOp_t>(*right)) {
                right = convert_integer_binop_to_function_call(right, is_argument);
            }
        }
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc,
            left, end_bin_op->m_op, right, end_bin_op->m_type, end_bin_op->m_value));
    }

    void dimension_attribute_error_check(ASR::expr_t* dim_expr) {
        bool error = false;
        if (ASR::is_a<ASR::Var_t>(*dim_expr)) {
            ASR::Var_t* dim_expr_var = ASR::down_cast<ASR::Var_t>(dim_expr);
            ASR::symbol_t* dim_expr_sym = dim_expr_var->m_v;
            SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(dim_expr_sym);
            if (ASR::is_a<ASR::Variable_t>(*dim_expr_sym)) {
                ASR::Variable_t* dim_expr_variable = ASR::down_cast<ASR::Variable_t>(dim_expr_sym);

                if (dim_expr_variable->m_type->type != ASR::ttypeType::Integer) {
                    error = true;
                } else {

                    if ((dim_expr_variable->m_storage != ASR::storage_typeType::Parameter) && !(in_Subroutine) && (symbol_scope->counter == current_scope->counter)) {
                        error = true;
                    }
                }
            }
        } else {

            ASR::ttype_t* dim_expr_type = ASRUtils::expr_type(dim_expr);

            if (dim_expr_type->type != ASR::ttypeType::Integer) {
                error = true;
            }
        }

        if(error){
            diag.add(Diagnostic(
                "Expecting a scalar integer or parameter annotated integer variable ",
                Level::Error, Stage::Semantic, {
                    Label("",{dim_expr->base.loc})
                }));
            throw SemanticAbort();
        }
    }


    void dimension_variable(AST::var_sym_t const & s, const Location& loc) {
	std::string sym = to_lower(s.m_name);
	ASR::symbol_t *get_sym = current_scope->get_symbol(sym);
	// get actual variable from SymTab, not the current line
	if (get_sym == nullptr) {
	    if (compiler_options.implicit_typing) {
		ASR::intentType intent;
		if (std::find(current_procedure_args.begin(),
			      current_procedure_args.end(), sym) !=
		    current_procedure_args.end()) {
		    intent = ASRUtils::intent_unspecified;
		} else {
		    intent = ASRUtils::intent_local;
		}
		get_sym = declare_implicit_variable2(s.loc, sym, intent, implicit_dictionary[std::string(1,sym[0])]);
	    } else {
		if (symbols_having_only_attributes_without_type.find(sym) == symbols_having_only_attributes_without_type.end()) {
	            ASR::intentType intent;
	            ASR::abiType abi;
	            if (std::find(current_procedure_args.begin(),
	                    current_procedure_args.end(), sym) !=
	                    current_procedure_args.end()) {
	                intent = ASRUtils::intent_unspecified;
	                abi = current_procedure_abi_type;
	            } else {
	                intent = ASRUtils::intent_local;
	                abi = ASR::abiType::Source;
	            }
	            get_sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc, current_scope,
	                                                    s.m_name, nullptr, 0, intent, nullptr,
	                                                    nullptr, ASR::storage_typeType::Default, nullptr, nullptr,
	                                                    abi, ASR::accessType::Public, ASR::presenceType::Required,
	                                                    false, false, false));
	        } else {
	            get_sym = symbols_having_only_attributes_without_type[sym];
	        }
	    }
	}

	if (ASR::is_a<ASR::Variable_t>(*get_sym)) {
	    bool is_compile_time = false;
	    Vec<ASR::dimension_t> dims;
	    dims.reserve(al, 0);
	    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(get_sym);
	    bool is_char_type = false;
            if ( v->m_type ) {
                is_char_type = ASR::is_a<ASR::String_t>(*v->m_type);
            }
	    process_dims(al, dims, s.m_dim, s.n_dim, is_compile_time, is_char_type, false, s.m_name);

	    bool is_star_dimension = false;

	    if (s.n_dim > 0) {
		is_star_dimension = (s.m_dim[0].m_end_star == AST::dimension_typeType::DimensionStar);
	    }

	    if (v->m_type && ASRUtils::is_array(v->m_type)) {
		/* You can't specify an attribute such as DIMENSION more than once in a scoping
		   unit (so sayth F2023, 8.5.1 C815). There are really four ways to dimension a variable:
		     1a. In a _type-decl_ DIMENSION attribute;
		     1b. In a _type-decl_ _entity-decl_ _array-spec_ (overides #1a)
		     2. In a DIMENSION statement
		     3. In a COMMON statement
		   Cases 1a and 1b are handled in the _type-decl_ visitor, but that visitor should
		   also check for existence of other attribute specifications via attribute statements.
		   This check handles cases 2 and 3.
		*/
                diag.add(diag::Diagnostic(
                    "Duplicate DIMENSION attribute specified",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {s.loc})}));
                throw SemanticAbort();
	    }

	    if ( v->m_type ) {
            	if (!ASRUtils::ttype_set_dimensions(&(v->m_type), dims.data(), dims.size(), al,
						    ASR::abiType::Source, false, is_star_dimension)) {
	            diag.add(Diagnostic(
			         "Cannot set dimension for variable of non-numerical type",
			         Level::Error, Stage::Semantic, {
			             Label("",{loc})
			         }));
	            throw SemanticAbort();
            	}
            	SetChar variable_dependencies_vec;
            	variable_dependencies_vec.reserve(al, 1);
            	ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, v->m_type,
                                			v->m_symbolic_value, v->m_value);
            	v->m_dependencies = variable_dependencies_vec.p;
            	v->n_dependencies = variable_dependencies_vec.size();
            } else {
                v->m_type = ASRUtils::make_Array_t_util(al, loc, nullptr, dims.p, dims.size(), ASR::abiType::Source, false, ASR::array_physical_typeType::DescriptorArray, false, is_star_dimension);
                symbols_having_only_attributes_without_type[sym] = get_sym;
            }
	} else {
	    diag.add(Diagnostic(
			 "Cannot attribute non-variable type with dimension",
			 Level::Error, Stage::Semantic, {
			     Label("",{loc})
			 }));
	    throw SemanticAbort();
	}
    }

    bool is_funcCall_to_unresolved_genereicProcedure(AST::expr_t* expr){
        return AST::is_a<AST::FuncCallOrArray_t>(*expr) &&
            (generic_procedures.find(
                AST::down_cast<AST::FuncCallOrArray_t>(expr)->m_func)
            != generic_procedures.end());
    }

    void process_dims(Allocator &al, Vec<ASR::dimension_t> &dims,
        AST::dimension_t *m_dim, size_t n_dim, bool &is_compile_time,
        bool is_char_type, bool is_argument, char* var_name) {  
        LCOMPILERS_ASSERT(dims.size() == 0);
        is_compile_time = false;
        _processing_dimensions = true;
        dims.reserve(al, n_dim);
        for (size_t i=0; i<n_dim; i++) {
            ASR::dimension_t dim_dummy; dims.push_back(al, dim_dummy);
            ASR::dimension_t &dim  = const_cast<ASR::dimension_t&>(dims[dims.size()-1]);
            dim.m_length = nullptr; dim.m_start = nullptr;
            dim.loc = m_dim[i].loc;
            if (m_dim[i].m_start) {
                if(is_funcCall_to_unresolved_genereicProcedure(m_dim[i].m_start)){
                    postponed_genericProcedure_calls_vec.emplace_back(&dim.m_start,
                        current_scope, m_dim[i].m_start, var_name, 
                        [this](ASR::expr_t* start){dimension_attribute_error_check(start);});
                    dim.m_start = nullptr;
                } else {
                    this->visit_expr(*m_dim[i].m_start);
                    dim.m_start = ASRUtils::EXPR(tmp);
                    dimension_attribute_error_check(dim.m_start);
                }
            } else {
                dim.m_start = nullptr;
            }
            if (m_dim[i].m_end) {
                ASR::expr_t* end{};
                if(is_funcCall_to_unresolved_genereicProcedure(m_dim[i].m_end)){ // Delay
                    postponed_genericProcedure_calls_vec.emplace_back(&dim.m_length,
                        current_scope, m_dim[i].m_end, var_name, 
                        [this](ASR::expr_t* start){dimension_attribute_error_check(start);});
                    dim.m_length = nullptr;
                } else {
                    this->visit_expr(*m_dim[i].m_end);
                    end = ASRUtils::EXPR(tmp);
                    dimension_attribute_error_check(end);
                    if (ASR::is_a<ASR::Var_t>(*end)) {
                        ASR::Var_t* end_var = ASR::down_cast<ASR::Var_t>(end);
                        ASR::symbol_t* end_sym = end_var->m_v;
                        SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
                        if ((is_argument || ASRUtils::expr_value(end) == nullptr) &&
                            (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym) ||
                            (symbol_scope->counter != current_scope->counter && is_argument &&
                            ASRUtils::expr_value(end) == nullptr)) ) {
                                end = get_transformed_function_call(end_sym);
                        }
                    } else if(ASR::is_a<ASR::IntegerBinOp_t>(*end)) {
                        end = convert_integer_binop_to_function_call(end, is_argument);
                    }
                    dim.m_length = ASRUtils::compute_length_from_start_end(al, dim.m_start,
                                        end);
                }
            } else {
                dim.m_length = nullptr;
            }
            if ( !dim.m_start && !dim.m_length ) {
                is_compile_time = true;
            }
            if (m_dim[i].m_end_star && is_char_type) {
                continue;
            }
        }
        _processing_dimensions = false;
    }

    ASR::accessType get_asr_simple_attr(AST::simple_attributeType simple_attr) {
        ASR::accessType access_type = ASR::accessType::Public;
        switch( simple_attr ) {
            case AST::simple_attributeType::AttrPublic: {
                access_type = ASR::accessType::Public;
                break;
            }
            case AST::simple_attributeType::AttrPrivate: {
                access_type = ASR::accessType::Private;
                break;
            }
            default:
                LCOMPILERS_ASSERT(false);
        }
        return access_type;
    }

    void visit_Format(const AST::Format_t &x) {
        format_statements[x.m_label] = x.m_fmt;
        tmp = nullptr;
    }

    void visit_Include(const AST::Include_t &x) {
        diag.semantic_error_label(
            "Include statement is not implemented at the AST level yet. You have to run LFortran with prescanning which can handle include statements.",
            {x.base.base.loc},
            "Enable prescanner to handle this"
        );
        tmp = nullptr;
    }

    ASR::asr_t* create_StructInstanceMember(ASR::expr_t* target, ASR::Variable_t* target_var) {
        uint64_t hash = get_hash((ASR::asr_t*) target_var);
        std::string target_var_name = target_var->m_name;
        SymbolTable* scope = target_var->m_parent_symtab;
        if (common_variables_hash.find(hash) != common_variables_hash.end()) {
            ASR::symbol_t* curr_struct = common_variables_hash[hash];
            ASR::Struct_t *struct_type = ASR::down_cast<ASR::Struct_t>(curr_struct);
            std::string ext_sym_name = std::string(struct_type->m_name);
            std::string module_name = "file_common_block_" + std::string(struct_type->m_name);
            ASR::symbol_t* ext_sym_struct = scope->resolve_symbol(ext_sym_name);
            if (!ext_sym_struct) {
                ext_sym_struct = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al, curr_struct->base.loc, scope,
                                                struct_type->m_name, curr_struct, s2c(al, module_name), nullptr, 0, struct_type->m_name, ASR::accessType::Public));
                scope->add_symbol(ext_sym_name, ext_sym_struct);
            }

            SymbolTable* module_scope = ASR::down_cast<ASR::Struct_t>(curr_struct)->m_symtab->parent;
            std::string struct_var_name = "struct_instance_"+std::string(struct_type->m_name);
            ASR::symbol_t* module_var_sym = module_scope->resolve_symbol(struct_var_name);
            ASR::symbol_t* struct_sym = scope->resolve_symbol(struct_var_name);
            if (!struct_sym) {
                struct_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al, curr_struct->base.loc, scope,
                                                s2c(al, struct_var_name), module_var_sym, s2c(al, module_name), nullptr, 0, s2c(al, struct_var_name), ASR::accessType::Public));
                scope->add_symbol(struct_var_name, struct_sym);
            }

            ASR::asr_t* struct_var_ = ASR::make_Var_t(al, target_var->base.base.loc, struct_sym);

            std::string member_name = "1_"+std::string(struct_type->m_name)+"_"+target_var_name;
            ASR::symbol_t* member_sym = scope->resolve_symbol(member_name);
            if (!member_sym) {

                member_sym = ASR::down_cast<ASR::symbol_t>(make_ExternalSymbol_t(al, target_var->base.base.loc, scope, s2c(al, member_name),
                                                        struct_type->m_symtab->resolve_symbol(target_var_name), s2c(al, ext_sym_name), nullptr, 0, s2c(al, target_var_name), ASR::accessType::Public));
                scope->add_symbol(member_name, member_sym);
            }

            ASR::asr_t* new_target = ASR::make_StructInstanceMember_t(al, target->base.loc, ASRUtils::EXPR(struct_var_),
                member_sym, ASRUtils::fix_scoped_type(al, ASRUtils::symbol_type(struct_type->m_symtab->resolve_symbol(target_var_name)),
                    current_scope), nullptr);

            return new_target;
        } else {
            return nullptr;
        }
    }

    ASR::expr_t* replace_with_common_block_variables(ASR::expr_t* target) {
        if (!target) {
            return target;
        }
        if (ASR::is_a<ASR::Var_t>(*target)) {
            ASR::symbol_t* target_var_sym = ASR::down_cast<ASR::Var_t>(target)->m_v;
            if (ASR::is_a<ASR::Variable_t>(*(target_var_sym))) {
                ASR::Variable_t* target_var = ASR::down_cast<ASR::Variable_t>(target_var_sym);
                ASR::asr_t* new_target = create_StructInstanceMember(target, target_var);
                if (new_target) {
                    return ASRUtils::EXPR(new_target);
                }
            }
        } else if (ASR::is_a<ASR::ArrayItem_t>(*target)) {
            ASR::ArrayItem_t* target_array_item = ASR::down_cast<ASR::ArrayItem_t>(target);
            ASR::expr_t* target_array = target_array_item->m_v;
            if (ASR::is_a<ASR::Var_t>(*target_array)) {
                ASR::symbol_t* target_array_var_sym = ASR::down_cast<ASR::Var_t>(target_array)->m_v;
                if (ASR::is_a<ASR::Variable_t>(*(target_array_var_sym))) {
                    ASR::Variable_t* target_array_var = ASR::down_cast<ASR::Variable_t>(target_array_var_sym);
                    ASR::asr_t* new_target_array = create_StructInstanceMember(target_array, target_array_var);
                    if (new_target_array) {
                        ASR::down_cast<ASR::ArrayItem_t>(target)->m_v = ASRUtils::EXPR(new_target_array);
                        return target;
                    }
                }
            }
        }
        return target;
    }

    bool check_equal_value(AST::expr_t** values, size_t n_values) {
        this->visit_expr(*values[0]);
        ASR::expr_t* value = ASRUtils::EXPR(tmp);
        ASR::expr_t* expression_value = ASRUtils::expr_value(value);

        for (size_t i=1; i<n_values; i++) {
            this->visit_expr(*values[i]);
            ASR::expr_t* value_ = ASRUtils::EXPR(tmp);
            ASR::expr_t* expression_value_ = ASRUtils::expr_value(value_);
            if (!ASRUtils::expr_equal(expression_value, expression_value_)) {
                return false;
            }
        }

        return true;
    }

    void handle_array_data_stmt(const AST::DataStmt_t &x, AST::DataStmtSet_t* a, ASR::ttype_t* obj_type, ASR::expr_t* object, size_t &curr_value) {
        ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(obj_type);
        if (check_equal_value(a->m_value, a->n_value)) {
            /*
                Case:
                integer :: x(5)
                data x / 5*1 /


                Here the data statement gets expanded to:
                integer :: x(5)
                x = 1


                Because for arrays of larger size, the current implementation
                of data statement is not efficient.
            */
            this->visit_expr(*a->m_value[curr_value++]);
            ASR::expr_t* value = ASRUtils::EXPR(tmp);
            if (!ASRUtils::types_equal(ASRUtils::expr_type(value), array_type->m_type)) {
                diag.add(Diagnostic(
                    "Type mismatch during data initialization",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            ASR::expr_t* expression_value = ASRUtils::expr_value(value);
            if (expression_value) {
                ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, object, expression_value);
                ASR::stmt_t* assignment_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, x.base.base.loc,
                                            object, expression_value, nullptr, compiler_options.po.realloc_lhs));
                current_body->push_back(al, assignment_stmt);
            } else {
                diag.add(Diagnostic(
                    "The value in data must be a constant",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        } else {
            Vec<ASR::expr_t*> body;
            body.reserve(al, a->n_value);
            int size_of_array = 0;
            if (ASR::is_a<ASR::ArraySection_t>(*object)) {
                size_of_array = ASRUtils::get_fixed_size_of_ArraySection(ASR::down_cast<ASR::ArraySection_t>(object));
                object = ASR::down_cast<ASR::ArraySection_t>(object)->m_v;
            } else {
                size_of_array = ASRUtils::get_fixed_size_of_array(array_type->m_dims, array_type->n_dims);
            }
            if (size_of_array == -1) {
                throw LCompilersException("ICE: Array size could not be computed");
            }
            curr_value += size_of_array;
            for (int j=0; j < size_of_array; j++) {
                this->visit_expr(*a->m_value[j]);
                ASR::expr_t* value = ASRUtils::EXPR(tmp);
                if (!ASRUtils::types_equal(ASRUtils::expr_type(value), array_type->m_type)) {
                    diag.add(Diagnostic(
                        "Type mismatch during data initialization",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
                ASR::expr_t* expression_value = ASRUtils::expr_value(value);
                if (expression_value) {
                    body.push_back(al, expression_value);
                } else {
                    diag.add(Diagnostic(
                        "The value in data must be a constant",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }

            }
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t dim; dim.m_length = nullptr; dim.m_start = nullptr;
            dim.loc = x.base.base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
            ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int_type));
            ASR::expr_t* x_n_args = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                a->n_value, int_type));
            dim.m_start = one;
            dim.m_length = x_n_args;
            dims.push_back(al, dim);
            obj_type = ASRUtils::duplicate_type(al, obj_type, &dims);
            tmp = ASRUtils::make_ArrayConstructor_t_util(al, x.base.base.loc, body.p,
                body.size(), obj_type, ASR::arraystorageType::ColMajor);
            ASR::Variable_t* v2 = nullptr;
            if (ASR::is_a<ASR::StructInstanceMember_t>(*object)) {
                ASR::StructInstanceMember_t *mem = ASR::down_cast<ASR::StructInstanceMember_t>(object);
                v2 = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(mem->m_m));
            } else {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(object);
                v2 = ASR::down_cast<ASR::Variable_t>(v->m_v);
            }
            v2->m_value = ASRUtils::EXPR(tmp);
            v2->m_symbolic_value = ASRUtils::EXPR(tmp);
            SetChar var_deps_vec;
            var_deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, var_deps_vec, v2->m_type,
                v2->m_symbolic_value, v2->m_value);
            v2->m_dependencies = var_deps_vec.p;
            v2->n_dependencies = var_deps_vec.size();
        }
    }

    void handle_implied_do_loop_data_stmt(const AST::DataStmt_t &data_stmt,
                                          AST::DataStmtSet_t *data_stmt_set,
                                          ASR::expr_t* implied_do_loop_expr,
                                          size_t &value_index) {
        ASR::ImpliedDoLoop_t *implied_do_loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_do_loop_expr);

        ASR::expr_t* loop_start_expr = implied_do_loop->m_start;
        ASR::expr_t* loop_end_expr = implied_do_loop->m_end;
        ASR::expr_t* loop_increment_expr = implied_do_loop->m_increment;
        ASR::expr_t* start_expr_value = ASRUtils::expr_value(loop_start_expr);
        ASR::expr_t* end_expr_value = ASRUtils::expr_value(loop_end_expr);
        if (!start_expr_value) {
            diag.add(Diagnostic(
                "The start variable of the data implied do loop must be constants",
                Level::Error, Stage::Semantic, {
                    Label("",{loop_start_expr->base.loc})
                }));
            throw SemanticAbort();
        }
        if (!end_expr_value) {
            diag.add(Diagnostic(
                "The end variable of the data implied do loop must be constants",
                Level::Error, Stage::Semantic, {
                    Label("",{loop_end_expr->base.loc})
                }));
            throw SemanticAbort();
        }

        ASR::ttype_t *integer_type = ASRUtils::TYPE(
                                        ASR::make_Integer_t(al, data_stmt.base.base.loc,
                                        compiler_options.po.default_integer_kind)
                                    );

        if (!loop_increment_expr) {
            loop_increment_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, data_stmt.base.base.loc, 1, integer_type));
        }
        ASR::expr_t* increment_expr_value = ASRUtils::expr_value(loop_increment_expr);
        if (!increment_expr_value) {
            diag.add(Diagnostic(
                "The increment variable of the data implied do loop must be a constant",
                Level::Error, Stage::Semantic, {
                    Label("",{loop_increment_expr->base.loc})
                }));
            throw SemanticAbort();
        }

        int64_t loop_start = ASR::down_cast<ASR::IntegerConstant_t>(start_expr_value)->m_n;
        int64_t loop_end = ASR::down_cast<ASR::IntegerConstant_t>(end_expr_value)->m_n;
        int64_t loop_increment = ASR::down_cast<ASR::IntegerConstant_t>(increment_expr_value)->m_n;

        ASRUtils::ExprStmtDuplicator exprDuplicator(al);
        for (int64_t loop_var = loop_start; loop_var <= loop_end; loop_var += loop_increment) {
            for (size_t value_index_in_loop = 0; value_index_in_loop < implied_do_loop->n_values; value_index_in_loop++) {
                ASR::expr_t* duplicatedExpr = exprDuplicator.duplicate_expr(
                                                implied_do_loop->m_values[value_index_in_loop]);
                ASR::ArrayItem_t* array_item_expr = ASR::down_cast<ASR::ArrayItem_t>(duplicatedExpr);
                for (size_t arg_index = 0; arg_index < array_item_expr->n_args; arg_index++) {
                    ASR::array_index_t array_index = array_item_expr->m_args[arg_index];
                    ASR::expr_t* index_expr = array_index.m_right;
                    if (ASR::is_a<ASR::Var_t>(*index_expr)) {
                        array_item_expr->m_args[arg_index].m_right = ASRUtils::EXPR(
                                                                    ASR::make_IntegerConstant_t(al,
                                                                        implied_do_loop->base.base.loc,
                                                                        loop_var, integer_type)
                                                                    );
                    }
                }
                ASR::expr_t* target = ASRUtils::EXPR((ASR::asr_t*) array_item_expr);
                this->visit_expr(*data_stmt_set->m_value[value_index++]);
                ASR::expr_t* value = ASRUtils::EXPR(tmp);
                ASRUtils::make_ArrayBroadcast_t_util(al, data_stmt.base.base.loc, target, value);
                ASR::stmt_t* assignStatement = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, data_stmt.base.base.loc,
                                                                                    target, value, nullptr, compiler_options.po.realloc_lhs)
                                                             );
                LCOMPILERS_ASSERT(current_body != nullptr)
                current_body->push_back(al, assignStatement);
            }
        }
    }

    void handle_scalar_data_stmt(const AST::DataStmt_t &x, AST::DataStmtSet_t *a, size_t i, size_t &j) {
        this->visit_expr(*a->m_object[i]);
        ASR::expr_t* object = ASRUtils::EXPR(tmp);
        this->visit_expr(*a->m_value[j++]);
        ASR::expr_t* value = ASRUtils::EXPR(tmp);
        // The parser ensures object is a TK_NAME
        // The `visit_expr` ensures it resolves as an expression
        // which must be a `Var_t` pointing to a `Variable_t`,
        // so no checks are needed:
        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &value,
                                ASRUtils::expr_type(value), ASRUtils::expr_type(object), diag);
        ASR::expr_t* expression_value = ASRUtils::expr_value(value);
        if (!expression_value) {
            diag.add(Diagnostic(
                "The value in data must be a constant",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*object)) {
            ASR::StructInstanceMember_t *mem = ASR::down_cast<ASR::StructInstanceMember_t>(object);
            ASR::Variable_t* v2 = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(mem->m_m));
            v2->m_value = expression_value;
            v2->m_symbolic_value = expression_value;
            SetChar var_deps_vec;
            var_deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, var_deps_vec, v2->m_type,
                v2->m_symbolic_value, v2->m_value);
            v2->m_dependencies = var_deps_vec.p;
            v2->n_dependencies = var_deps_vec.size();
            ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, object, expression_value);
            ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
                        object->base.loc, object, expression_value, nullptr, compiler_options.po.realloc_lhs));
            LCOMPILERS_ASSERT(current_body != nullptr)
            current_body->push_back(al, assign_stmt);
        } else if (ASR::is_a<ASR::Var_t>(*object)) {
            // This is the following case:
            // y / 2 /
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(object);
            ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v->m_v);
            v2->m_value = expression_value;
            v2->m_symbolic_value = expression_value;
            SetChar var_deps_vec;
            var_deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, var_deps_vec, v2->m_type,
                v2->m_symbolic_value, v2->m_value);
            v2->m_dependencies = var_deps_vec.p;
            v2->n_dependencies = var_deps_vec.size();
            ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, object, expression_value);
            ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
                        object->base.loc, object, expression_value, nullptr, compiler_options.po.realloc_lhs));
            LCOMPILERS_ASSERT(current_body != nullptr)
            current_body->push_back(al, assign_stmt);
        } else if (ASR::is_a<ASR::ArrayItem_t>(*object)) {
            // This is the following case:
            // x(2) / 2 /
            // We create an assignment node and insert into the current body.
            // i.e., x(2) = 2.
            // Note: this will only work if the data statement is
            // above the place where it is being used, otherwise it
            // won't work correctly
            // To fix that, we would have to iterate over data statements first
            // but we can fix that later.
            ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, object, expression_value);
            ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
                        object->base.loc, object, expression_value, nullptr, compiler_options.po.realloc_lhs));
            LCOMPILERS_ASSERT(current_body != nullptr)
            current_body->push_back(al, assign_stmt);
        } else {
            diag.add(Diagnostic(
                "The variable (object) type is not supported (only variables and array items are supported so far)",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
    }


    void visit_DataStmt(const AST::DataStmt_t &x) {
        // The DataStmt is a statement, so it occurs in the BodyVisitor.
        // We add its contents into the symbol table here. This visitor
        // could probably be in either the CommonVisitor or the BodyVisitor.

        // This outer loop is iterating over sections in the data statement,
        // for example in the following we have three items:
        //   data x / 1.0, 2.0 /, a, b / 1.0, 2.0 /, c / 1.0 /
        for (size_t i=0; i < x.n_items; i++) {
            AST::DataStmtSet_t *a = AST::down_cast<AST::DataStmtSet_t>(x.m_items[i]);
            // Now we are dealing with just one item, there are four cases possible:
            // data x / 1, 2, 3 /       ! x must be an array
            // data x / 1 /             ! x must be a scalar (integer)
            // data x, y, z / 1, 2, 3 / ! x, y, z must be a scalar (integer)
            // data x, (y(i),i = 1,3) /1, 2, 3, 4/ ! x must be a scalar (integer) and y must be an array
            if (a->n_object != a->n_value) {
                // This is the first case:
                // data x / 1, 2, 3 /       ! x must be an array
                if (a->n_object == 1) {
                    this->visit_expr(*a->m_object[0]);
                    ASR::expr_t* object = ASRUtils::EXPR(tmp);
                    ASR::ttype_t* obj_type = ASRUtils::expr_type(object);
                    if (ASRUtils::is_array(obj_type)) { // it is an array
                        size_t curr_value = 0;
                        handle_array_data_stmt(x, a, obj_type, object, curr_value);
                    } else if (ASR::is_a<ASR::ImpliedDoLoop_t>(*object)) {
                        /*
                            case: DATA (a(i),i=1,5) /1.0, 2.0, 3*0.0/
                            Here the implied do loop gets expanded to:
                            DATA a(1), a(2), a(3), a(4), a(5) /1.0, 2.0, 0.0, 0.0, 0.0/
                        */
                        handle_implied_do_loop_data_stmt(x, a, object, i);
                    } else {
                        diag.add(Diagnostic(
                            "There is one variable and multiple values, but the variable is not an array",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                } else {
                    // This is fourth case:
                    // data x, (y(i),i = 1,3) /1, 2, 3, 4/ ! x can be array or scalar and y must be an array
                    // TODO: check if n_objects == n_values after unrolling implied do loops and length of arrays

                    size_t curr_value = 0;

                    for (size_t j = 0; j < a->n_object; j++) {
                        this->visit_expr(*a->m_object[j]);
                        ASR::expr_t* object = ASRUtils::EXPR(tmp);
                        ASR::ttype_t* obj_type = ASRUtils::expr_type(object);
                        if (ASRUtils::is_array(obj_type)) { // it is an array
                            handle_array_data_stmt(x, a, obj_type, object, curr_value);
                        } else if (ASR::is_a<ASR::ImpliedDoLoop_t>(*object)) {
                            handle_implied_do_loop_data_stmt(x, a, object, curr_value);
                        } else {
                            handle_scalar_data_stmt(x, a, j, curr_value);
                        }
                    }
                }
            } else {
                // This is the second and third case:
                // data x / 1 /             ! x must be a scalar (integer)
                // data x, y, z / 1, 2, 3 / ! x, y, z must be a scalar (integer)

                // Note: this also happens for a case like:
                // data x(1), x(2), x(3) / 1, 2, 3 /
                for (size_t i=0;i<a->n_object;++i) {
                    // Here we are now dealing with just one variable (object)
                    // and the corresponding value at a time, such as:
                    // y / 2 /
                    // or
                    // x(2) / 2 /
                    //
                    size_t j = i;
                    handle_scalar_data_stmt(x, a, i, j);
                }
            }
        }
        tmp = nullptr;
    }

    void mark_common_blocks_as_declared() {
        for(auto &it: common_block_dictionary) {
            if(it.second.first) {
                it.second.first = false;
            }
        }
    }

    ASR::symbol_t* create_common_module(Location loc, std::string common_block_name) {
        std::string base_module_name = "file_common_block_";
        std::string base_struct_instance_name = "struct_instance_";
        std::string module_name = base_module_name + common_block_name;
        SymbolTable *parent_scope = current_scope;
        SymbolTable *global_scope = current_scope;
        // get global scope
        while(global_scope->parent) {
            global_scope = global_scope->parent;
        }
        if(!global_scope->resolve_symbol(module_name)){
            current_scope = al.make_new<SymbolTable>(global_scope);

            // create a struct
            SymbolTable* struct_scope = al.make_new<SymbolTable>(current_scope);
            ASR::symbol_t* struct_symbol = ASR::down_cast<ASR::symbol_t>(make_Struct_t(
                al, loc, struct_scope, s2c(al,common_block_name),
                nullptr, 0, nullptr, 0, nullptr, 0, ASR::abiType::Source, ASR::accessType::Public, false, false,
                nullptr, 0, nullptr, nullptr));
            current_scope->add_symbol(common_block_name, struct_symbol);

            // create a struct instance
            ASR::ttype_t* type = ASRUtils::TYPE(ASRUtils::make_StructType_t_util(al, loc, struct_symbol));
            std::string struct_var_name = base_struct_instance_name + common_block_name;
            ASR::symbol_t* struct_var_sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc, current_scope, s2c(al, struct_var_name), nullptr, 0,
                                        ASR::intentType::Local, nullptr, nullptr, ASR::storage_typeType::Default, type, nullptr,
                                        ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false));
            current_scope->add_symbol(struct_var_name, struct_var_sym);

            ASR::asr_t *tmp0 = ASR::make_Module_t(al, loc,
                        /* a_symtab */ current_scope,
                        /* a_name */ s2c(al, to_lower(module_name)),
                        nullptr,
                        0,
                        false, false);

            ASR::symbol_t* current_module_sym = ASR::down_cast<ASR::symbol_t>(tmp0);
            global_scope->add_symbol(to_lower(module_name), current_module_sym);
            current_scope = parent_scope;
            return struct_symbol;
        } else {
            ASR::symbol_t* current_module_sym = global_scope->resolve_symbol(module_name);
            return ASR::down_cast<ASR::Module_t>(current_module_sym)->m_symtab->resolve_symbol(common_block_name);
        }
    }

    void add_sym_to_struct(ASR::Variable_t* var_, ASR::Struct_t* struct_type) {
        char* var_name = var_->m_name;
        SymbolTable* struct_scope = struct_type->m_symtab;
        ASR::symbol_t* var_sym_new = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, var_->base.base.loc, struct_scope,
                        var_->m_name, var_->m_dependencies, var_->n_dependencies, var_->m_intent,
                        var_->m_symbolic_value, var_->m_value, var_->m_storage, var_->m_type,
                        var_->m_type_declaration, var_->m_abi, var_->m_access, var_->m_presence, var_->m_value_attr));
        struct_scope->add_symbol(var_name, var_sym_new);

        Vec<char*> members;
        members.reserve(al, struct_type->n_members+1);
        for (size_t i=0; i<struct_type->n_members; i++) {
            members.push_back(al, struct_type->m_members[i]);
        }
        members.push_back(al, var_name);
        struct_type->m_members = members.p;
        struct_type->n_members = members.size();
    }

    ASR::Variable_t * get_symtab_var_for_common(AST::var_sym_t const &s) {
	std::string const sym {to_lower(s.m_name)};
	ASR::symbol_t *get_sym = current_scope->get_symbol(sym);
	if (get_sym == nullptr) {
	    if (compiler_options.implicit_typing) {
		ASR::intentType intent = ASRUtils::intent_local;
		get_sym = declare_implicit_variable2(s.loc, sym, intent, implicit_dictionary[std::string(1,sym[0])]);
	    } else {
		diag.add(Diagnostic(
			     "Cannot implicitly declare variable in COMMON block",
			     Level::Error, Stage::Semantic, {
				 Label("",{s.loc})
			     }));
		throw SemanticAbort();
	    }
	}

	ASR::Variable_t* var_{nullptr};
	if (ASR::is_a<ASR::Variable_t>(*get_sym)) {
	    var_ = ASR::down_cast<ASR::Variable_t>(get_sym);
	} else {
	    diag.add(Diagnostic(
			 "Can only add variables to COMMON blocks",
			 Level::Error, Stage::Semantic, {
			     Label("",{s.loc})
			 }));
	    throw SemanticAbort();
	}
	return var_;
    }


    void populate_common_dictionary(const AST::Declaration_t &x, common_block_varsyms const & objs_by_blk) {
	for (auto const & blk : objs_by_blk) {
	    std::string const & common_block_name = blk.first;
	    ASR::symbol_t* common_block_struct_sym = create_common_module(x.base.base.loc,
									  common_block_name);
	    ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(common_block_struct_sym);
	    size_t const num_cb_var = blk.second.size();

	    auto cbd_it = common_block_dictionary.find(common_block_name);

	    if (cbd_it == common_block_dictionary.end()) {
		// Populate an entire new common_block_dictionary entry
		std::vector<ASR::expr_t*> common_block_variables;
		common_block_variables.reserve(num_cb_var);

		// Add all the block variables
		for (auto const &s : blk.second) {
		    AST::expr_t* expr = s.m_initializer;
		    this->visit_expr(*expr);
		    ASR::Variable_t* var_ = get_symtab_var_for_common(s);
		    uint64_t hash = get_hash((ASR::asr_t*) var_);
		    common_block_variables.push_back(ASRUtils::EXPR(tmp));
		    // add variable to struct
		    add_sym_to_struct(var_, struct_type);
		    common_variables_hash[hash] = common_block_struct_sym;
		}

		common_block_dictionary[common_block_name].first = true;
		common_block_dictionary[common_block_name].second.swap(common_block_variables);

	    } else {
		// There is already an entry in the dictionary for this block name
		if (cbd_it->second.first) {
		    // The block has not been marked as declared, so we just append all the variables
		    std::vector<ASR::expr_t*> & common_block_variables = cbd_it->second.second;
		    common_block_variables.reserve(common_block_variables.size() + blk.second.size());

		    for (auto const &s : blk.second) {
			AST::expr_t* expr = s.m_initializer;
			this->visit_expr(*expr);
			ASR::Variable_t* var_ = get_symtab_var_for_common(s);
			uint64_t hash = get_hash((ASR::asr_t*) var_);
			common_block_variables.push_back(ASRUtils::EXPR(tmp));
			common_variables_hash[hash] = common_block_struct_sym;
			// add variable to struct
			add_sym_to_struct(var_, struct_type);
		    }
		} else {
		    /* The block has already been declared, so we need to compare the structure of the block
		       declarations and update the structs holding the variables. */
		    std::vector<ASR::expr_t*> const & common_block_variables = cbd_it->second.second;

		    if (common_block_variables.size() != num_cb_var) {
			diag.add(Diagnostic(
				     "The number of variables in common block must be same in all programs",
				     Level::Error, Stage::Semantic, {
					 Label("",{x.base.base.loc})
				     }));
			throw SemanticAbort();
		    }

		    for (size_t i = 0; i < num_cb_var; ++i) {
			auto &expr = common_block_variables[i];
			auto &s = blk.second[i];

			ASR::Variable_t* var_ = nullptr;
			if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
			    ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(expr);
			    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(array_item->m_v);
			    var_ = ASR::down_cast<ASR::Variable_t>(var->m_v);
			} else {
			    var_ = ASRUtils::EXPR2VAR(expr);
			}

			AST::expr_t* expr_ = s.m_initializer;
			this->visit_expr(*expr_);
			ASR::Variable_t* var__ = nullptr;
			if (ASR::is_a<ASR::ArrayItem_t>(*ASRUtils::EXPR(tmp))) {
			    ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(ASRUtils::EXPR(tmp));
			    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(array_item->m_v);
			    var__ = ASR::down_cast<ASR::Variable_t>(var->m_v);
			} else {
			    var__ = ASRUtils::EXPR2VAR(ASRUtils::EXPR(tmp));
			}
			if (!ASRUtils::check_equal_type(var_->m_type, var__->m_type)) {
			    diag.add(Diagnostic(
					 "The order of variables in common block must be same in all programs",
					 Level::Error, Stage::Semantic, {
					     Label("",{x.base.base.loc})
					 }));
			    throw SemanticAbort();
			} else {
			    uint64_t hash = get_hash((ASR::asr_t*) var__);
			    common_variables_hash[hash] = common_block_struct_sym;
			}
			if (ASRUtils::is_array(var_->m_type) && ASR::is_a<ASR::ArrayItem_t>(*expr)) {
			    /*
			      Update type of original symbol
			      case:
			      program main
			      double precision x
			      common /a/ x(10)
			      end program
			    */
			    ASR::symbol_t* var_sym = current_scope->get_symbol(s2c(al, var_->m_name));
			    if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
				ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(var_sym);
				var->m_type = var_->m_type;
			    }
			}
		    }
		}
	    }
	}
    }

    template <typename T>
    void check_if_global_save_is_enabled(T &x) {
        for ( size_t i = 0; i < x.n_decl; i++ ) {
            if ( AST::is_a<AST::Declaration_t>(*x.m_decl[i]) ) {
                AST::Declaration_t* decl = AST::down_cast<AST::Declaration_t>(x.m_decl[i]);
                if ( decl->n_attributes > 0 && decl->n_syms == 0 &&
                    decl->m_trivia == nullptr &&
                    AST::is_a<AST::SimpleAttribute_t>(*decl->m_attributes[0]) ) {
                    AST::SimpleAttribute_t* attr = AST::down_cast<AST::SimpleAttribute_t>(decl->m_attributes[0]);
                    if ( attr->m_attr == AST::simple_attributeType::AttrSave ) {
                        is_global_save_enabled = true;
                        break;
                    }
                }
            }
        }
    }

    void create_external_function(std::string sym, Location loc, ASR::ttype_t* determined_type = nullptr) {
        if (compiler_options.implicit_interface) {
            bool is_subroutine = false;
            external_procedures.push_back(sym);
            ASR::symbol_t *sym_ = current_scope->resolve_symbol(sym);
            assgnd_access[sym] = ASR::accessType::Public;
            SymbolTable *parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            ASR::ttype_t *type = nullptr;
            if (sym_) {
                if (ASR::is_a<ASR::Function_t>(*sym_)) {
                    ASR::Function_t* func_sym = ASR::down_cast<ASR::Function_t>(sym_);
                    if (!func_sym->m_return_var) {
                        is_subroutine = true;
                    }
                }
                if (!is_subroutine) {
                    type = ASRUtils::symbol_type(sym_);
                }
            } else if (determined_type) {
                // if explicit type provided, give preference to it.
                type = determined_type;
            } else if (compiler_options.implicit_typing) {
                type = implicit_dictionary[std::string(1,sym[0])];
                if (!type) {
                    // There exists an `implicit none` statement, here compiler has
                    // no information about type of symbol hence keeping it real*4.
                    type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
                }
            } else {
                // Here compiler has no information about type of symbol hence keeping it real*4.
                type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
            }
            // add return var
            std::string return_var_name = sym + "_return_var_name";
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
            ASR::asr_t *return_var = nullptr;
            ASR::expr_t *to_return = nullptr;
            if (!is_subroutine) {
                return_var = ASRUtils::make_Variable_t_util(al, loc,
                    current_scope, s2c(al, return_var_name), variable_dependencies_vec.p,
                    variable_dependencies_vec.size(), ASRUtils::intent_return_var,
                    nullptr, nullptr, ASR::storage_typeType::Default, type, nullptr,
                    ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required,
                    false);
                current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
                to_return = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    ASR::down_cast<ASR::symbol_t>(return_var)));
            }
            tmp = ASRUtils::make_Function_t_util(
                al, loc,
                /* a_symtab */ current_scope,
                /* a_name */ s2c(al, sym),
                nullptr, 0,
                /* a_args */ nullptr,
                /* n_args */ 0,
                /* a_body */ nullptr,
                /* n_body */ 0,
                /* a_return_var */ to_return,
                ASR::abiType::BindC, ASR::accessType::Public, ASR::deftypeType::Interface,
                nullptr, false, false, false, false, false, nullptr, 0,
                false, false, false);
            parent_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(tmp));
            current_scope = parent_scope;
        } else {
            diag.add(Diagnostic(
                "function interface must be specified explicitly; you can enable implicit interfaces with `--implicit-interface`",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
    }

    bool check_is_external(std::string sym, SymbolTable* scope = nullptr) {
        if (scope) {
            external_procedures = external_procedures_mapping[get_hash(scope->asr_owner)];
        } else if (current_scope->asr_owner) {
            external_procedures = external_procedures_mapping[get_hash(current_scope->asr_owner)];
        }
        return ( std::find(external_procedures.begin(), external_procedures.end(), sym) != external_procedures.end() );
    }

    bool check_is_explicit_intrinsic(std::string sym, SymbolTable* scope=nullptr) {
        if (scope) {
            explicit_intrinsic_procedures = explicit_intrinsic_procedures_mapping[get_hash(scope->asr_owner)];
        } else if (current_scope->asr_owner) {
            explicit_intrinsic_procedures = explicit_intrinsic_procedures_mapping[get_hash(current_scope->asr_owner)];
        }
        return (
            std::find(explicit_intrinsic_procedures.begin(), explicit_intrinsic_procedures.end(), sym) != explicit_intrinsic_procedures.end()
        );
    }

    void erase_from_external_mapping(std::string sym) {
        uint64_t hash = get_hash(current_scope->asr_owner);
        external_procedures_mapping[hash].erase(
            std::remove(external_procedures_mapping[hash].begin(),
            external_procedures_mapping[hash].end(), sym),
            external_procedures_mapping[hash].end());
    }

    void erase_from_explicit_intrinsic_mapping(std::string sym) {
        uint64_t hash = get_hash(current_scope->asr_owner);
        explicit_intrinsic_procedures_mapping[hash].erase(
            std::remove(
                explicit_intrinsic_procedures_mapping[hash].begin(),
                explicit_intrinsic_procedures_mapping[hash].end(),
                sym
            )
        );
    }

    // pad (with ' ') or trim character string 'value'
    ASR::expr_t* adjust_character_length(ASR::expr_t* value, int64_t lhs_len, int64_t rhs_len, const Location& loc, Allocator& al) {
        ASR::StringConstant_t* string_constant = ASR::down_cast<ASR::StringConstant_t>(value);
        char* original_str = string_constant->m_s;
        size_t original_length = std::strlen(original_str);

        size_t new_length = static_cast<size_t>(lhs_len);
        char* adjusted_str = al.allocate<char>(new_length + 1);

        if (lhs_len < rhs_len) { // trim
            std::memcpy(adjusted_str, original_str, new_length);
        } else { // pad
            std::memcpy(adjusted_str, original_str, original_length);
            std::memset(adjusted_str + original_length, ' ', new_length - original_length);
        }
        adjusted_str[new_length] = '\0'; // null-terminate the string

        ASR::ttype_t* value_type = ASRUtils::TYPE(ASR::make_String_t(
            al, loc, 1,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, loc, new_length, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::PointerString));

        return ASRUtils::EXPR(ASR::make_StringConstant_t(
            al, loc, adjusted_str, value_type));
    }

    ASR::expr_t* adjust_array_character_length(ASR::expr_t* value, int64_t lhs_len, int64_t rhs_len, Allocator& al) {
        ASR::ArrayConstant_t* array_constant = ASR::down_cast<ASR::ArrayConstant_t>(value);
        Vec<ASR::expr_t*> body;
        size_t array_size = ASRUtils::get_fixed_size_of_array(array_constant->m_type);

        body.reserve(al, array_size);

        for (size_t i = 0; i < array_size; i++) {
            ASR::expr_t* item = ASRUtils::fetch_ArrayConstant_value(al, array_constant, i);

            if (ASR::is_a<ASR::ArrayConstant_t>(*item)) {
                item = adjust_array_character_length(item, lhs_len, rhs_len,  al);
            } else {
                item = adjust_character_length(item, lhs_len, rhs_len, item->base.loc, al);
            }

            body.push_back(al, item);
        }

        array_constant->m_data = ASRUtils::set_ArrayConstant_data(
                body.p, body.size(), ASRUtils::extract_type(array_constant->m_type));

        return (ASR::expr_t*) array_constant;
    }

    void visit_DeclarationUtil(const AST::Declaration_t &x) {
        _declaring_variable = true;
        if (x.m_vartype == nullptr &&
                x.n_attributes == 1 &&
                AST::is_a<AST::AttrNamelist_t>(*x.m_attributes[0])) {
            //char *name = down_cast<AttrNamelist_t>(x.m_attributes[0])->m_name;
            diag.add(Diagnostic(
                "Namelists not implemented yet",
                Level::Error, Stage::Semantic, {
                    Label("",{x.base.base.loc})
                }));
            throw SemanticAbort();
        }
        for (size_t i=0; i<x.n_attributes; i++) {
            if (AST::is_a<AST::AttrType_t>(*x.m_attributes[i])) {
                diag.add(Diagnostic(
                    "Type must be declared first",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            };
        }
        if (x.m_vartype == nullptr) {
            // Examples:
            // private
            // public
            // private :: x, y, z
            if (x.n_attributes == 0) {
                diag.add(Diagnostic(
                    "No attribute specified",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
            for (size_t i=0; i<x.n_attributes; i++) {
                if (AST::is_a<AST::SimpleAttribute_t>(*x.m_attributes[i])) {
                    AST::SimpleAttribute_t *sa =
                        AST::down_cast<AST::SimpleAttribute_t>(x.m_attributes[i]);
                    if (x.n_syms == 0) {
                        // Example:
                        // private
                        if (sa->m_attr == AST::simple_attributeType
                                ::AttrPrivate) {
                            dflt_access = ASR::accessType::Private;
                        } else if (sa->m_attr == AST::simple_attributeType
                                ::AttrPublic) {
                            // Do nothing (public access is the default)
                            LCOMPILERS_ASSERT(dflt_access == ASR::accessType::Public);
                        } else if (sa->m_attr == AST::simple_attributeType
                                ::AttrSave) {
                            if (in_module && !in_Subroutine) {
                                // Do nothing (all variables implicitly have the
                                // save attribute in a module/main program)
                            } else {
                                default_storage_save = true;
                            }
                        } else if (sa->m_attr == AST::simple_attributeType
                                ::AttrSequence) {
                            // TODO: Implement it for CPP backend
                        } else {
                            diag.add(Diagnostic(
                                "Attribute declaration not supported yet",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    } else {
                        // Example:
                        // private :: x, y, z
                        for (size_t i=0; i<x.n_syms; i++) {
                            AST::var_sym_t &s = x.m_syms[i];
                            if (s.m_name == nullptr) {
                                if (s.m_spec->type == AST::decl_attributeType::AttrIntrinsicOperator) {
                                    // Operator Overloading Encountered
                                    if( sa->m_attr != AST::simple_attributeType::AttrPublic &&
                                    sa->m_attr != AST::simple_attributeType::AttrPrivate ) {
                                    overloaded_ops[current_scope][s.m_spec] = AST::simple_attributeType::AttrPublic;
                                    } else {
                                    overloaded_ops[current_scope][s.m_spec] = sa->m_attr;
                                    }
                                } else if( s.m_spec->type == AST::decl_attributeType::AttrAssignment ) {
                                    // Assignment Overloading Encountered
                                    if( sa->m_attr != AST::simple_attributeType::AttrPublic &&
                                    sa->m_attr != AST::simple_attributeType::AttrPrivate ) {
                                    assgn[current_scope] = ASR::Public;
                                    } else {
                                    assgn[current_scope] = get_asr_simple_attr(sa->m_attr);
                                    }
                                } else if (s.m_spec->type == AST::decl_attributeType::AttrDefinedOperator) {
                                    //std::string op_name = to_lower(AST::down_cast<AST::AttrDefinedOperator_t>(s.m_spec)->m_op_name);
                                    // Custom Operator Overloading Encountered
                                    if( sa->m_attr != AST::simple_attributeType::AttrPublic &&
                                    sa->m_attr != AST::simple_attributeType::AttrPrivate ) {
                                    overloaded_ops[current_scope][s.m_spec] = AST::simple_attributeType::AttrPublic;
                                    } else {
                                    overloaded_ops[current_scope][s.m_spec] = sa->m_attr;
                                    }
                                } else {
                                    diag.add(Diagnostic(
                                        "Attribute type not implemented yet.",
                                        Level::Error, Stage::Semantic, {
                                            Label("",{x.base.base.loc})
                                        }));
                                    throw SemanticAbort();
                                }
                            } else {
                                std::string sym = to_lower(s.m_name);
                                if (sa->m_attr == AST::simple_attributeType
                                        ::AttrPrivate) {
                                    ASR::symbol_t* sym_ = current_scope->get_symbol(sym);
                                    if (!sym_) {
                                        assgnd_access[sym] = ASR::accessType::Private;
                                    } else {
                                        sym_ = ASRUtils::symbol_get_past_external(sym_);
                                        if (ASR::is_a<ASR::Variable_t>(*sym_)) {
                                            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_);
                                            v->m_access = ASR::accessType::Private;
                                        } else if (ASR::is_a<ASR::Function_t>(*sym_)) {
                                            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym_);
                                            f->m_access = ASR::accessType::Private;
                                        }
                                    }
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrPublic) {
                                    ASR::symbol_t* sym_ = current_scope->get_symbol(sym);
                                    if (!sym_) {
                                        assgnd_access[sym] = ASR::accessType::Public;
                                    } else {
                                        sym_ = ASRUtils::symbol_get_past_external(sym_);
                                        if (ASR::is_a<ASR::Variable_t>(*sym_)) {
                                            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_);
                                            v->m_access = ASR::accessType::Public;
                                        } else if (ASR::is_a<ASR::Function_t>(*sym_)) {
                                            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym_);
                                            f->m_access = ASR::accessType::Public;
                                        }
                                    }
                                } else if (sa->m_attr == AST::simple_attributeType
                                                ::AttrParameter) {
                                    ASR::symbol_t* sym_ = current_scope->get_symbol(sym);
                                    if (!sym_) {
                                        assgnd_storage[sym] = std::make_pair(ASR::storage_typeType::Parameter, s.m_initializer);
                                    } else {
                                        this->visit_expr(*s.m_initializer);
                                        ASR::expr_t* init_val = ASRUtils::EXPR(tmp);
                                        sym_ = ASRUtils::symbol_get_past_external(sym_);
                                        if (ASR::is_a<ASR::Variable_t>(*sym_)) {
                                            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_);
                                            v->m_storage = ASR::storage_typeType::Parameter;
                                            if (ASR::is_a<ASR::RealConstant_t>(*init_val)) {
                                                ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(init_val);
                                                init_val = ASRUtils::EXPR(ASR::make_RealConstant_t(al, x.base.base.loc, rc->m_r, v->m_type));
                                            } else if (ASR::is_a<ASR::IntegerConstant_t>(*init_val)) {
                                                ASR::IntegerConstant_t* ic = ASR::down_cast<ASR::IntegerConstant_t>(init_val);
                                                init_val = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, ic->m_n, v->m_type));
                                            }
                                            v->m_symbolic_value = init_val;
                                            v->m_value = ASRUtils::expr_value(init_val);
                                            SetChar variable_dependencies_vec;
                                            variable_dependencies_vec.reserve(al, 1);
                                            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, v->m_type, v->m_symbolic_value, v->m_value);
                                            v->m_dependencies = variable_dependencies_vec.p;
                                            v->n_dependencies = variable_dependencies_vec.size();
                                        }
                                    }
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrOptional) {
                                    ASR::symbol_t* sym_ = current_scope->get_symbol(sym);
                                    if (!sym_) {
                                        assgnd_presence[sym] = ASR::presenceType::Optional;
                                    } else {
                                        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                                            ASRUtils::symbol_get_past_external(sym_));
                                        v->m_presence = ASR::presenceType::Optional;
                                    }
                                } else if(sa->m_attr == AST::simple_attributeType
                                        ::AttrIntrinsic) {
                                    explicit_intrinsic_procedures.push_back(sym);
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrExternal) {
                                    create_external_function(sym, x.m_syms[i].loc);
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrSave) {
                                    ASR::symbol_t* sym_ = current_scope->get_symbol(sym);
                                    if (!sym_) {
                                        if (compiler_options.implicit_typing) {
                                            ASR::ttype_t* type = implicit_dictionary[std::string(1,sym[0])];
                                            if (type) {
                                                sym_ = declare_implicit_variable(x.m_syms[i].loc, sym, ASR::intentType::Local);
                                                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_);
                                                v->m_storage = ASR::storage_typeType::Save;
                                            } else {
                                                // there exists an `implicit none` statement
                                                diag.add(Diagnostic(
                                                    "Save `" + sym + "` has no IMPLICIT Type",
                                                    Level::Error, Stage::Semantic, {
                                                        Label("",{x.base.base.loc})
                                                    }));
                                                throw SemanticAbort();
                                            }
                                        } else {
                                            diag.add(Diagnostic(
                                                "Save `" + sym + "` has no IMPLICIT Type, use `--implicit-typing`",
                                                Level::Error, Stage::Semantic, {
                                                    Label("",{x.base.base.loc})
                                                }));
                                            throw SemanticAbort();
                                        }
                                    } else {
                                        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_);
                                        v->m_storage = ASR::storage_typeType::Save;
                                    }
                                } else if (sa->m_attr == AST::simple_attributeType::AttrEnumerator) {
                                    ASR::symbol_t *sym;
                                    ASR::ttype_t *init_type = ASRUtils::TYPE(
                                        ASR::make_Integer_t(al, x.m_syms[i].loc, compiler_options.po.default_integer_kind));
                                    ASR::expr_t *init_expr = ASRUtils::EXPR(
                                        ASR::make_IntegerConstant_t(al, x.m_syms[i].loc,
                                        enum_init_val, init_type));
                                    if (x.m_syms[i].m_sym == AST::symbolType::Equal) {
                                        this->visit_expr(*x.m_syms[i].m_initializer);
                                        init_expr = ASRUtils::expr_value(ASRUtils::EXPR(tmp));
                                        if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(init_expr))) {
                                            diag.add(Diagnostic(
                                                "Enumerator must be initialized with an integer expression",
                                                Level::Error, Stage::Semantic, {
                                                    Label("",{x.m_syms[i].loc})
                                                }));
                                            throw SemanticAbort();
                                        }
                                        if (ASR::is_a<ASR::IntegerConstant_t>(*init_expr)) {
                                            enum_init_val = ASR::down_cast<
                                                ASR::IntegerConstant_t>(init_expr)->m_n;
                                        } else {
                                            LCOMPILERS_ASSERT(false); // TODO
                                        }
                                    }
                                    ASR::expr_t* init_expr_value = ASRUtils::expr_value(init_expr);
                                    if( init_expr && !ASRUtils::is_value_constant(init_expr_value) ) {
                                        diag.add(Diagnostic(
                                            "Initialization of `" + std::string(x.m_syms[i].m_name) +
                                            "` must reduce to a compile time constant.",
                                            Level::Error, Stage::Semantic, {
                                                Label("",{x.m_syms[i].loc})
                                            }));
                                        throw SemanticAbort();
                                    }
                                    sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
                                        al, x.m_syms[i].loc, current_scope,
                                        x.m_syms[i].m_name, nullptr, 0, ASR::intentType::Local,
                                        init_expr, init_expr_value, ASR::storage_typeType::Parameter,
                                        init_type, nullptr, ASR::abiType::Source, ASR::accessType::Public,
                                        ASR::presenceType::Required, false, false));
                                    current_scope->add_symbol(x.m_syms[i].m_name, sym);
                                    enum_init_val++;
                                } else {
                                    diag.add(Diagnostic(
                                        "Attribute declaration not supported",
                                        Level::Error, Stage::Semantic, {
                                            Label("",{x.base.base.loc})
                                        }));
                                    throw SemanticAbort();
                                }
                                ASR::symbol_t* sym_ = current_scope->resolve_symbol(sym);
                                if (!sym_ && compiler_options.implicit_typing &&
                                    sa->m_attr != AST::simple_attributeType::AttrExternal
                                ) {
                                    ASR::expr_t* value = nullptr;
                                    if (sa->m_attr == AST::simple_attributeType::AttrParameter) {
                                        this->visit_expr(*s.m_initializer);
                                        value = ASRUtils::EXPR(tmp);
                                    }
                                    if ( implicit_dictionary[std::string(1, sym[0])] != nullptr ) {
                                        sym_ = declare_implicit_variable2(x.m_syms[i].loc, sym, ASR::intentType::Local, implicit_dictionary[std::string(1, sym[0])], value);
                                    }
                                }
                            }
                        }
                    }
		    // enable sole `dimension` attribute
                } else if (AST::is_a<AST::AttrDimension_t>(*x.m_attributes[i])) {
                    for (size_t i=0;i<x.n_syms;++i) { // symbols for line only
                        AST::var_sym_t &s = x.m_syms[i];
			dimension_variable(s, x.base.base.loc);
                    }
		} else if (AST::is_a<AST::AttrCommon_t>(*x.m_attributes[i])) {
		    AST::AttrCommon_t const & common_stmt =
			*AST::down_cast<AST::AttrCommon_t>(x.m_attributes[i]);
		    constexpr char BLANK_BLOCK[] = "blank#block";
		    std::string common_block_name;
		    /* Local dictionary to aggregate objects into:
		       "COMMON A /B1/ B, // C", will put "A" and "C"
		       in the blank block.  This should be done for
		       the entire declaration-construct. */
		    common_block_varsyms objs_by_blk;
		    for (size_t bi = 0; bi < common_stmt.n_blks; ++bi) {
			AST::common_block_t const &cb = common_stmt.m_blks[bi];
			if (cb.m_name) {
			    common_block_name = to_lower(cb.m_name);
			} else {
			    common_block_name = BLANK_BLOCK;
			}

			// Aggregate the objects into their named common blocks
			for (size_t oi = 0; oi < cb.n_objects; ++oi) {
			    AST::var_sym_t const & vs = cb.m_objects[oi];
			    objs_by_blk[common_block_name].push_back(vs);
			    if (vs.n_dim) {
				/* Treat this as a dimension statement. */
				dimension_variable(vs, x.base.base.loc);
			    }
			}
		    }
		    populate_common_dictionary(x, objs_by_blk);
		} else if (AST::is_a<AST::AttrEquivalence_t>(*x.m_attributes[i])) {
                    AST::AttrEquivalence_t *eq = AST::down_cast<AST::AttrEquivalence_t>(x.m_attributes[i]);
                    int eq_n_args = eq->n_args;
                    for (int i = 0; i < eq_n_args; i++) {
                        if (eq->m_args[i].n_set_list == 2) {
                            AST::expr_t *eq1 = eq->m_args[i].m_set_list[0];
                            AST::expr_t *eq2 = eq->m_args[i].m_set_list[1];
                            this->visit_expr(*eq1);
                            ASR::expr_t* asr_eq1 = ASRUtils::EXPR(tmp);
                            this->visit_expr(*eq2);
                            ASR::expr_t* asr_eq2 = ASRUtils::EXPR(tmp);

                            if (AST::is_a<AST::FuncCallOrArray_t>(*eq1) && AST::is_a<AST::FuncCallOrArray_t>(*eq2)) {
                                ASR::ttype_t* arg_type1 = ASRUtils::type_get_past_allocatable(
                                            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(asr_eq1)));
                                ASR::ttype_t* pointer_type_ = ASRUtils::TYPE(ASR::make_Pointer_t(al, asr_eq1->base.loc, ASRUtils::type_get_past_array(arg_type1)));
                                ASR::asr_t* get_pointer = ASR::make_GetPointer_t(al, asr_eq1->base.loc, asr_eq1, pointer_type_, nullptr);
                                ASR::ttype_t *cptr = ASRUtils::TYPE(ASR::make_CPtr_t(al, asr_eq1->base.loc));
                                ASR::asr_t* pointer_to_cptr = ASR::make_PointerToCPtr_t(al, asr_eq1->base.loc, ASRUtils::EXPR(get_pointer), cptr, nullptr);

                                ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, asr_eq1->base.loc, compiler_options.po.default_integer_kind));
                                ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, asr_eq1->base.loc, 1, int_type));

                                Vec<ASR::dimension_t> dim;
                                dim.reserve(al, 1);
                                ASR::dimension_t dim_;
                                dim_.m_length = nullptr; dim_.m_start = nullptr;
                                dim_.m_start = one;
                                dim_.m_length = one;
                                dim_.loc = asr_eq1->base.loc;
                                dim.push_back(al, dim_);

                                ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(asr_eq2);
                                ASR::expr_t* array = array_item->m_v;
                                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(array);
                                ASR::Variable_t *var__ = ASR::down_cast<ASR::Variable_t>(var->m_v);
                                std::string name = var__->m_name;
                                ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(var__->m_type);
                                ASR::ttype_t* type = nullptr;
                                ASR::ttype_t* arg_type2 = ASRUtils::type_get_past_allocatable(
                                            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(asr_eq2)));

                                Vec<ASR::dimension_t> dim2; dim2.reserve(al, arr->n_dims);
                                for (size_t i = 0; i < arr->n_dims; i++) {
                                    ASR::dimension_t dim2_; dim2_.m_start = nullptr; dim2_.m_length = nullptr;
                                    dim2_.loc = asr_eq2->base.loc;
                                    dim2.push_back(al, dim2_);
                                }

                                if (ASR::is_a<ASR::Integer_t>(*arg_type2)) {
                                    type = ASRUtils::TYPE(ASR::make_Integer_t(al, asr_eq2->base.loc, compiler_options.po.default_integer_kind));
                                } else if (ASR::is_a<ASR::Real_t>(*arg_type2)) {
                                    type = ASRUtils::TYPE(ASR::make_Real_t(al, asr_eq2->base.loc, 4));
                                } else {
                                    diag.semantic_warning_label(
                                        "This equivalence statement is not implemented yet, for now we will ignore it",
                                        {x.base.base.loc},
                                        "ignored for now"
                                    );
                                }
                                type = ASRUtils::make_Array_t_util(al, asr_eq2->base.loc, type, dim2.p, dim2.size(), ASR::abiType::Source, false, ASR::array_physical_typeType::DescriptorArray, false, false);
                                ASR::ttype_t* ptr = ASRUtils::TYPE(ASR::make_Pointer_t(al, asr_eq2->base.loc, type));
                                var__->m_type = ptr;

                                Vec<ASR::expr_t*> args;
                                args.reserve(al, arr->n_dims);
                                for (size_t i = 0; i < arr->n_dims; i++) {
                                    ASR::IntegerConstant_t* ic = ASR::down_cast<ASR::IntegerConstant_t>(arr->m_dims[i].m_length);
                                    ASR::expr_t* size = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, asr_eq1->base.loc, ic->m_n, int_type));
                                    args.push_back(al, size);
                                }

                                ASR::ttype_t* array_type = ASRUtils::TYPE(ASR::make_Array_t(al, asr_eq1->base.loc, int_type, dim.p, dim.size(), ASR::array_physical_typeType::PointerToDataArray));
                                ASR::asr_t* array_constant = ASRUtils::make_ArrayConstructor_t_util(al, asr_eq1->base.loc, args.p, args.size(), array_type, ASR::arraystorageType::ColMajor);
                                ASR::asr_t* c_f_pointer = ASR::make_CPtrToPointer_t(al, asr_eq1->base.loc, ASRUtils::EXPR(pointer_to_cptr), ASR::down_cast<ASR::ArrayItem_t>(asr_eq2)->m_v, ASRUtils::EXPR(array_constant), nullptr);

                                ASR::stmt_t *stmt = ASRUtils::STMT(c_f_pointer);
                                data_structure.push_back(stmt);
                            } else {
                                if (AST::is_a<AST::FuncCallOrArray_t>(*eq1)) {
                                    ASR::ttype_t* arg_type1 = ASRUtils::type_get_past_allocatable(
                                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(asr_eq1)));
                                    ASR::ttype_t* pointer_type_ = ASRUtils::TYPE(ASR::make_Pointer_t(al, asr_eq1->base.loc, ASRUtils::type_get_past_array(arg_type1)));
                                    ASR::asr_t* get_pointer = ASR::make_GetPointer_t(al, asr_eq1->base.loc, asr_eq1, pointer_type_, nullptr);
                                    ASR::ttype_t *cptr = ASRUtils::TYPE(ASR::make_CPtr_t(al, asr_eq1->base.loc));
                                    ASR::asr_t* pointer_to_cptr = ASR::make_PointerToCPtr_t(al, asr_eq1->base.loc, ASRUtils::EXPR(get_pointer), cptr, nullptr);

                                    ASR::ttype_t* arg_type2 = ASRUtils::expr_type(asr_eq2);
                                    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(asr_eq2);
                                    ASR::Variable_t *var__ = ASR::down_cast<ASR::Variable_t>(var->m_v);
                                    ASR::ttype_t* type = nullptr;
                                    if (ASR::is_a<ASR::Real_t>(*arg_type2)) {
                                        type = ASRUtils::TYPE(ASR::make_Real_t(al, asr_eq2->base.loc, 4));
                                    } else if (ASR::is_a<ASR::Integer_t>(*arg_type2)) {
                                        type = ASRUtils::TYPE(ASR::make_Integer_t(al, asr_eq2->base.loc, compiler_options.po.default_integer_kind));
                                    } else {
                                        diag.semantic_warning_label(
                                            "This equivalence statement is not implemented yet, for now we will ignore it",
                                            {x.base.base.loc},
                                            "ignored for now"
                                        );
                                    }
                                    ASR::ttype_t* ptr = ASRUtils::TYPE(ASR::make_Pointer_t(al, asr_eq2->base.loc, type));
                                    var__->m_type = ptr;

                                    ASR::asr_t* c_f_pointer = ASR::make_CPtrToPointer_t(al, asr_eq1->base.loc, ASRUtils::EXPR(pointer_to_cptr),asr_eq2, nullptr, nullptr);
                                    ASR::stmt_t *stmt = ASRUtils::STMT(c_f_pointer);
                                    data_structure.push_back(stmt);
                                } else if (AST::is_a<AST::FuncCallOrArray_t>(*eq2)) {
                                    ASR::ttype_t* arg_type2 = ASRUtils::type_get_past_allocatable(
                                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(asr_eq2)));
                                    ASR::ttype_t* pointer_type_ = ASRUtils::TYPE(ASR::make_Pointer_t(al, asr_eq2->base.loc, ASRUtils::type_get_past_array(arg_type2)));
                                    ASR::asr_t* get_pointer = ASR::make_GetPointer_t(al, asr_eq2->base.loc, asr_eq2, pointer_type_, nullptr);
                                    ASR::ttype_t *cptr = ASRUtils::TYPE(ASR::make_CPtr_t(al, asr_eq2->base.loc));
                                    ASR::asr_t* pointer_to_cptr = ASR::make_PointerToCPtr_t(al, asr_eq2->base.loc, ASRUtils::EXPR(get_pointer), cptr, nullptr);

                                    ASR::ttype_t* arg_type1 = ASRUtils::expr_type(asr_eq1);
                                    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(asr_eq1);
                                    ASR::Variable_t *var__ = ASR::down_cast<ASR::Variable_t>(var->m_v);
                                    ASR::ttype_t* type = nullptr;
                                    if (ASR::is_a<ASR::Real_t>(*arg_type1)) {
                                        type = ASRUtils::TYPE(ASR::make_Real_t(al, asr_eq1->base.loc, 4));
                                    } else if (ASR::is_a<ASR::Integer_t>(*arg_type1)) {
                                        type = ASRUtils::TYPE(ASR::make_Integer_t(al, asr_eq1->base.loc, compiler_options.po.default_integer_kind));
                                    } else {
                                        diag.semantic_warning_label(
                                            "This equivalence statement is not implemented yet, for now we will ignore it",
                                            {x.base.base.loc},
                                            "ignored for now"
                                        );
                                    }
                                    ASR::ttype_t* ptr = ASRUtils::TYPE(ASR::make_Pointer_t(al, asr_eq1->base.loc, type));
                                    var__->m_type = ptr;

                                    ASR::asr_t* c_f_pointer = ASR::make_CPtrToPointer_t(al, asr_eq2->base.loc, ASRUtils::EXPR(pointer_to_cptr),asr_eq1, nullptr, nullptr);
                                    ASR::stmt_t *stmt = ASRUtils::STMT(c_f_pointer);
                                    data_structure.push_back(stmt);
                                } else {
                                    diag.semantic_warning_label(
                                        "This equivalence statement is not implemented yet, for now we will ignore it",
                                        {x.base.base.loc},
                                        "ignored for now"
                                    );
                                }
                            }
                        } else {
                            diag.semantic_warning_label(
                                "This equivalence statement is not implemented yet, for now we will ignore it",
                                {x.base.base.loc},
                                "ignored for now"
                            );
                        }
                    }
                } else if (AST::is_a<AST::AttrIntent_t>(*x.m_attributes[i])) {
                    AST::AttrIntent_t* ai = AST::down_cast<AST::AttrIntent_t>(x.m_attributes[i]);
                    ASR::intentType s_intent;
                    switch (ai->m_intent) {
                        case (AST::attr_intentType::In) : {
                            s_intent = ASRUtils::intent_in;
                            break;
                        }
                        case (AST::attr_intentType::Out) : {
                            s_intent = ASRUtils::intent_out;
                            break;
                        }
                        case (AST::attr_intentType::InOut) : {
                            s_intent = ASRUtils::intent_inout;
                            break;
                        }
                        default : {
                            s_intent = ASRUtils::intent_unspecified;
                            break;
                        }
                    }
                    AST::var_sym_t* s = x.m_syms;
                    std::string sym = to_lower(s->m_name);
                    ASR::symbol_t* orig_decl = current_scope->get_symbol(sym);
                    if ( orig_decl && ASR::is_a<ASR::Variable_t>(*orig_decl) ) {
                        ASR::Variable_t* orig_decl_variable = ASR::down_cast<ASR::Variable_t>(orig_decl);
                        orig_decl_variable->m_intent = s_intent;
                    }
                } else {
                    diag.add(Diagnostic(
                        "Attribute declaration not supported",
                        Level::Error, Stage::Semantic, {
                            Label("",{x.base.base.loc})
                        }));
                    throw SemanticAbort();
                }
            }
        } else {
            // Example
            // real(dp), private :: x, y(3), z
            for (size_t i=0; i<x.n_syms; i++) {
                bool is_save = false;
                bool implicit_save = false;
                bool is_compile_time = false;
                bool is_implicitly_declared = false;
                bool is_dimension_star = false;
                AST::var_sym_t &s = x.m_syms[i];
                std::string sym = to_lower(s.m_name);
                bool is_external = check_is_external(sym);
                bool is_attr_external = false;
                ASR::accessType s_access = dflt_access;
                ASR::presenceType s_presence = dflt_presence;
                ASR::storage_typeType storage_type = dflt_storage;
                bool target_attr = false;
                bool contig_attr = false;
                bool value_attr = false;
                char *bindc_name = nullptr;
                bool is_volatile = false;
                AST::AttrType_t *sym_type =
                    AST::down_cast<AST::AttrType_t>(x.m_vartype);
                bool is_char_type = sym_type->m_type == AST::decl_typeType::TypeCharacter;
                if (assgnd_access.count(sym)) {
                    s_access = assgnd_access[sym];
                }
                if (assgnd_presence.count(sym)) {
                    s_presence = assgnd_presence[sym];
                }
                if (assgnd_storage.count(sym)) {
                    storage_type = assgnd_storage[sym].first;
                    s.m_initializer = assgnd_storage[sym].second;
                }
                bool is_pointer = false;
                if (current_scope->get_symbol(sym) !=
                        nullptr) {
                    if (current_scope->parent != nullptr && !is_external) {
                        if ( compiler_options.implicit_typing && implicit_dictionary[std::string(1, sym[0])]!=nullptr ) {
                            // sym is implicitly declared
                            is_implicitly_declared = true;
                        } else if (pre_declared_array_dims.find(sym) != pre_declared_array_dims.end()) {
                            // sym is implicitly declared
                            is_implicitly_declared = true;
                            pre_declared_array_dims[sym] = 2;
                        } else {
                            // re-declaring a global scope variable is allowed
                            // Otherwise raise an error
                            ASR::symbol_t *orig_decl = current_scope->get_symbol(sym);
                            diag.add(Diagnostic(
                                "Symbol is already declared in the same scope",
                                Level::Error, Stage::Semantic, {
                                    Label("redeclaration",{s.loc}),
                                    Label("original declaration",{orig_decl->base.loc}, false)
                                }));
                            throw SemanticAbort();
                        }
                    }
                }
                ASR::intentType s_intent;
                bool is_argument = false;
                if (std::find(current_procedure_args.begin(),
                        current_procedure_args.end(), to_lower(s.m_name)) !=
                        current_procedure_args.end()) {
                    s_intent = ASRUtils::intent_unspecified;
                    is_argument = true;
                } else {
                    s_intent = ASRUtils::intent_local;
                }
                ASR::abiType s_abi = is_argument ? current_procedure_abi_type : ASR::abiType::Source;
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 0);
                // location for dimension(...) if present
                Location dims_attr_loc;
                bool is_allocatable = false;
                if (x.n_attributes > 0) {
                    for (size_t i=0; i < x.n_attributes; i++) {
                        AST::decl_attribute_t *a = x.m_attributes[i];
                        if (AST::is_a<AST::AttrIntent_t>(*a)) {
                            AST::AttrIntent_t *ai =
                                AST::down_cast<AST::AttrIntent_t>(a);
                            switch (ai->m_intent) {
                                case (AST::attr_intentType::In) : {
                                    s_intent = ASRUtils::intent_in;
                                    break;
                                }
                                case (AST::attr_intentType::Out) : {
                                    s_intent = ASRUtils::intent_out;
                                    break;
                                }
                                case (AST::attr_intentType::InOut) : {
                                    s_intent = ASRUtils::intent_inout;
                                    break;
                                }
                                default : {
                                    s_intent = ASRUtils::intent_unspecified;
                                    break;
                                }
                            }
                        }
                    }
                    for (size_t i=0; i < x.n_attributes; i++) {
                        AST::decl_attribute_t *a = x.m_attributes[i];
                        if (AST::is_a<AST::SimpleAttribute_t>(*a)) {
                            AST::SimpleAttribute_t *sa =
                                AST::down_cast<AST::SimpleAttribute_t>(a);
                            if (sa->m_attr == AST::simple_attributeType
                                    ::AttrPrivate) {
                                s_access = ASR::accessType::Private;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrPublic) {
                                s_access = ASR::accessType::Public;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrSave) {
                                storage_type = ASR::storage_typeType::Save;
                                is_save = true;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrParameter) {
                                storage_type = ASR::storage_typeType::Parameter;
                            } else if( sa->m_attr == AST::simple_attributeType
                                    ::AttrAllocatable ) {
                                is_allocatable = true;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrPointer) {
                                is_pointer = true;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrOptional) {
                                s_presence = ASR::presenceType::Optional;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrTarget) {
                                target_attr = true;
                                if (storage_type == ASR::storage_typeType::Parameter) {
                                    diag.add(Diagnostic(
                                        "Parameter attribute cannot be used with Target attribute",
                                        Level::Error, Stage::Semantic, {
                                            Label("",{x.base.base.loc})
                                        }));
                                    throw SemanticAbort();
                                }
                                // Do nothing for now
                            } else if (sa->m_attr == AST::AttrContiguous){
                                contig_attr = true;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrValue) {
                                value_attr = true;
                            } else if(sa->m_attr == AST::simple_attributeType
                                    ::AttrIntrinsic) {
                                excluded_from_symtab.push_back(sym);
                            } else if(sa->m_attr == AST::simple_attributeType
                                    ::AttrExternal) {
                                is_attr_external = true;
                                assgnd_access[sym] = ASR::accessType::Public;
                                if (assgnd_access.count(sym)) {
                                    s_access = assgnd_access[sym];
                                }
                                is_external = true;
                            } else if(sa->m_attr == AST::simple_attributeType
                                ::AttrNoPass) {
                            } else if (sa->m_attr == AST::simple_attributeType::AttrVolatile) {
                                is_volatile = true;
                            } else {
                                diag.add(Diagnostic(
                                    "Attribute type not implemented yet " + std::to_string(sa->m_attr),
                                    Level::Error, Stage::Semantic, {
                                        Label("",{x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                        } else if (AST::is_a<AST::AttrIntent_t>(*a)) {
                            // processed already
                        } else if (AST::is_a<AST::AttrDimension_t>(*a)) {
                            AST::AttrDimension_t *ad =
                                AST::down_cast<AST::AttrDimension_t>(a);
                            if (dims.size() > 0) {
                                diag.add(Diagnostic(
                                    "Dimensions specified twice",
                                    Level::Error, Stage::Semantic, {
                                        Label("",{x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                            dims_attr_loc = ad->base.base.loc;
                            process_dims(al, dims, ad->m_dim, ad->n_dim, is_compile_time, is_char_type,
                                (s_intent == ASRUtils::intent_in || s_intent == ASRUtils::intent_out ||
                                s_intent == ASRUtils::intent_inout) || is_argument, s.m_name);
                        } else if (AST::is_a<AST::AttrBind_t>(*a)) {
                            AST::AttrBind_t attr_bd = *AST::down_cast<AST::AttrBind_t>(a);
                            extract_bind(attr_bd, s_abi, bindc_name, diag);
                        } else {
                            diag.add(Diagnostic(
                                "Attribute type not implemented yet",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    }


                    if (is_allocatable && storage_type == ASR::storage_typeType::Parameter) {
                        diag.add((Diagnostic(
                            "`parameter` attribute conflicts with `allocatable` attribute",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }
                        )));
                        throw SemanticAbort();
                    } else if (is_pointer && storage_type == ASR::storage_typeType::Parameter) {
                        diag.add((Diagnostic(
                            "`parameter` attribute conflicts with `pointer` attribute",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }
                        )));
                        throw SemanticAbort();
                    }

                    if (s_intent == ASRUtils::intent_out && value_attr) {
                        diag.add(Diagnostic(
                            "`value` attribute conflicts with `intent(out)` attribute",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    if (!s.m_initializer && s_intent == ASRUtils::intent_local
                            && storage_type == ASR::storage_typeType::Parameter) {
                        diag.add(Diagnostic(
                            "Variable `" + std::string(s.m_name) +
                            "` with parameter attribute is not initialised",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }
                }
                if (s.n_dim > 0) {
                    if (dims.size() > 0) {
                        // This happens for:
                        // integer, private, dimension(2,2) :: a(2,2)
                        diag.semantic_warning_label(
                            "Dimensions are specified twice",
                            {dims_attr_loc, s.loc}, // dimension(2,2), a(2,2)
                            "help: consider specifying it just one way or the other"
                        );
                        dims.n = 0;
                    }
                    if (s.m_dim[0].m_end_star == AST::dimension_typeType::DimensionStar) {
                        is_dimension_star = true;
                    }
                    process_dims(al, dims, s.m_dim, s.n_dim, is_compile_time, is_char_type,
                        (s_intent == ASRUtils::intent_in || s_intent == ASRUtils::intent_out ||
                        s_intent == ASRUtils::intent_inout), s.m_name);
                }
                ASR::symbol_t *type_declaration;
                ASR::ttype_t *type = nullptr;
                type = determine_type(x.base.base.loc, sym, x.m_vartype, is_pointer,
                    is_allocatable, dims, type_declaration, s_abi,
                    (s_intent != ASRUtils::intent_local) || is_argument, is_dimension_star);
                if ( is_attr_external ) create_external_function(sym, x.m_syms[i].loc, type);
                if ( current_scope->get_symbol( sym ) != nullptr && ( is_external && !is_attr_external ) ) {
                    /*
                        return type of external function is specified
                        external :: x
                        integer :: x -> we are handling this case
                    */
                    ASR::symbol_t *sym_ = current_scope->get_symbol(sym);
                    LCOMPILERS_ASSERT( sym_ != nullptr );
                    // set function return type as `type`
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym_);
                    ASRUtils::EXPR2VAR(f->m_return_var)->m_type = type;
                }
                current_variable_type_ = type;

                ASR::expr_t* init_expr = nullptr;
                ASR::expr_t* char_length { nullptr };
                ASR::expr_t* value = nullptr;

                // set the character (or character array) length correctly
                // e.g. character :: x*3   !> set char length to 3
                // OR character(len=4)     !> set char length to 4
                // OR character :: x(2)*3  !> set char length to 3
                if (is_char_type && s.m_length) {
                    this->visit_expr(*s.m_length);
                    ASR::String_t *lhs_type = ASR::down_cast<ASR::String_t>(
                        ASRUtils::type_get_past_array(type));
                    char_length = ASRUtils::EXPR(tmp);
                    ASR::expr_t* c_length = ASRUtils::expr_value(char_length);
                    ASRUtils::ASRBuilder b(al, x.base.base.loc);
                    if (c_length == nullptr) c_length = ASRUtils::expr_value(b.i32(0));
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::IntegerConstant_t>(*c_length))
                    int64_t lhs_len = ASR::down_cast<ASR::IntegerConstant_t>(c_length)->m_n;
                    lhs_type->m_len = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, lhs_len,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 8))));
                }
                ASR::Variable_t* variable_added_to_symtab = nullptr;
                if( std::find(excluded_from_symtab.begin(), excluded_from_symtab.end(), sym) == excluded_from_symtab.end() ) {
                    if ( !is_implicitly_declared && !is_external) {
                        SetChar variable_dependencies_vec;
                        variable_dependencies_vec.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type, init_expr, value);
                        if ( symbols_having_only_attributes_without_type.find(sym) != symbols_having_only_attributes_without_type.end() ) {
                            ASR::symbol_t* symbol = symbols_having_only_attributes_without_type[sym];
                            ASR::Variable_t* symbol_variable = ASR::down_cast<ASR::Variable_t>(symbol);
                            symbol_variable->base.base.loc = s.loc;
                            if ( symbol_variable->m_type ) {
                                if ( ASR::is_a<ASR::Array_t>(*symbol_variable->m_type) ) {
                                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(symbol_variable->m_type);
                                    array_type->m_type = type;
                                } else {
                                    symbol_variable->m_type = type;
                                }
                            } else {
                                symbol_variable->m_type = type;
                            }
                            current_scope->add_symbol(sym, symbol);
                            variable_added_to_symtab = symbol_variable;
                        } else {
                            ASR::asr_t *v = ASRUtils::make_Variable_t_util(al, s.loc, current_scope,
                                s2c(al, to_lower(s.m_name)), variable_dependencies_vec.p,
                                variable_dependencies_vec.size(), s_intent, init_expr, value,
                                storage_type, type, type_declaration, s_abi, s_access, s_presence,
                                value_attr, target_attr, contig_attr, bindc_name, is_volatile
                            );
                            current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(v));
                            variable_added_to_symtab = ASR::down_cast<ASR::Variable_t>(ASR::down_cast<ASR::symbol_t>(v));
                        }
                        if( is_derived_type ) {
                            data_member_names.push_back(al, s2c(al, to_lower(s.m_name)));
                        }
                    } else if ( is_implicitly_declared ) {
                        ASR::symbol_t* symbol = current_scope->get_symbol(sym);
                        ASR::Variable_t* symbol_variable = ASR::down_cast<ASR::Variable_t>(symbol);
                        if ( ASR::is_a<ASR::Array_t>(*symbol_variable->m_type) ) {
                            ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(symbol_variable->m_type);
                            array_type->m_type = type;
                        } else {
                            symbol_variable->m_type = type;
                        }
                    }
                }

                if (s.m_initializer != nullptr &&
                    sym_type->m_type == AST::decl_typeType::TypeType) {
                    if (AST::is_a<AST::FuncCallOrArray_t>(*s.m_initializer)) {
                        AST::FuncCallOrArray_t* func_call =
                            AST::down_cast<AST::FuncCallOrArray_t>(s.m_initializer);
                        ASR::symbol_t *sym_found = current_scope->resolve_symbol(func_call->m_func);
                        bool is_struct_const = false;
                        if (current_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner)) {
                            ASR::symbol_t* asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
                            if (ASR::is_a<ASR::Module_t>(*asr_owner_sym)) {
                                is_struct_const = true;
                            }
                        }
                        if (is_derived_type || storage_type == ASR::storage_typeType::Parameter) {
                            is_struct_const = true;
                        }
                        if (sym_found == nullptr) {
                            visit_FuncCallOrArray(*func_call);
                            init_expr = ASRUtils::EXPR(tmp);
                        } else {
                            if( ASR::is_a<ASR::Struct_t>(
                                *ASRUtils::symbol_get_past_external(sym_found)) ) {
                                init_expr = ASRUtils::EXPR(create_DerivedTypeConstructor(
                                                func_call->base.base.loc,
                                                func_call->m_args, func_call->n_args,
                                                func_call->m_keywords, func_call->n_keywords,
                                                sym_found, is_struct_const));
                            } else {
                                LCOMPILERS_ASSERT(false);
                            }
                        }
                    } else if (AST::is_a<AST::Name_t>(*s.m_initializer)) {
                        std::string sym_name = AST::down_cast<AST::Name_t>(s.m_initializer)->m_id;
                        sym_name = to_lower(sym_name);
                        if (sym_name == "c_null_ptr") {
                            ASR::symbol_t *sym_found = current_scope->resolve_symbol(sym_name);
                            if (sym_found == nullptr) {
                                diag.add(Diagnostic(
                                    "Symbol not found: `c_null_ptr`",
                                    Level::Error, Stage::Semantic, {
                                        Label("",{x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                            // Check if c_null_ptr is imported from iso_c_binding (intrinsic module)
                            if (ASR::is_a<ASR::ExternalSymbol_t>(*sym_found)) {
                                std::string m_name = ASR::down_cast<ASR::ExternalSymbol_t>(sym_found)->m_module_name;
                                if (startswith(m_name, "lfortran_intrinsic")) {
                                    init_expr = ASRUtils::EXPR(ASR::make_PointerNullConstant_t(al,
                                                    x.base.base.loc, current_variable_type_));
                                }
                            } else {
                                diag.add(Diagnostic(
                                    "Named initialization not supported with: " + sym_name,
                                    Level::Error, Stage::Semantic, {
                                        Label("",{x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }

                        } else {
                            diag.add(Diagnostic(
                                "Named initialization not supported with: " + sym_name,
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    } else if (AST::is_a<AST::ArrayInitializer_t>(*s.m_initializer)) {
                        AST::ArrayInitializer_t *array_init = AST::down_cast<AST::ArrayInitializer_t>(s.m_initializer);
                        if (array_init->n_args > 0) {
                            bool is_correct_type = true;
                            AST::FuncCallOrArray_t* func_call = nullptr;

                            is_correct_type = AST::is_a<AST::FuncCallOrArray_t>(*array_init->m_args[0]);
                            if (is_correct_type) {
                                func_call = AST::down_cast<AST::FuncCallOrArray_t>(array_init->m_args[0]);
                            }

                            if (!is_correct_type || strcmp(func_call->m_func, sym_type->m_name) != 0) {
                                diag.add(Diagnostic(
                                    "Array members must me of the same type as the struct",
                                    Level::Error, Stage::Semantic, {
                                        Label("",{array_init->m_args[0]->base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                        }

                        visit_ArrayInitializer(*array_init);
                        init_expr = ASRUtils::EXPR(tmp);
                    } else {
                        diag.add(Diagnostic(
                            "Only function call assignment is allowed for now",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }

                    value = ASRUtils::expr_value(init_expr);
                    if ( init_expr ) {
                        if( ASRUtils::is_value_constant(value) ) {
                        } else if( ASRUtils::is_value_constant(init_expr) ) {
                            value = nullptr;
                        } else if (ASR::is_a<ASR::ArrayConstructor_t>(*init_expr)) {
                            ASR::ArrayConstructor_t *array_construct = ASR::down_cast<ASR::ArrayConstructor_t>(init_expr);
                            for (size_t j = 0; j < array_construct->n_args; j++) {
                                if (!ASRUtils::is_value_constant(array_construct->m_args[j])) {
                                    diag.add(Diagnostic(
                                        "Initialization of `" + std::string(x.m_syms[i].m_name) +
                                        "` must reduce to a compile time constant.",
                                        Level::Error, Stage::Semantic, {
                                            Label("",{array_construct->m_args[j]->base.loc})
                                        }));
                                    throw SemanticAbort();
                                }
                            }
                            value = nullptr;
                        } else {
                            diag.add(Diagnostic(
                                "Initialization of `" + std::string(x.m_syms[i].m_name) +
                                "` must reduce to a compile time constant.",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    }
                } else if (s.m_initializer != nullptr) {
                    this->visit_expr(*s.m_initializer);
                    if (is_compile_time && AST::is_a<AST::ArrayInitializer_t>(*s.m_initializer)) {
                        AST::ArrayInitializer_t *temp_array =
                            AST::down_cast<AST::ArrayInitializer_t>(s.m_initializer);
                        // For case  `integer, parameter :: x(*) = [1,2,3], get the compile time length of RHS array.
                        Vec<ASR::dimension_t> temp_dims;
                        temp_dims.reserve(al, 1);
                        ASR::dimension_t temp_dim; temp_dim.m_length = nullptr; temp_dim.m_start = nullptr;
                        temp_dim.loc = (temp_array->base).base.loc;
                        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, (temp_array->base).base.loc, compiler_options.po.default_integer_kind));
                        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, (temp_array->base).base.loc, 1, int_type));
                        ASR::expr_t* x_n_args = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, (temp_array->base).base.loc, temp_array->n_args, int_type));
                        temp_dim.m_start = one;
                        temp_dim.m_length = x_n_args;
                        temp_dims.push_back(al, temp_dim);
                        type = ASRUtils::duplicate_type(al, type, &temp_dims);
                    }
                    init_expr = ASRUtils::EXPR(tmp);
                    value = ASRUtils::expr_value(init_expr);
                    bool PARAMETER_NOT_CONSTANT_EXCEPTION = // TODO : Fix that (look `array_reshape_21.f90`)
                        ASRUtils::is_array(ASRUtils::expr_type(init_expr))? true : false;
                    if( PARAMETER_NOT_CONSTANT_EXCEPTION == false &&
                        !value && 
                        storage_type == ASR::storage_typeType::Parameter){
                        diag.add(Diagnostic(
                            "RHS must reduce to a constant expression.",
                            Level::Error, Stage::Semantic, {
                                Label("",{init_expr->base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    // we do checks and correct length initialization for
                    // character (& character array) before creating repeated argument
                    // list for an initialization like:
                    // character(*) :: x(2) = "a", as we can assign "length" to
                    // character easily
                    if (is_char_type && storage_type == ASR::storage_typeType::Parameter) {
                        ASR::String_t *lhs_type = ASR::down_cast<ASR::String_t>(
                            ASRUtils::type_get_past_array(type));
                        ASR::String_t *rhs_type = ASR::down_cast<ASR::String_t>(
                            ASRUtils::type_get_past_array(ASRUtils::expr_type(value)));
                        int64_t lhs_len, rhs_len;
                        bool is_lhs_length_constant = ASRUtils::extract_value(lhs_type->m_len, lhs_len);
                        bool is_rhs_length_constant = ASRUtils::extract_value(rhs_type->m_len, rhs_len);
                        if( is_lhs_length_constant && is_rhs_length_constant ){
                            if((lhs_len < rhs_len)){
                                diag.semantic_warning_label(
                                    "The LHS character len="
                                        + std::to_string(lhs_len)
                                        + " and the RHS character len="
                                        + std::to_string(rhs_len)
                                        + " are not equal.",
                                    {x.base.base.loc},
                                    "help: consider changing the RHS character len to match the LHS character len"
                                );
                            }
                            if((lhs_len != rhs_len)) {
                                // Adjust character string by padding or trimming
                                // Notice that we only trim when variable is parameter, to have compile-time-correct string.
                                if (ASR::is_a<ASR::ArrayConstant_t>(*value)) {
                                    value = adjust_array_character_length(value, lhs_len,
                                        rhs_len, al);
                                } else {
                                    value = adjust_character_length(value, lhs_len,
                                        rhs_len, init_expr->base.loc, al);
                                }
                            }
                            
                        }
                        if(!is_rhs_length_constant){
                                diag.add(Diagnostic(
                                "The RHS character len must be known at compile time",
                                Level::Error, Stage::Semantic, {
                                    Label("",{rhs_type->base.base.loc})
                                }));
                            throw SemanticAbort();
                        }

                        if (lhs_type->m_len_kind == ASR::string_length_kindType::DeferredLength) {
                            diag.add(Diagnostic(
                                "The LHS character length must not be deferred (allocatable) in a parameter declaration",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        if(lhs_type->m_len_kind == ASR::string_length_kindType::AssumedLength){lhs_type->m_len = rhs_type->m_len;}
                    }

                    ASR::expr_t* tmp_init = init_expr;
                    if (value != nullptr) {
                        tmp_init = value;
                    }
                    if (!is_compile_time && ASR::is_a<ASR::Array_t>(*type)
                        && (ASR::is_a<ASR::IntegerConstant_t>(*tmp_init) || ASR::is_a<ASR::RealConstant_t>(*tmp_init)
                            || ASR::is_a<ASR::ComplexConstant_t>(*tmp_init) || ASR::is_a<ASR::RealUnaryMinus_t>(*tmp_init)
                            || ASR::is_a<ASR::IntegerUnaryMinus_t>(*tmp_init) || ASR::is_a<ASR::StringConstant_t>(*tmp_init)
                            || ASR::is_a<ASR::LogicalConstant_t>(*tmp_init)))
                    {
                        /*
                            Case: integer :: x(2) = 1
                            which is equivalent to x(2) = [1,1]
                        */
                        int64_t size = ASRUtils::get_fixed_size_of_array(type);
                        Vec<ASR::expr_t*> args;
                        args.reserve(al, size);
                        LCOMPILERS_ASSERT(tmp_init != nullptr)
                        // in case of declaration like:
                        // REAL :: x(2) = 1, we need to cast `tmp_init`
                        ImplicitCastRules::set_converted_value(
                            al, x.base.base.loc, &tmp_init,
                            ASRUtils::expr_type(tmp_init),
                            ASRUtils::type_get_past_allocatable(type), diag
                        );
                        for (int64_t i = 0; i < size; i++) {
                            args.push_back(al, tmp_init);
                        }
                        init_expr = ASRUtils::expr_value(
                            ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, init_expr->base.loc,
                                args.p, args.n, type, ASR::arraystorageType::ColMajor))
                        );
                        LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*init_expr));
                        value = init_expr;
                    }
                    if (!is_compile_time && ASR::is_a<ASR::Array_t>(*type)
                        && ASR::is_a<ASR::ArrayConstant_t>(*tmp_init)) {
                        ASR::Array_t* lhs_array = ASR::down_cast<ASR::Array_t>(type);
                        ASR::Array_t* rhs_array
                            = ASR::down_cast<ASR::Array_t>(ASRUtils::expr_type(tmp_init));
                        if (lhs_array->n_dims != rhs_array->n_dims) {
                            diag.add(Diagnostic("Incompatible ranks in assignment",
                                                Level::Error,
                                                Stage::Semantic,
                                                { Label("", { x.base.base.loc }) }));
                            throw SemanticAbort();
                        }
                    }
                    ASR::ttype_t *init_type = ASRUtils::expr_type(init_expr);
                    if (ASRUtils::is_real(*type) && ASRUtils::is_logical(*init_type)) {
                        diag.add(Diagnostic(
                            "Cannot convert LOGICAL to REAL",
                            Level::Error, Stage::Semantic, {
                                Label("", {init_expr->base.loc})
                            }));
                        throw SemanticAbort();
                    }
                    size_t rhs_rank = ASRUtils::extract_n_dims_from_ttype(init_type);
                    size_t lhs_rank = ASRUtils::extract_n_dims_from_ttype(type);

                    // when lhs_rank > rhs_rank it can broadcast
                    if( lhs_rank != rhs_rank && lhs_rank < rhs_rank ){
                        diag.add(Diagnostic(
                            "Incompatible ranks `"+ std::to_string(lhs_rank) + "` and `"
                                                                  + std::to_string(rhs_rank) + "` in assignment",
                            Level::Error, Stage::Semantic, {
                                Label("",{x.base.base.loc})
                            }));
                        throw SemanticAbort();
                    }

                     if ( ASR::is_a<ASR::Array_t>(*init_type) && ASR::is_a<ASR::ArrayReshape_t>(*init_expr) ){
                        ASR::Array_t* arr_rhs = ASR::down_cast<ASR::Array_t>(init_type);
                        ASR::Array_t* arr_lhs = ASR::down_cast<ASR::Array_t>(type);

                    for (size_t i = 0; i < arr_lhs->n_dims; i++) {
                           std::string lhs_dim = ASRUtils::extract_dim_value(arr_lhs->m_dims[i].m_length);
                           std::string rhs_dim = ASRUtils::extract_dim_value(arr_rhs->m_dims[i].m_length);
                            if(lhs_dim!=":" && rhs_dim!=":" && lhs_dim!=rhs_dim){
                                diag.add(Diagnostic(
                                    "Incompatible shape of array on assignment on dimension " + std::to_string(i) +
                                    " (" + lhs_dim + " and " + rhs_dim + ")",
                                    Level::Error, Stage::Semantic, {
                                        Label("",{x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                        }
                   }
                    if (init_type->type == ASR::ttypeType::Integer
                        && ASRUtils::type_get_past_array(ASRUtils::type_get_past_pointer(type))->type == ASR::ttypeType::String
                        && s.m_sym == AST::symbolType::Asterisk) {
                        /*
                            Case: character :: a*4
                            Here 4 represents the length of the character, which is an integer.
                        */
                        value = ASRUtils::expr_value(init_expr);
                        if (value == nullptr) {
                            diag.add(Diagnostic(
                                "Initialization of `" + std::string(x.m_syms[i].m_name) +
                                "` must reduce to a compile time constant.",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        if (ASR::is_a<ASR::IntegerConstant_t>(*value)) {
                            ASR::IntegerConstant_t *int_const = ASR::down_cast<ASR::IntegerConstant_t>(value);
                            int64_t len = int_const->m_n;
                            if (ASR::is_a<ASR::Array_t>(*type)) {
                                // case: character :: a(2)*4
                                ASR::Array_t *array = ASR::down_cast<ASR::Array_t>(type);
                                    array->m_type = ASRUtils::TYPE(ASR::make_String_t(al, int_const->base.base.loc, 1, 
                                        ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, int_const->base.base.loc, len,
                                            ASRUtils::TYPE(ASR::make_Integer_t(al, int_const->base.base.loc, 4)))),
                                        ASR::string_length_kindType::ExpressionLength,
                                        ASR::string_physical_typeType::PointerString));
                            } else {
                                type = ASRUtils::TYPE(ASR::make_String_t(al, int_const->base.base.loc, 1, 
                                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, int_const->base.base.loc, len,
                                        ASRUtils::TYPE(ASR::make_Integer_t(al, int_const->base.base.loc, 4)))),
                                    ASR::string_length_kindType::ExpressionLength,    
                                    ASR::string_physical_typeType::PointerString));
                            }
                        } else {
                            diag.add(Diagnostic(
                                "Initialization of `" + std::string(x.m_syms[i].m_name) +
                                "` must reduce to a compile time constant.",
                                Level::Error, Stage::Semantic, {
                                    Label("",{x.base.base.loc})
                                }));
                            throw SemanticAbort();
                        }
                        value = nullptr;
                        init_expr = nullptr;
                    } else if (!is_char_type) {
                        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &init_expr, init_type, type, diag);
                        LCOMPILERS_ASSERT(init_expr != nullptr);
                        value = ASRUtils::expr_value(init_expr);
                        if ( init_expr && !ASR::is_a<ASR::FunctionType_t>(*
                                ASRUtils::type_get_past_pointer(
                                    ASRUtils::expr_type(init_expr))) ) {
                            if( ASRUtils::is_value_constant(value) ) {
                            } else if( ASRUtils::is_value_constant(init_expr) ) {
                                if (ASR::is_a<ASR::Cast_t>(*init_expr)) {
                                    ASR::Cast_t *cast = ASR::down_cast<ASR::Cast_t>(init_expr);
                                    if (cast->m_arg && ASR::is_a<ASR::ArrayConstant_t>(*cast->m_arg)) {
                                        ASR::cast_kindType cast_kind = cast->m_kind;
                                        bool is_convertible = false;
                                        ASR::ArrayConstant_t *a = ASR::down_cast<ASR::ArrayConstant_t>(cast->m_arg);
                                        ASR::ttype_t* cast_type = cast->m_type;
                                        Vec<ASR::expr_t*> body;
                                        body.reserve(al, ASRUtils::get_fixed_size_of_array(a->m_type));
                                        if (cast_kind == ASR::cast_kindType::IntegerToReal) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
                                                    ASR::IntegerConstant_t *int_const = ASR::down_cast<ASR::IntegerConstant_t>(e);
                                                    double val = int_const->m_n;
                                                    ASR::expr_t *real_const = ASRUtils::EXPR(ASR::make_RealConstant_t(al, int_const->base.base.loc,
                                                        val, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, real_const);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::RealToInteger) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::RealConstant_t>(*e)) {
                                                    ASR::RealConstant_t *real_const = ASR::down_cast<ASR::RealConstant_t>(e);
                                                    int64_t val = real_const->m_r;
                                                    ASR::expr_t *int_const = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, real_const->base.base.loc,
                                                        val, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, int_const);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::RealToReal) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::RealConstant_t>(*e)) {
                                                    ASR::RealConstant_t *real_const = ASR::down_cast<ASR::RealConstant_t>(e);
                                                    int64_t val = real_const->m_r;
                                                    ASR::expr_t *real_const2 = ASRUtils::EXPR(ASR::make_RealConstant_t(al, real_const->base.base.loc,
                                                        val, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, real_const2);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::ComplexToInteger) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::ComplexConstant_t>(*e)) {
                                                    ASR::ComplexConstant_t *complex_const = ASR::down_cast<ASR::ComplexConstant_t>(e);
                                                    int64_t val = complex_const->m_re;
                                                    ASR::expr_t *integer_const = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, complex_const->base.base.loc,
                                                        val, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, integer_const);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::IntegerToComplex) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
                                                    ASR::IntegerConstant_t *integer_const = ASR::down_cast<ASR::IntegerConstant_t>(e);
                                                    int64_t val = integer_const->m_n;
                                                    double y_val_ = 0.0;
                                                    ASR::expr_t *complex_const = ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, integer_const->base.base.loc,
                                                        val, y_val_, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, complex_const);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::ComplexToReal) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::ComplexConstant_t>(*e)) {
                                                    ASR::ComplexConstant_t *complex_const = ASR::down_cast<ASR::ComplexConstant_t>(e);
                                                    int64_t val = complex_const->m_re;
                                                    ASR::expr_t *real_const = ASRUtils::EXPR(ASR::make_RealConstant_t(al, complex_const->base.base.loc,
                                                        val, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, real_const);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::ComplexToComplex) {
                                            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                if (ASR::is_a<ASR::ComplexConstant_t>(*e)) {
                                                    ASR::ComplexConstant_t *complex_const = ASR::down_cast<ASR::ComplexConstant_t>(e);
                                                    int64_t val1 = complex_const->m_re;
                                                    int64_t val2 = complex_const->m_im;
                                                    ASR::expr_t *complex_const2 = ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, complex_const->base.base.loc,
                                                        val1, val2, ASRUtils::type_get_past_array(cast_type)));
                                                    body.push_back(al, complex_const2);
                                                    is_convertible = true;
                                                } else {
                                                    break;
                                                }
                                            }
                                        } else if (cast_kind == ASR::cast_kindType::IntegerToLogical) {
                                            if (compiler_options.logical_casting) {
                                                for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                                    ASR::expr_t *e = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                                    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
                                                        ASR::IntegerConstant_t *int_const = ASR::down_cast<ASR::IntegerConstant_t>(e);
                                                        bool val = int_const->m_n;
                                                        ASR::expr_t *logical_const = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, int_const->base.base.loc,
                                                            val, ASRUtils::type_get_past_array(cast_type)));
                                                        body.push_back(al, logical_const);
                                                        is_convertible = true;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                            } else {
                                                diag.add(Diagnostic(
                                                    "Type mismatch in array initialization.\n Enable logical casting by setting `--logical-casting = true`",
                                                    Level::Error, Stage::Semantic, {
                                                        Label("",{x.base.base.loc})
                                                    }));
                                                throw SemanticAbort();
                                            }
                                        }
                                        if (is_convertible) {
                                                ASR::expr_t* array_const = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, a->base.base.loc, body.p, body.size(), cast_type, a->m_storage_format));
                                                cast->m_value = ASRUtils::expr_value(array_const);
                                                value = cast->m_value;
                                        } else {
                                            diag.add(Diagnostic(
                                                "Type mismatch in array initialization",
                                                Level::Error, Stage::Semantic, {
                                                    Label("",{x.base.base.loc})
                                                }));
                                            throw SemanticAbort();
                                        }
                                    }
                                } else {
                                    value = nullptr;
                                }
                            } else if ( ASR::is_a<ASR::ArrayConstructor_t>(*init_expr) ||
                                ( ASR::is_a<ASR::Cast_t>(*init_expr) &&
                                ASR::is_a<ASR::ArrayConstructor_t>(*ASR::down_cast<ASR::Cast_t>(init_expr)->m_arg) )
                                || ASR::is_a<ASR::IntrinsicElementalFunction_t>(*init_expr) ||
                                ASR::is_a<ASR::IntrinsicArrayFunction_t>(*init_expr) ||
                                ASR::is_a<ASR::TypeInquiry_t>(*init_expr) ||
                                ASR::is_a<ASR::StringLen_t>(*init_expr) ) {
                                value = init_expr;
                            } else if (ASR::is_a<ASR::IntegerBinOp_t>(*init_expr) || ASR::is_a<ASR::RealBinOp_t>(*init_expr) ||
                                        ASR::is_a<ASR::ComplexBinOp_t>(*init_expr)) {
                                value = init_expr;
                            } else if (ASR::is_a<ASR::ArrayReshape_t>(*init_expr) || ASR::is_a<ASR::BitCast_t>(*init_expr) ||
                                ASR::is_a<ASR::IntegerCompare_t>(*init_expr)) {
                                value = init_expr;
                            } else {
                                diag.add(Diagnostic(
                                    "Initialization of `" + std::string(x.m_syms[i].m_name) +
                                    "` must reduce to a compile time constant.",
                                    Level::Error, Stage::Semantic, {
                                        Label("",{x.base.base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                        }
                    }
                    if (storage_type == ASR::storage_typeType::Parameter) {
                        if( ASRUtils::is_array(type) ) {
                            ASR::array_physical_typeType var_ptype = ASRUtils::extract_physical_type(type);
                            ASR::array_physical_typeType init_expr_ptype = ASRUtils::extract_physical_type(
                                ASRUtils::expr_type(init_expr));
                            if( var_ptype != init_expr_ptype &&
                                var_ptype == ASR::array_physical_typeType::DescriptorArray ) {
                                type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(init_expr));
                            }
                        }
                        if( value ) {
                            // TODO: move this into `expr_value` itself:
                            if (ASR::is_a<ASR::ArrayConstant_t>(*value)) {
                                // For constant arrays we iterate over each element
                                // and copy over the value
                                ASR::ArrayConstant_t *a = ASR::down_cast<ASR::ArrayConstant_t>(value);
                                Vec<ASR::expr_t*> body;
                                body.reserve(al, ASRUtils::get_fixed_size_of_array(a->m_type));
                                for (size_t i=0; i < (size_t) ASRUtils::get_fixed_size_of_array(a->m_type); i++) {
                                    ASR::expr_t* a_m_args = ASRUtils::fetch_ArrayConstant_value(al, a, i);
                                    // if( a_m_args == nullptr ) {
                                    //     a_m_args = a->m_args[i];
                                    // }
                                    body.push_back(al, a_m_args);
                                }
                                value = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al,
                                    a->base.base.loc, body.p, body.size(),
                                    a->m_type, a->m_storage_format));
                                if (ASRUtils::is_dimension_empty(dims.p, dims.n)) {
                                    type = a->m_type;
                                }
                            }
                            if (ASR::is_a<ASR::ArrayConstructor_t>(*value)) {
                                // For constant arrays we iterate over each element
                                // and copy over the value
                                ASR::ArrayConstructor_t *a = ASR::down_cast<ASR::ArrayConstructor_t>(value);
                                Vec<ASR::expr_t*> body;
                                body.reserve(al, a->n_args);
                                for (size_t i=0; i < a->n_args; i++) {
                                    ASR::expr_t* a_m_args = ASRUtils::expr_value(a->m_args[i]);
                                    if( a_m_args == nullptr ) {
                                        a_m_args = a->m_args[i];
                                    }
                                    body.push_back(al, a_m_args);
                                }
                                value = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al,
                                    a->base.base.loc, body.p, body.size(),
                                    a->m_type, a->m_storage_format));
                                if (ASRUtils::is_dimension_empty(dims.p, dims.n)) {
                                    type = a->m_type;
                                }
                            }
                            if (ASR::is_a<ASR::StringLen_t>(*value)) {
                                ASR::StringLen_t *a = ASR::down_cast<ASR::StringLen_t>(value);
                                value = a->m_value;
                            }
                        }
                    } else {
                        implicit_save = true;
                        storage_type = ASR::storage_typeType::Save; // implicit save
                    }
                }
                if (is_Function && implicit_save && !is_save) {
                    // throw warning to that particular variable
                    if ( !is_global_save_enabled ) {
                        diag.semantic_warning_label(
                            "Assuming implicit save attribute for variable declaration",
                            {x.m_syms[i].loc},
                            "help: add explicit save attribute or parameter attribute or initialize in a separate statement"
                        );
                    }
                }
                if ( variable_added_to_symtab != nullptr ) {
                    variable_added_to_symtab->m_value = value;
                    variable_added_to_symtab->m_symbolic_value = init_expr;
                    variable_added_to_symtab->m_storage = storage_type;
                    if ( !is_implicitly_declared && !is_external) {
                        if ( symbols_having_only_attributes_without_type.find(sym) != symbols_having_only_attributes_without_type.end() ) {
                            ASR::symbol_t* symbol = symbols_having_only_attributes_without_type[sym];
                            ASR::Variable_t* symbol_variable = ASR::down_cast<ASR::Variable_t>(symbol);
                            if ( symbol_variable->m_type ) {
                                if ( ASR::is_a<ASR::Array_t>(*symbol_variable->m_type) ) {
                                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(symbol_variable->m_type);
                                    array_type->m_type = type;
                                } else {
                                    symbol_variable->m_type = type;
                                }
                            } else {
                                symbol_variable->m_type = type;
                            }
                            variable_added_to_symtab->m_type = symbol_variable->m_type;
                        } else {
                            variable_added_to_symtab->m_type = type;
                        }
                    } else {
                        variable_added_to_symtab->m_type = type;
                    }
                    SetChar variable_dependencies_vec;
                    variable_dependencies_vec.reserve(al, 1);
                    ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type, init_expr, value, sym);
                    variable_added_to_symtab->m_dependencies = variable_dependencies_vec.p;
                    variable_added_to_symtab->n_dependencies = variable_dependencies_vec.n;
                }
            } // for m_syms
        }
        _declaring_variable = false;
    }

    void visit_BlockData(const AST::BlockData_t&/*x*/) {

    }

    void visit_Interface(const AST::Interface_t &/*x*/) {

    }

    void visit_DeclarationPragma(const AST::DeclarationPragma_t &/*x*/) {

    }

    void visit_DerivedType(const AST::DerivedType_t &/*x*/) {

    }

    void visit_Enum(const AST::Enum_t &/*x*/) {

    }

    ASR::ttype_t* determine_type(const Location &loc, std::string& sym,
        AST::decl_attribute_t* decl_attribute, bool is_pointer,
        bool is_allocatable, Vec<ASR::dimension_t>& dims,
        ASR::symbol_t *&type_declaration, ASR::abiType abi, bool is_argument=false, bool is_dimension_star=false) {
        AST::AttrType_t *sym_type = AST::down_cast<AST::AttrType_t>(decl_attribute);
        ASR::ttype_t *type;
        type_declaration = nullptr;

        int a_kind = 4;
        if (sym_type->m_type == AST::decl_typeType::TypeInteger) {
            a_kind = compiler_options.po.default_integer_kind;
        }

        // general assignments and checks except when it's a
        // "Character" declaration
        if (sym_type->m_type != AST::decl_typeType::TypeCharacter &&
            sym_type->m_kind != nullptr
        ) {
            if (sym_type->m_kind->m_value) {
                this->visit_expr(*sym_type->m_kind->m_value);
                ASR::expr_t* kind_expr = ASRUtils::EXPR(tmp);
                a_kind = ASRUtils::extract_kind<SemanticAbort>(kind_expr, sym_type->m_kind->loc, diag);
            }
            // kind=* only allowed for "Character"
            else if (sym_type->m_kind->m_type == AST::kind_item_typeType::Star) {
                diag.add(Diagnostic(
                    "Expected initialization expression for kind",
                    Level::Error, Stage::Semantic, {
                        Label("",{sym_type->m_kind->loc})
                    }));
                throw SemanticAbort();
            }
        }
        if (sym_type->m_type == AST::decl_typeType::TypeReal) {
            if (a_kind != 4 && a_kind != 8) {
                diag.add(Diagnostic(
                    "Kind " + std::to_string(a_kind) + " is not supported for Real",
                    Level::Error, Stage::Semantic, {
                        Label("",{sym_type->m_kind->loc})
                    }));
                throw SemanticAbort();
            }
            type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, a_kind));
            type = ASRUtils::make_Array_t_util(al, loc, type, dims.p, dims.size(), abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeDoublePrecision) {
            a_kind = 8;
            type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, a_kind));
            type = ASRUtils::make_Array_t_util(al, loc, type, dims.p, dims.size(), abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeInteger) {
            if (a_kind != 1 && a_kind != 2 && a_kind != 4 && a_kind != 8) {
                diag.add(Diagnostic(
                    "Kind " + std::to_string(a_kind) + " is not supported for Integer",
                    Level::Error, Stage::Semantic, {
                        Label("",{sym_type->m_kind->loc})
                    }));
                throw SemanticAbort();
            }
            type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, a_kind));
            type = ASRUtils::make_Array_t_util(
                al, loc, type, dims.p, dims.size(), abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeLogical) {
            if (a_kind != 1 && a_kind != 2 && a_kind != 4 && a_kind != 8) {
                diag.add(Diagnostic(
                    "Kind " + std::to_string(a_kind) + " is not supported for Logical",
                    Level::Error, Stage::Semantic, {
                        Label("",{sym_type->m_kind->loc})
                    }));
                throw SemanticAbort();
            }
            // currently we change the kind's of all logical's to
            // 'default_integer_kind'. GFortran support's logical's of
            // different kind's, we need to think about this
            type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, compiler_options.po.default_integer_kind));
            type = ASRUtils::make_Array_t_util(
                al, loc, type, dims.p, dims.size(), abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeComplex) {
            if (a_kind != 4 && a_kind != 8) {
                diag.add(Diagnostic(
                    "Kind " + std::to_string(a_kind) + " is not supported for Complex",
                    Level::Error, Stage::Semantic, {
                        Label("",{sym_type->m_kind->loc})
                    }));
                throw SemanticAbort();
            }
            type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc, a_kind));
            type = ASRUtils::make_Array_t_util(
                al, loc, type, dims.p, dims.size(), abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeDoubleComplex) {
            a_kind = 8;
            type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc, a_kind));
            type = ASRUtils::make_Array_t_util(
                al, loc, type, dims.p, dims.size(), abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeCharacter) {
            a_kind = 1;
            // Create String type.
            type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, a_kind, nullptr,
                ASR::string_length_kindType::ExpressionLength, // Invalid state. Should be captured by ASR_Verify. 
                (is_allocatable && dims.size() == 0) ? 
                    ASR::string_physical_typeType::DescriptorString :
                        ASR::string_physical_typeType::PointerString));
            ASR::String_t* str = ASR::down_cast<ASR::String_t>(type);

            // Set length of the string -> [`:`, `*`, `expr`, None(default 1)]
            if (sym_type->m_kind == nullptr) {// Default len of "character :: x" is 1
                str->m_len = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
                str->m_len_kind = ASR::string_length_kindType::ExpressionLength;
            }


            LCOMPILERS_ASSERT(sym_type->n_kind < 3)

            if (sym_type->n_kind == 1) {
                const auto &item = sym_type->m_kind[0];
                std::string id = item.m_id ? to_lower(item.m_id) : "";

                if (id != "kind" && id != "len" && id != "") {
                    diag.add(Diagnostic(
                        "Syntax error in CHARACTER declaration: only 'len' and 'kind' are allowed as type parameters",
                        Level::Error, Stage::Semantic, {
                            Label("",{sym_type->base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                if (id == "kind") {
                    //TODO: Handle kind attribute on item (ideally should be a function call)
                    str->m_len = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
                    str->m_len_kind = ASR::string_length_kindType::ExpressionLength;
                } else {
                    determine_char_len(item, sym, str);
                }

            } else if (sym_type->n_kind == 2) {
                const auto &item1 = sym_type->m_kind[0];
                const auto &item2 = sym_type->m_kind[1];
                std::string id1 = item1.m_id ? to_lower(item1.m_id) : "";
                std::string id2 = item2.m_id ? to_lower(item2.m_id) : "";

                if ((id1 != "kind" && id1 != "len" && id1 != "") ||
                        (id2 != "kind" && id2 != "len" && id2 != "")) {
                    diag.add(Diagnostic(
                        "Syntax error in CHARACTER declaration: only 'len' and 'kind' are allowed as keyword arguments",
                        Level::Error, Stage::Semantic, {
                            Label("",{sym_type->base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                // character(kind=x, kind=y) or character(len=x, len=y)
                if (id1 == id2 && id1 != "") {
                    diag.add(Diagnostic(
                        "Syntax error in CHARACTER declaration: can't use a keyword argument more than once",
                        Level::Error, Stage::Semantic, {
                            Label("",{sym_type->base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                // character(len=x, y) or character(kind=x, y)
                if (id1 != "" && id2 == "") {
                    diag.add(Diagnostic(
                        "Syntax error in CHARACTER declaration: positional type parameters cannot follow a keyword argument",
                        Level::Error, Stage::Semantic, {
                            Label("",{sym_type->base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                // character(x, len=y)
                if (id1 == "" && id2 == "len") {
                    diag.add(Diagnostic(
                        "Syntax error in CHARACTER declaration: using only 'len' keyword argument after a positional type is invalid",
                        Level::Error, Stage::Semantic, {
                            Label("",{sym_type->base.base.loc})
                        }));
                    throw SemanticAbort();
                }

                if (id1 == "kind" && id2 == "len") {
                    determine_char_len(item2, sym, str);

                    //TODO: Handle kind attribute on item1 (ideally should be a function call)
                } else {
                    determine_char_len(item1, sym, str);

                    //TODO: Handle kind attribute on item2 (ideally should be a function call)
                }
            }

            type = ASRUtils::make_Array_t_util(
                al, loc, type, dims.p, dims.size(), abi, is_argument,
                dims.size() > 0 && abi == ASR::abiType::BindC ? ASR::array_physical_typeType::StringArraySinglePointer :
                                ASRUtils::is_fixed_size_array(dims.p, dims.n) ? ASR::array_physical_typeType::FixedSizeArray :
                                ASR::array_physical_typeType::DescriptorArray,
                dims.size() > 0 ? true : false);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeType) {
            if (sym_type->m_attr) {
                return determine_type(loc, sym, sym_type->m_attr,
                    is_pointer, is_allocatable, dims, type_declaration, abi,
                    is_argument);
            }
            if (!sym_type->m_name) {
                diag.add(Diagnostic(
                    "Type must have a name",
                    Level::Error, Stage::Semantic, {
                        Label("",{loc})
                    }));
                throw SemanticAbort();
            }
            std::string derived_type_name = to_lower(sym_type->m_name);
            if (derived_type_name == "integer") {
                sym_type->m_type = AST::decl_typeType::TypeInteger;
                return determine_type(loc, sym, decl_attribute, is_pointer,
                    is_allocatable, dims, type_declaration, abi, is_argument);
            } else if (derived_type_name == "real") {
                sym_type->m_type = AST::decl_typeType::TypeReal;
                return determine_type(loc, sym, decl_attribute, is_pointer,
                    is_allocatable, dims, type_declaration, abi, is_argument);
            } else if (derived_type_name == "complex") {
                sym_type->m_type = AST::decl_typeType::TypeComplex;
                return determine_type(loc, sym, decl_attribute, is_pointer,
                    is_allocatable, dims, type_declaration, abi, is_argument);
            } else if (derived_type_name == "logical") {
                sym_type->m_type = AST::decl_typeType::TypeLogical;
                return determine_type(loc, sym, decl_attribute, is_pointer,
                    is_allocatable, dims, type_declaration, abi, is_argument);
            } else if (derived_type_name == "character") {
                sym_type->m_type = AST::decl_typeType::TypeCharacter;
                return determine_type(loc, sym, decl_attribute, is_pointer,
                    is_allocatable, dims, type_declaration, abi, is_argument);
            } else if (startswith(derived_type_name, "_lfortran_")) {
                // LFortran-specific intrinsics 

                if (derived_type_name == "_lfortran_list_integer") 
                    return ASRUtils::TYPE(ASR::make_List_t(al, loc, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))); 
                else if (derived_type_name == "_lfortran_list_real") 
                    return ASRUtils::TYPE(ASR::make_List_t(al, loc, ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4))));
                else if (derived_type_name == "_lfortran_set_integer") 
                    return ASRUtils::TYPE(ASR::make_Set_t(al, loc, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))); 
            }
            ASR::symbol_t* v = current_scope->resolve_symbol(derived_type_name);
            if (v && ASR::is_a<ASR::Variable_t>(*v)
                  && ASR::is_a<ASR::TypeParameter_t>(*
                    ASRUtils::type_get_past_array(
                        ASR::down_cast<ASR::Variable_t>(v)->m_type))) {
                type = ASRUtils::TYPE(ASR::make_TypeParameter_t(al, loc,
                                        s2c(al, derived_type_name)));
                type = ASRUtils::make_Array_t_util(
                    al, loc, type, dims.p, dims.size(), abi, is_argument);
            } else if (v && ASRUtils::is_c_ptr(v, derived_type_name)) {
                type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
                type = ASRUtils::make_Array_t_util(
                    al, loc, type, dims.p, dims.size(), abi, is_argument,
                    ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star
                );
            } else if (v && ASRUtils::is_c_funptr(v, derived_type_name)) {
                type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
            } else {
                if (!v) {
                    if (is_template) {
                        diag.add(Diagnostic(
                            "Type parameter '" + derived_type_name + "' is not specified "
                            "in any requirements",
                            Level::Error, Stage::Semantic, {
                                Label("",{loc})
                            }));
                        throw SemanticAbort();
                    }
                    // Placeholder symbol for StructType type
                    // Derived type can be used before its actually defined
                    v = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                            al, loc, current_scope, s2c(al, derived_type_name),
                            nullptr, nullptr, nullptr, 0, s2c(al, derived_type_name),
                            ASR::accessType::Private));
                }
                // type = ASRUtils::TYPE(ASRUtils::make_StructType_t_util(al, loc, v));
                type = ASRUtils::TYPE(ASR::make_StructType_t(al, loc, nullptr, 0, nullptr, 0, true, v));
                type = ASRUtils::make_Array_t_util(
                    al, loc, type, dims.p, dims.size(), abi, is_argument);
                if (is_pointer) {
                    type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                        type));
                }
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeClass) {
            std::string derived_type_name;
            if( !sym_type->m_name ) {
                derived_type_name = "~abstract_type";
            } else {
                derived_type_name = to_lower(sym_type->m_name);
            }
            ASR::symbol_t *v = current_scope->resolve_symbol(derived_type_name);
            if( !v ) {
                if( derived_type_name != "~abstract_type" ) {
                    diag.add(Diagnostic(
                        "Derived type '" + derived_type_name
                        + "' not declared",
                        Level::Error, Stage::Semantic, {
                            Label("",{loc})
                        }));
                    throw SemanticAbort();
                }
                SymbolTable *parent_scope = current_scope;
                current_scope = al.make_new<SymbolTable>(parent_scope);
                ASR::asr_t* dtype = ASR::make_Struct_t(al, loc, current_scope,
                                                s2c(al, to_lower(derived_type_name)), nullptr, 0, nullptr, 0,
                                                nullptr, 0, ASR::abiType::Source, dflt_access, false, true,
                                                nullptr, 0, nullptr, nullptr);
                v = ASR::down_cast<ASR::symbol_t>(dtype);
                parent_scope->add_symbol(derived_type_name, v);
                current_scope = parent_scope;
            }
            // this is class variable declaration
            type = ASRUtils::TYPE(ASRUtils::make_StructType_t_util(al, loc, v, false));
            type = ASRUtils::make_Array_t_util(
                al, loc, type, dims.p, dims.size(), abi, is_argument);
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(type)));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeProcedure) {
            std::string func_name = to_lower(sym_type->m_name);
            ASR::symbol_t *v = current_scope->resolve_symbol(func_name);
            if( !v ) {
                diag.add(Diagnostic(
                    "Procedure type '" + func_name
                    + "' not declared",
                    Level::Error, Stage::Semantic, {
                        Label("",{loc})
                    }));
                throw SemanticAbort();
            }
            type_declaration = v;
            v = ASRUtils::symbol_get_past_external(v);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*v));
            type = ASR::down_cast<ASR::Function_t>(v)->m_function_signature;
        } else {
            diag.add(Diagnostic("Type not implemented yet.",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }

        if( is_allocatable ) {
            type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc,
                ASRUtils::type_get_past_allocatable(type)));
        }

        return type;
    }


    ASR::asr_t* create_DerivedTypeConstructor(const Location &loc,
            AST::fnarg_t* m_args, size_t n_args, AST::keyword_t* kwargs,
            size_t n_kwargs, ASR::symbol_t *v, bool is_const = false) {
        Vec<ASR::call_arg_t> vals;
        visit_expr_list(m_args, n_args, vals);
        if(ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(v))){
            ASR::Struct_t* struct_= ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(v));
            size_t iter = 0 ;
            for (auto item : struct_->m_symtab->get_scope()){
                if(iter >= vals.size()) break;
                    // TO DO :: check type match. Use code below as reference.
                    // if(!ASRUtils::check_equal_type(
                    //     ASRUtils::symbol_type(item.second), ASRUtils::expr_type(vals[iter].m_value))){
                    //     diag.add(Diagnostic("Type mismatch in derived type constructor",
                    //         Level::Error, Stage::Semantic, {Label("", {vals[iter].loc})} ));
                    //     throw SemanticAbort();
                    // }
                // Cast RHS and LHS to have the same string physical type (LHS rules).
                if(ASR::is_a<ASR::Variable_t>(*item.second)){
                    ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(item.second);
                    if( ASRUtils::is_descriptorString(var->m_type) &&
                        ASRUtils::is_character(*ASRUtils::expr_type(vals[iter].m_value)) &&
                        !ASRUtils::is_descriptorString(ASRUtils::expr_type(vals[iter].m_value))){ // DescriptorString(LHS), PointerString (RHS)
                        ASR::call_arg_t* passed_arg = const_cast<ASR::call_arg_t*>(&vals[iter]);
                        passed_arg->m_value = ASRUtils::cast_string_pointer_to_descriptor(al, passed_arg->m_value);
                    }
                    if( ASRUtils::is_character(*var->m_type) &&
                        !ASRUtils::is_descriptorString(var->m_type) &&
                        ASRUtils::is_descriptorString(ASRUtils::expr_type(vals[iter].m_value))){ // PointerString(LHS), DescriptorString (RHS)
                        ASR::call_arg_t* passed_arg = const_cast<ASR::call_arg_t*>(&vals[iter]);
                        passed_arg->m_value = ASRUtils::cast_string_descriptor_to_pointer(al, passed_arg->m_value);
                    }
                }
                ++iter;
            }
        }
        visit_kwargs(vals, kwargs, n_kwargs, loc, v, diag);
        ASR::ttype_t* der = ASRUtils::TYPE(
                            ASRUtils::make_StructType_t_util(al, loc, v));

        // Ensure all values are present and are constant before creating StructConstant
        for (const auto& val : vals) {
            if (!val.m_value ||
                    !(ASRUtils::is_value_constant(val.m_value) ||
                      ASRUtils::is_value_constant(ASRUtils::expr_value(val.m_value)))) {
                    is_const = false;
                    break;
            }
        }
        if (is_const) {
           return ASR::make_StructConstant_t(al, loc,
                    v, vals.p, vals.size(), der);
        }
        return ASR::make_StructConstructor_t(al, loc,
                v, vals.p, vals.size(), der, nullptr);
    }

    int get_based_indexing(ASR::symbol_t* v) {
        if (v != nullptr && ASR::is_a<ASR::Variable_t>(*v)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(v);
            if (ASRUtils::is_array(var->m_type) && var->m_value && var->m_storage == ASR::storage_typeType::Parameter) {
                ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(var->m_type);
                for (size_t i = 0; i < arr->n_dims; i++) {
                    ASR::dimension_t dim = arr->m_dims[i];
                    if (dim.m_start != nullptr) {
                        ASR::expr_t *start = ASRUtils::expr_value(dim.m_start);
                        if (start) {
                            ASR::IntegerConstant_t *start2 = ASR::down_cast<ASR::IntegerConstant_t>(start);
                            return start2->m_n;
                        }
                    }
                }
            }
        }
        return 1; // default
    }

    void create_and_replace_structType() {

        class StructTypeVisitor : public ASR::BaseWalkVisitor<StructTypeVisitor> {
            private:
                Allocator &al;

            public:
                int sem = -1;
                SymbolTable* local_current_scope;
                StructTypeVisitor (Allocator &_al, SymbolTable* current_scope) : al(_al) {
                    local_current_scope = current_scope;
                    sem = -1;
                }

                void visit_Module(const ASR::Module_t &x) {
                    for (auto &a : x.m_symtab->get_scope()) {
                        if ( ASR::is_a<ASR::Function_t>(*a.second) ) {
                            ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(a.second);
                            SymbolTable* local_current_scope_copy = local_current_scope;
                            local_current_scope = f->m_symtab;
                            this->visit_Function(*f);
                            local_current_scope = local_current_scope_copy;
                        } else {
                            this->visit_symbol(*a.second);
                        }
                    }
                }

                void visit_Program(const ASR::Program_t &x) {
                    for (auto &a : x.m_symtab->get_scope()) {
                        if ( ASR::is_a<ASR::Function_t>(*a.second) ) {
                            ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(a.second);
                            SymbolTable* local_current_scope_copy = local_current_scope;
                            local_current_scope = f->m_symtab;
                            this->visit_Function(*f);
                            local_current_scope = local_current_scope_copy;
                        } else {
                            this->visit_symbol(*a.second);
                        }
                    }
                    for (size_t i=0; i<x.n_body; i++) {
                        this->visit_stmt(*x.m_body[i]);
                    }
                }

                void visit_Struct( const ASR::Struct_t& x ) {
                    SymbolTable* local_current_scope_copy = local_current_scope;
                    local_current_scope = x.m_symtab;
                    for (auto &a : x.m_symtab->get_scope()) {
                        this->visit_symbol(*a.second);
                    }
                    for (size_t i=0; i<x.n_initializers; i++) {
                        this->visit_call_arg(x.m_initializers[i]);
                    }
                    if (x.m_alignment) {
                        this->visit_expr(*x.m_alignment);
                    }
                    local_current_scope = local_current_scope_copy;
                }

                void visit_Array( const ASR::Array_t& x ) {
                    if ( ASR::is_a<ASR::StructType_t>(*x.m_type) && !ASRUtils::is_class_type(x.m_type) ) {
                        sem += 1;
                        visit_StructType(*ASR::down_cast<ASR::StructType_t>(x.m_type));
                        sem -= 1;
                    }

                }

                void visit_FunctionType( const ASR::FunctionType_t& x ) {
                    for (size_t i=0; i<x.n_arg_types; i++) {
                        this->visit_ttype(*x.m_arg_types[i]);
                    }
                    if (x.m_return_var_type)
                        this->visit_ttype(*x.m_return_var_type);
                }

                void visit_ArrayItem(const ASR::ArrayItem_t& x) {
                    if ( ASR::is_a<ASR::StructType_t>(*x.m_type) && !ASRUtils::is_class_type(x.m_type) ) {
                        sem += 1;
                        visit_StructType(*ASR::down_cast<ASR::StructType_t>(x.m_type));
                        sem -= 1;
                    }
                    this->visit_expr(*x.m_v);
                }

                void visit_StructType( const ASR::StructType_t& x ) {
                    if ( sem >= 0 ) {
                        ASR::StructType_t& xx = const_cast<ASR::StructType_t&>(x);
                        if ( ASRUtils::symbol_parent_symtab(xx.m_derived_type)->counter != local_current_scope->counter) {
                            if ((local_current_scope->resolve_symbol(ASRUtils::symbol_name(xx.m_derived_type)) == nullptr)) {
                                ASRUtils::SymbolDuplicator sd(al);
                                sd.duplicate_symbol(xx.m_derived_type, local_current_scope);
                            }
                            xx.m_derived_type = local_current_scope->resolve_symbol(ASRUtils::symbol_name(xx.m_derived_type));
                        }
                    }
                }
        };

        StructTypeVisitor v(al, current_scope);
        ASR::asr_t* asr_owner = current_scope->asr_owner;
        if ( ASR::is_a<ASR::symbol_t>(*asr_owner) ) {
            ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(asr_owner);
            if ( ASR::is_a<ASR::Function_t>(*sym) ) {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sym);
                v.visit_Function(*f);
            } else if ( ASR::is_a<ASR::Program_t>(*sym) ) {
                ASR::Program_t* p = ASR::down_cast<ASR::Program_t>(sym);
                v.visit_Program(*p);
            } else if ( ASR::is_a<ASR::Module_t>(*sym) ) {
                ASR::Module_t* m = ASR::down_cast<ASR::Module_t>(sym);
                v.visit_Module(*m);
            }
        }
    }

    ASR::asr_t* create_ArrayRef(const Location &loc, AST::fnarg_t* m_args,
        size_t n_args, AST::fnarg_t* m_subargs, size_t n_subargs,
        ASR::expr_t* v_expr, ASR::symbol_t *v, ASR::symbol_t *f2) {
        ASR::ttype_t* root_v_type = ASRUtils::type_get_past_pointer(
            ASRUtils::symbol_type(v));
        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(root_v_type);
        if (ASRUtils::is_array(root_v_type) && n_dims != n_args) {
            std::string var_name = ASRUtils::symbol_name(v);
            diag.add(Diagnostic(
                "Rank mismatch in array reference: the array `"
                + var_name + "` has rank `" + std::to_string(n_dims) +
                "`, but is referenced as rank `" + std::to_string(n_args) + "`",
                Level::Error, Stage::Semantic, {
                    Label("",{loc})
                }));
            throw SemanticAbort();
        }
        bool is_item = true;
        Vec<ASR::array_index_t> args;
        args.reserve(al, n_args);
        ASR::expr_t* v_Var = nullptr;
        if( v_expr ) {
            ASR::ttype_t* struct_t_mem_type = ASRUtils::symbol_type(v);
            ASR::symbol_t* v_ext = ASRUtils::import_struct_instance_member(al, v, current_scope, struct_t_mem_type);
            v_Var = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(
                        al, v_expr->base.loc, v_expr, v_ext,
                        ASRUtils::fix_scoped_type(al, struct_t_mem_type, current_scope), nullptr));
        } else {
            v_Var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, v));
        }
        for (size_t i=0; i<n_args; i++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ASR::expr_t *m_start, *m_end, *m_step;
            m_start = m_end = m_step = nullptr;
            if (m_args[i].m_start != nullptr) {
                this->visit_expr(*(m_args[i].m_start));
                m_start = ASRUtils::EXPR(tmp);
                ai.loc = m_start->base.loc;
            }
            if (m_args[i].m_end != nullptr) {
                this->visit_expr(*(m_args[i].m_end));
                m_end = ASRUtils::EXPR(tmp);
                ai.loc = m_end->base.loc;
            } else {
                if(ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_allocatable_pointer(root_v_type))) {
                    ASR::String_t* char_type = ASR::down_cast<ASR::String_t>(
                        ASRUtils::extract_type(ASRUtils::symbol_type(v)));
                    int64_t m_len_value; // Dummy to just use below.
                    bool is_comp_time_value =  ASRUtils::extract_value(char_type->m_len, m_len_value);
                    if( is_comp_time_value ) {
                        m_end = ASRUtils::expr_value(char_type->m_len);
                    } else {
                        m_end = ASRUtils::EXPR(ASR::make_StringLen_t(al, loc,
                            v_Var, ASRUtils::TYPE(
                                ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind)),
                            nullptr));
                    }
                } else {
                    // LCOMPILERS_ASSERT(ASRUtils::is_array(root_v_type))
                    m_end = ASRUtils::get_bound<SemanticAbort>(v_Var, i + 1, "ubound", al, diag);
                }
            }
            if (m_args[i].m_step != nullptr) {
                this->visit_expr(*(m_args[i].m_step));
                m_step = ASRUtils::EXPR(tmp);
                ai.loc = m_step->base.loc;
            }
            if( m_start != nullptr &&
                ASR::is_a<ASR::Var_t>(*m_start) &&
                ASR::is_a<ASR::Var_t>(*m_end) ) {
                ASR::Variable_t* startv = ASRUtils::EXPR2VAR(m_start);
                ASR::Variable_t* endv = ASRUtils::EXPR2VAR(m_end);
                is_item = is_item && (startv == endv);
                if( is_item ) {
                    m_start = nullptr;
                    m_step = nullptr;
                }
            } else {
                is_item = is_item && (m_start == nullptr &&
                                      m_step == nullptr &&
                                      m_end != nullptr);
            }
            ai.m_left = m_start;
            ai.m_right = m_end;
            ai.m_step = m_step;
            args.push_back(al, ai);
        }

        ASR::ttype_t *type;
        type = ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(f2));
        ASR::expr_t *arr_ref_val = nullptr;
        bool all_args_eval = ASRUtils::all_args_evaluated(args);
        Vec<ASR::dimension_t> res_dims_vec;
        res_dims_vec.reserve(al, n_args);
        bool is_arg_array = false;
        for( auto& a : args ) {
            // Assume that indices are constant integers
            int64_t start = 1, end = -1, step = 1;
            bool flag = false;
            if( a.m_left ) {
                if( all_args_eval ) {
                    ASR::expr_t* m_left_expr = ASRUtils::expr_value(a.m_left);
                    if (!ASR::is_a<ASR::IntegerConstant_t>(*m_left_expr)) {
                        diag.add(Diagnostic("Substring start index at must be of type integer",
                            Level::Error, Stage::Semantic, {Label("", {m_left_expr->base.loc})}));
                        throw SemanticAbort();
                    }
                    ASR::IntegerConstant_t *m_left = ASR::down_cast<ASR::IntegerConstant_t>(m_left_expr);
                    start = m_left->m_n;
                }
            }
            if( a.m_right ) {
                if (ASRUtils::is_array(ASRUtils::expr_type(a.m_right))) {
                    is_arg_array = true;
                    ASR::dimension_t* arg_dim = nullptr;
                    if (!(ASRUtils::extract_dimensions_from_ttype(
                            ASRUtils::expr_type(a.m_right), arg_dim) == 1)) {
                        diag.add(Diagnostic("Array index must be of rank 1",
                            Level::Error, Stage::Semantic, {Label("", {a.m_right->base.loc})}));
                        throw SemanticAbort();
                    }
                    res_dims_vec.push_back(al, arg_dim[0]);
                } else if( all_args_eval ) {
                    flag = true;
                    ASR::expr_t* m_right_expr = ASRUtils::expr_value(a.m_right);
                    if(!ASR::is_a<ASR::IntegerConstant_t>(*m_right_expr)) {
                        diag.add(Diagnostic("Substring end index at must be of type integer",
                            Level::Error, Stage::Semantic, {Label("", {m_right_expr->base.loc})}));
                        throw SemanticAbort();
                    }
                    ASR::IntegerConstant_t *m_right = ASR::down_cast<ASR::IntegerConstant_t>(m_right_expr);
                    end = m_right->m_n;
                }
            }
            if( a.m_step ) {
                if( all_args_eval ) {
                    ASR::expr_t* m_step_expr = ASRUtils::expr_value(a.m_step);
                    if(!ASR::is_a<ASR::IntegerConstant_t>(*m_step_expr)) {
                        diag.add(Diagnostic("Substring stride must be of type integer",
                            Level::Error, Stage::Semantic, {Label("", {m_step_expr->base.loc})}));
                        throw SemanticAbort();
                    }
                    ASR::IntegerConstant_t *m_step = ASR::down_cast<ASR::IntegerConstant_t>(m_step_expr);
                    step = m_step->m_n;
                }
            }
            if( v->type == ASR::symbolType::Variable ) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v);
                ASR::expr_t *m_value = var->m_value;
                if( m_value && m_value->type == ASR::exprType::StringConstant ) {
                    ASR::StringConstant_t *m_str = ASR::down_cast<ASR::StringConstant_t>(m_value);
                    ASR::String_t* s_type = ASR::down_cast<ASR::String_t>(
                        ASRUtils::type_get_past_allocatable_pointer(var->m_type));
                    std::string sliced_str;
                    int64_t str_length; // Set by function below.
                    if(!ASRUtils::extract_value(s_type->m_len, str_length)){LCOMPILERS_ASSERT(false)}
                    if( start <= 0 ) {
                        diag.add(Diagnostic("Substring `start` is less than one",
                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    if(end > str_length) {
                        diag.add(Diagnostic("Substring end index exceeds the string length",
                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    if( end == -1 && !flag ) {
                        end = str_length;
                    } else {
                        for( int i = start - 1; i < end; i += step ) {
                            sliced_str.push_back(m_str->m_s[i]);
                        }
                        Str l_str;
                        l_str.from_str(al, sliced_str);
                        arr_ref_val = ASRUtils::EXPR(ASR::make_StringConstant_t(al, loc, l_str.c_str(al), m_str->m_type));
                    }
                }
            }
        }

        if( is_item ) {
            Vec<ASR::dimension_t> empty_dims;
            empty_dims.reserve(al, 1);
            if (ASR::is_a<ASR::StructInstanceMember_t>(*v_Var)) {
                type = ASR::down_cast<ASR::StructInstanceMember_t>(v_Var)->m_type;
            }
            type = ASRUtils::duplicate_type(al, type, &empty_dims);
            if (arr_ref_val == nullptr) {
                // For now we will only handle 1D arrays
                if (args.size() == 1) {
                    ASR::array_index_t arg = args[0];
                    if (arg.m_left == nullptr && arg.m_step == nullptr) {
                        ASR::expr_t *val = ASRUtils::expr_value(v_Var);
                        ASR::expr_t *index = ASRUtils::expr_value(arg.m_right);
                        if (val && index) {
                            val = ASRUtils::expr_value(val);
                            ASR::ArrayConstant_t *val2 = ASR::down_cast<ASR::ArrayConstant_t>(val);
                            ASR::IntegerConstant_t *index2 = ASR::down_cast<ASR::IntegerConstant_t>(index);
                            int based_indexing = get_based_indexing(v);
                            int64_t index3 = index2->m_n-based_indexing;
                            size_t index4 = index3;
                            if (index3 < 0 || index4 >= (size_t) ASRUtils::get_fixed_size_of_array(val2->m_type)) {
                                diag.add(Diagnostic("The index is out of bounds",
                                    Level::Error, Stage::Semantic, {Label("", {index2->base.base.loc})}));
                                throw SemanticAbort();
                            }
                            arr_ref_val = ASRUtils::fetch_ArrayConstant_value(al, val2, index4);
                        }
                    }
                }
            }
            if( ASRUtils::is_character(*root_v_type) &&
                !ASRUtils::is_array(root_v_type) ) {
                ASR::ttype_t  *char_type = ASRUtils::TYPE(ASR::make_String_t(
                    al, type->base.loc, 1,
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, type->base.loc, 1,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, type->base.loc, 4)))),
                        ASR::string_length_kindType::ExpressionLength,
                    ASR::string_physical_typeType::PointerString));
                if(ASRUtils::is_descriptorString(ASRUtils::expr_type(v_Var))){
                    v_Var = ASRUtils::cast_string_descriptor_to_pointer(al, v_Var);
                }
                return ASR::make_StringItem_t(al, loc,
                    v_Var, args.p[0].m_right, char_type, arr_ref_val);
            } else if ( ASRUtils::is_character(*root_v_type) &&
                        ASRUtils::is_array(root_v_type) &&
                        n_subargs > 0) {
                ASR::expr_t* array_item = replace_with_common_block_variables(ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                    v_Var, args.p, args.size(), ASRUtils::type_get_past_pointer(
                        ASRUtils::type_get_past_allocatable(type)),
                    ASR::arraystorageType::ColMajor, arr_ref_val)));
                LCOMPILERS_ASSERT(n_subargs == 1);
                ASR::ttype_t *char_type = ASRUtils::type_get_past_allocatable(type);
                ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
                ASR::expr_t* const_1 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                                            1, int_type));
                ASR::expr_t *l = nullptr, *r = nullptr, *step = nullptr;
                if (m_subargs[0].m_start) {
                    this->visit_expr(*(m_subargs[0].m_start));
                    l = ASRUtils::EXPR(tmp);
                    l = CastingUtil::perform_casting(l, int_type, al, loc);
                    l = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l,
                            ASR::binopType::Sub, const_1, int_type, nullptr));
                }
                if (m_subargs[0].m_end) {
                    this->visit_expr(*(m_subargs[0].m_end));
                    r = ASRUtils::EXPR(tmp);
                    r = CastingUtil::perform_casting(r, int_type, al, loc);
                }
                this->visit_expr(*(m_subargs[0].m_step));
                step = ASRUtils::EXPR(tmp);
                step = CastingUtil::perform_casting(step, int_type, al, loc);
                return ASR::make_StringSection_t(al, loc, array_item, l,
                        r, ASRUtils::EXPR(tmp), char_type, arr_ref_val);
            } else {
                ASR::ttype_t* final_type;
                if (is_arg_array) {
                  ASR::ttype_t *op_type = ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_Var));
                  final_type = ASRUtils::duplicate_type(al, op_type, &res_dims_vec);
                } else {
                  final_type = ASRUtils::type_get_past_pointer(
                        ASRUtils::type_get_past_allocatable(type));
                }
                if ( current_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner) &&
                    !ASR::is_a<ASR::Block_t>(*ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner)) &&
                    !ASRUtils::is_array(ASRUtils::expr_type(v_Var))) {
                    diag.add(Diagnostic("Array reference is not allowed on scalar variable",
                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
                return (ASR::asr_t*) replace_with_common_block_variables(ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                    v_Var, args.p, args.size(), final_type,
                    ASR::arraystorageType::ColMajor, arr_ref_val)));
            }
        } else {
            ASR::ttype_t *v_type = ASRUtils::symbol_type(v);
            if (ASR::is_a<ASR::Pointer_t>(*v_type)) {
                v_type = ASR::down_cast<ASR::Pointer_t>(v_type)->m_type;
            }
            if (ASRUtils::is_character(*v_type)) {
                int dims = ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::type_get_past_allocatable(v_type));
                if (dims == 0) {
                    // this is the case of String Section (or slicing)
                    LCOMPILERS_ASSERT(n_args == 1);
                    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
                    ASR::expr_t *l = nullptr, *r = nullptr;

                    if (m_args[0].m_start) {
                        // use 0 based indexing for string slice, so subtract 1 from left index
                        int32_t offset = 1;
                        ASRUtils::ASRBuilder b(al, loc);
                        ASR::expr_t* const_1 = b.i_t(offset, int_type);
                        ASR::expr_t* a_value = nullptr;
                        if (ASR::is_a<ASR::IntegerConstant_t>(*args[0].m_left)) {
                            int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(
                                            ASRUtils::expr_value(args[0].m_left))->m_n - offset;
                            a_value = ASRUtils::EXPR((ASR::make_IntegerConstant_t(al, loc,
                                                    a, int_type)));
                        }
                        ASR::expr_t* value = ASRUtils::expr_value(args[0].m_left);
                        if ( value ) {
                            int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n - offset;
                            a_value = ASRUtils::EXPR((ASR::make_IntegerConstant_t(al, loc,
                                                a, int_type)));
                        }
                        if ( a_value != nullptr ) {
                            int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(a_value)->m_n;
                            if ( a < 0 ) {
                                diag.add(Diagnostic("The first index in string section is less than 1",
                                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                                throw SemanticAbort();
                            }
                        }

                        ASR::expr_t* casted_left = CastingUtil::perform_casting(args[0].m_left, int_type, al, loc);
                        l = b.Sub(casted_left, const_1, a_value);
                    }
                    if (m_args[0].m_end) {
                        r = args[0].m_right;
                        r = CastingUtil::perform_casting(r, int_type, al, loc);
                    }
                    ASR::expr_t* casted_step = CastingUtil::perform_casting(args[0].m_step, int_type, al, loc);
                    ASR::ttype_t *char_type = nullptr;
                    if (arr_ref_val) {
                        char_type = ASRUtils::expr_type(arr_ref_val);
                    } else {
                        ASR::expr_t *a_len_expr = nullptr;
                        if (l && r) {
                            // TODO: Handle `args[0].m_step`
                            ASRUtils::ASRBuilder b(al, loc);
                            a_len_expr = b.Sub(r, l);
                        }
                        if(!l && !r){
                            // No runtime slicing is required.
                            // Use the actual type of lhs as the type of the resulting expr from string slicing operation.
                            char_type = ASRUtils::TYPE(ASR::make_String_t(al, loc, 1, a_len_expr,
                                a_len_expr?
                                    ASR::string_length_kindType::ExpressionLength
                                    :ASR::string_length_kindType::DeferredLength,
                                ASR::down_cast<ASR::String_t>(
                                    ASRUtils::extract_type(v_type))->m_physical_type));
                        } else { // resulting string is of pointerString physical type
                            char_type = ASRUtils::TYPE(ASR::make_String_t(al, loc, 1, a_len_expr,
                                a_len_expr?
                                    ASR::string_length_kindType::ExpressionLength
                                    :ASR::string_length_kindType::DeferredLength,
                                ASR::string_physical_typeType::PointerString));
                            if(ASRUtils::is_descriptorString(ASRUtils::expr_type(v_Var))){
                                v_Var = ASRUtils::cast_string_descriptor_to_pointer(al, v_Var);
                            }
                        }
                    }
                    return ASR::make_StringSection_t(al, loc, v_Var, l,
                            r, casted_step, char_type, arr_ref_val);
                }
            }

            Vec<ASR::dimension_t> array_section_dims;
            array_section_dims.reserve(al, n_args);
            for( size_t i = 0; i < n_args; i++ ) {
                if( args.p[i].m_step != nullptr &&
                    args.p[i].m_left == nullptr ) {
                    args.p[i].m_left = ASRUtils::get_bound<SemanticAbort>(v_Var, i + 1, "lbound", al, diag);
                }
                if (args.p[i].m_step != nullptr
                    || (args.p[i].m_step == nullptr && args.p[i].m_right != nullptr
                        && ASRUtils::is_array(ASRUtils::expr_type(args.p[i].m_right)))) {
                    ASR::dimension_t empty_dim;
                    empty_dim.loc = loc;
                    empty_dim.m_start = nullptr;
                    empty_dim.m_length = nullptr;
                    array_section_dims.push_back(al, empty_dim);
                }
                int max_kind = -1;
                int left_kind = -1;
                int right_kind = -1;
                int step_kind = -1;
                if (args.p[i].m_left) {
                    left_kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args.p[i].m_left));
                    max_kind = std::max(max_kind, left_kind);
                } 
                if (args.p[i].m_right) {
                    right_kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args.p[i].m_right));
                    max_kind = std::max(max_kind, right_kind);
                }
                if (args.p[i].m_step) {
                    step_kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args.p[i].m_step));
                    max_kind = std::max(max_kind, step_kind);
                }

                if (left_kind != -1 && left_kind != max_kind) {
                    if (ASRUtils::is_value_constant(args.p[i].m_left)) {
                        args.p[i].m_left = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                            ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(args.p[i].m_left))->m_n,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, max_kind))));
                    } else {
                        args.p[i].m_left = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, 
                            args.p[i].m_left, ASR::cast_kindType::IntegerToInteger, 
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, max_kind)), nullptr));
                    }
                }
                if (right_kind != -1 && right_kind != max_kind) {
                    if (ASRUtils::is_value_constant(args.p[i].m_right)) {
                        args.p[i].m_right = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                            ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(args.p[i].m_right))->m_n,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, max_kind))));
                    } else {
                        args.p[i].m_right = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, 
                            args.p[i].m_right, ASR::cast_kindType::IntegerToInteger, 
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, max_kind)), nullptr));
                    }
                }
                if (step_kind != -1 && step_kind != max_kind) {
                    if (ASRUtils::is_value_constant(args.p[i].m_step)) {
                        args.p[i].m_step = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                            ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(args.p[i].m_step))->m_n,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, max_kind))));
                    } else {
                        args.p[i].m_step = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, 
                            args.p[i].m_step, ASR::cast_kindType::IntegerToInteger, 
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, max_kind)), nullptr));
                    }
                }
            }
            type = ASRUtils::duplicate_type(al, ASRUtils::type_get_past_allocatable(type),
                    &array_section_dims);
            return ASR::make_ArraySection_t(al, loc,
                v_Var, args.p, args.size(), type, arr_ref_val);
        }
    }

    void check_if_type_spec_has_asterisk(const ASR::ttype_t* type) {
        if (type && 
            ASR::is_a<ASR::String_t>(*type) &&
            ASR::down_cast<ASR::String_t>(type)->m_len_kind == ASR::string_length_kindType::AssumedLength) {
            // e.g. [character(len=*) :: "a", "apple"], this isn't allowed
                diag.add(Diagnostic("Type-spec cannot contain an asterisk for a type "
                    "parameter", Level::Error, Stage::Semantic,
                    {Label("",{type->base.loc})}));
                throw SemanticAbort();
        }
    }

    void visit_ArrayInitializer(const AST::ArrayInitializer_t &x) {
        Vec<ASR::expr_t*> body;
        body.reserve(al, x.n_args);
        ASR::ttype_t *type = nullptr;
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        if (x.m_vartype != nullptr) {
            std::string sym = "";
            ASR::symbol_t *type_declaration;
            type = determine_type(x.base.base.loc, sym, x.m_vartype, false, false,
                dims, type_declaration, ASR::abiType::Source);
        } else {
            if (x.n_args == 0) {
                diag.add(Diagnostic(
                    "Empty array constructor is not allowed",
                    Level::Error, Stage::Semantic, {
                        Label("",{x.base.base.loc})
                    }));
                throw SemanticAbort();
            }
        }

        // if "type-spec" is omitted (for e.g. in "[1, 2, 3, 4.5]"), each element in
        // the array-constructor shall have the same type and kind type parameters,
        // otherwise each element in the array-constructor is cast using "ImplicitCastRules"
        // (e.g. "[real :: 1, 2, 3, 4]")
        bool is_type_spec_ommitted { type == nullptr };
        check_if_type_spec_has_asterisk(type);

        bool is_fixed_size_implied_do_loop = true;
        bool use_descriptorArray = false; // Set to true if any argument has no fixed size (array arguments).
        ASR::ttype_t* extracted_type { type ? ASRUtils::extract_type(type) : nullptr };
        size_t n_elements = 0;
        for (size_t i=0; i<x.n_args; i++) {
            this->visit_expr(*x.m_args[i]);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);

            if( ASR::is_a<ASR::ImpliedDoLoop_t>(*expr) ) {
                ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(expr);
                if (idl->m_value && ASR::is_a<ASR::ArrayConstant_t>(*idl->m_value)) {
                    ASR::ArrayConstant_t* array_constant = ASR::down_cast<ASR::ArrayConstant_t>(idl->m_value);
                    idl->m_type = array_constant->m_type;
                };
                is_fixed_size_implied_do_loop = ASRUtils::is_fixed_size_array(ASRUtils::expr_type(expr));
            }

            ASR::ttype_t* expr_type { ASRUtils::expr_type(expr) };
            if (ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_allocatable_pointer(expr_type))){ // Check physicalType of the array.
                ASR::array_physical_typeType arr_physicalType =  ASR::down_cast<ASR::Array_t>(
                                            ASRUtils::type_get_past_allocatable_pointer(expr_type))->m_physical_type;
                if( arr_physicalType == ASR::array_physical_typeType::DescriptorArray ||
                    arr_physicalType == ASR::array_physical_typeType::PointerToDataArray){

                    use_descriptorArray = true;
                }
            }
            if (type == nullptr) {
                type = expr_type;
                extracted_type = ASRUtils::extract_type(type);
            } else if (is_type_spec_ommitted) {
                // as the "type-spec" is omitted, each element should be the same type
                ASR::ttype_t* extracted_new_type = ASRUtils::extract_type(expr_type);
                if (!ASRUtils::check_equal_type(extracted_new_type, extracted_type)) {
                    diag.add(Diagnostic("Element in `" + ASRUtils::type_to_str_with_type(extracted_type) +
                        "` array constructor is `" + ASRUtils::type_to_str_with_type(extracted_new_type) + "`",
                        Level::Error, Stage::Semantic, {Label("",{expr->base.loc})}));
                    throw SemanticAbort();
                } else if (ASR::is_a<ASR::String_t>(*extracted_type)) {
                    int64_t l1, l2;
                    if (ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(extracted_type)->m_len, l1) &&
                        ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(extracted_new_type)->m_len, l2)) {
                        if (l1 != l2) {
                            diag.add(Diagnostic("Different `character` lengths " + std::to_string(l1)
                                + " and " + std::to_string(l2) + " in array constructor",
                                Level::Error, Stage::Semantic, {Label("",{expr->base.loc})}));
                            throw SemanticAbort();
                        }
                    }
                }
            } else if (!ASRUtils::check_equal_type(expr_type, type)) {
                ImplicitCastRules::set_converted_value(al, expr->base.loc,
                    &expr, expr_type, type, diag);
            }

            if (ASRUtils::is_fixed_size_array(expr_type)) {
                n_elements += ASRUtils::get_fixed_size_of_array(expr_type);
            } else {
                n_elements += 1;
            }
            body.push_back(al, expr);
        }

        ASR::dimension_t dim;
        dim.loc = x.base.base.loc;
        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int_type));
        dim.m_start = one;
        if( !is_fixed_size_implied_do_loop || use_descriptorArray) {
            dim.m_length = nullptr;
        } else {
            ASR::expr_t* x_n_args = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, n_elements, int_type));
            dim.m_length = x_n_args;
        }
        dims.push_back(al, dim);
        if (use_descriptorArray){
            type = ASRUtils::duplicate_type(al, type, &dims,
                    ASR::array_physical_typeType::DescriptorArray, true);
        } else {
            type = ASRUtils::duplicate_type(al, type, &dims);
        }
        tmp = ASRUtils::make_ArrayConstructor_t_util(al, x.base.base.loc, body.p,
            body.size(), type, ASR::arraystorageType::ColMajor);
    }

    void fill_expr_in_ttype_t(std::vector<ASR::expr_t*>& exprs, ASR::dimension_t* dims, size_t n_dims) {
        for( size_t i = 0; i < n_dims; i++ ) {
            exprs.push_back(dims[i].m_start);
            exprs.push_back(dims[i].m_length);
        }
    }

    void fix_exprs_ttype_t(std::vector<ASR::expr_t*>& exprs,
                           Vec<ASR::call_arg_t>& orig_args,
                           ASR::Function_t* orig_func=nullptr) {
        ASRUtils::ExprStmtDuplicator expr_duplicator(al);
        expr_duplicator.allow_procedure_calls = true;
        ASRUtils::ReplaceArgVisitor arg_replacer(al, current_scope, orig_func,
                        orig_args, current_function_dependencies, current_module_dependencies);
        for( size_t i = 0; i < exprs.size(); i++ ) {
            ASR::expr_t* expri = exprs[i];
            if (expri) {
                expr_duplicator.success = true;
                ASR::expr_t* expri_copy = expr_duplicator.duplicate_expr(expri);
                LCOMPILERS_ASSERT(expr_duplicator.success);
                arg_replacer.current_expr = &expri_copy;
                arg_replacer.replace_expr(expri_copy);
                exprs[i] = expri_copy;
            }
        }
    }

    void fill_new_dims(ASR::Array_t* t, const std::vector<ASR::expr_t*>& func_calls,
        Vec<ASR::dimension_t>& new_dims) {
        new_dims.reserve(al, t->n_dims);
        for( size_t i = 0, j = 0; i < func_calls.size(); i += 2, j++ ) {
            ASR::dimension_t new_dim;
            if (func_calls[i] != nullptr) {
                new_dim.loc = func_calls[i]->base.loc;
                new_dim.m_start = func_calls[i];
                new_dim.m_length = func_calls[i + 1];
                new_dims.push_back(al, new_dim);
            } else {
                new_dims.push_back(al, t->m_dims[j]);
            }
        }
    }

    ASR::ttype_t* handle_return_type(ASR::ttype_t *return_type, const Location &loc,
                                     Vec<ASR::call_arg_t>& args,
                                     ASR::Function_t* f=nullptr) {
        // Rebuild the return type if needed and make FunctionCalls use ExternalSymbol
        std::vector<ASR::expr_t*> func_calls;
        switch( return_type->type ) {
            case ASR::ttypeType::Allocatable: {
                ASR::Allocatable_t* allocatable_t = ASR::down_cast<ASR::Allocatable_t>(return_type);
                return ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc,
                    ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(
                            handle_return_type(allocatable_t->m_type, loc, args, f)))));
            }
            case ASR::ttypeType::Pointer: {
                ASR::Pointer_t* pointer_t = ASR::down_cast<ASR::Pointer_t>(return_type);
                return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(
                            handle_return_type(pointer_t->m_type, loc, args, f)))));
            }
            case ASR::ttypeType::Array: {
                ASR::Array_t* t = ASR::down_cast<ASR::Array_t>(return_type);
                ASR::ttype_t* t_m_type = handle_return_type(t->m_type, loc, args, f);
                fill_expr_in_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_exprs_ttype_t(func_calls, args, f);
                Vec<ASR::dimension_t> new_dims;
                fill_new_dims(t, func_calls, new_dims);
                bool for_type = true;
                for( size_t k = 0; k < new_dims.size(); k++ ) {
                    if( new_dims[k].m_length &&
                        ASR::is_a<ASR::ArraySize_t>(*new_dims[k].m_length) ) {
                        ASR::ArraySize_t* array_size_t =
                            ASR::down_cast<ASR::ArraySize_t>(new_dims[k].m_length);
                        if( (ASR::is_a<ASR::FunctionCall_t>(*array_size_t->m_v) &&
                            ASRUtils::is_allocatable(array_size_t->m_v)) ||
                            ASR::is_a<ASR::IntrinsicArrayFunction_t>(*array_size_t->m_v) ) {
                            for_type = false;
                            break;
                        }
                    }
                }
                return ASRUtils::make_Array_t_util(
                    al, loc, t_m_type, new_dims.p, new_dims.size(),
                    current_procedure_abi_type, false,
                    ASR::DescriptorArray, false, false, for_type);
            }
            case ASR::ttypeType::String: {
                ASR::String_t *t = ASR::down_cast<ASR::String_t>(return_type);
                func_calls.push_back(t->m_len);
                fix_exprs_ttype_t(func_calls, args, f);
                ASR::expr_t* a_len = func_calls[0] ?  func_calls[0] : t->m_len;
                return ASRUtils::TYPE(ASR::make_String_t(
                    al, loc, t->m_kind, a_len,
                    t->m_len_kind,
                    t->m_physical_type));
            }
            case ASR::ttypeType::StructType: {
                ASR::StructType_t* struct_t_type = ASR::down_cast<ASR::StructType_t>(return_type);
                ASR::symbol_t *sym = struct_t_type->m_derived_type;
                ASR::symbol_t *es_s = current_scope->resolve_symbol(
                    ASRUtils::symbol_name(sym));
                if (es_s == nullptr) {
                    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(sym);
                    ASR::Module_t* sym_module = ASRUtils::get_sym_module(sym);
                    LCOMPILERS_ASSERT(sym_module != nullptr);
                    std::string st_name = "1_" + std::string(st->m_name);
                    if (current_scope->get_symbol(st_name)) {
                        sym = current_scope->get_symbol(st_name);
                    } else {
                        sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                            al, st->base.base.loc, current_scope, s2c(al, st_name),
                            sym, sym_module->m_name, nullptr, 0, st->m_name,
                            ASR::accessType::Public));
                        current_scope->add_symbol(st_name, sym);
                    }
                } else {
                    sym = es_s;
                }
                return ASRUtils::TYPE(ASRUtils::make_StructType_t_util(
                    al, loc, sym, struct_t_type->m_is_cstruct));
            }
            default: {
                return return_type;
            }
        }
        return nullptr;
    }

    ASR::asr_t* symbol_resolve_external_generic_procedure_util(const Location &loc,
        int idx, ASR::symbol_t *v, Vec<ASR::call_arg_t>& args,
        ASR::GenericProcedure_t *g, ASR::ExternalSymbol_t *p) {
        ASR::symbol_t *final_sym;
        final_sym = ASRUtils::symbol_get_past_external(g->m_procs[idx]);
        if (!ASR::is_a<ASR::Function_t>(*final_sym)) {
            diag.add(Diagnostic("ExternalSymbol must point to a Function",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        ASR::ttype_t *return_type = nullptr;
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(final_sym);

        ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
        if (first_array_arg) {
            ASR::dimension_t* array_dims;
            size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(first_array_arg), array_dims
            );
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
            return_type = ASRUtils::duplicate_type(
                al, ASRUtils::get_FunctionType(func)->m_return_var_type, &new_dims
            );
        } else {
            return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
            return_type = handle_return_type(return_type, loc, args, func);
        }
        // Create ExternalSymbol for the final subroutine:
        // We mangle the new ExternalSymbol's local name as:
        //   generic_procedure_local_name @
        //     specific_procedure_remote_name
        std::string local_sym = std::string(p->m_name) + "@"
            + ASRUtils::symbol_name(final_sym);
        if (current_scope->get_symbol(local_sym) == nullptr) {
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
                al, g->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                final_sym,
                ASRUtils::symbol_name(ASRUtils::get_asr_owner(final_sym)),
                nullptr, 0, ASRUtils::symbol_name(final_sym),
                ASR::accessType::Private
                );
            final_sym = ASR::down_cast<ASR::symbol_t>(sub);
            current_scope->add_symbol(local_sym, final_sym);
        } else {
            final_sym = current_scope->get_symbol(local_sym);
        }
        ASR::expr_t *value = nullptr;
        ASR::symbol_t* final_sym2 = ASRUtils::symbol_get_past_external(final_sym);
        if (ASR::is_a<ASR::Function_t>(*final_sym2)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(final_sym2);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                ASR::symbol_t* v2 = ASRUtils::symbol_get_past_external(v);
                ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(v2);

                if (args.size() <= 2) {
                    value = intrinsic_procedures.comptime_eval(gp->m_name, al, loc, args, compiler_options);
                }
            }
        }
        if (ASRUtils::symbol_parent_symtab(final_sym)->get_counter() != current_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES(current_scope, final_sym, current_function_dependencies);
        }
        ASRUtils::insert_module_dependency(final_sym, al, current_module_dependencies);
        ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
        return ASRUtils::make_FunctionCall_t_util(al, loc,
            final_sym, v, args.p, args.size(), return_type,
            value, nullptr, false);
    }

    ASR::asr_t* symbol_resolve_external_generic_procedure(
                const Location &loc,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
        ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
        ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(f2);
        int idx = ASRUtils::select_generic_procedure(args, *g, loc,
                    [&](const std::string &msg, const Location &loc) {
                            diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        });
        return symbol_resolve_external_generic_procedure_util(loc, idx, v, args, g, p);
    }

    ASR::asr_t* symbol_resolve_external_generic_procedure_with_ast_node(
                const AST::FuncCallOrArray_t &x,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        const Location& loc = x.base.base.loc;
        ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
        ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
        ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(f2);
        int idx = ASRUtils::select_generic_procedure(args, *g, loc,
                    [&](const std::string &msg, const Location &loc) {
                        diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    },
                    false);
        if( idx == -1 ) {
            bool is_function = true;
            v = intrinsic_as_node(x, is_function);
            if( !is_function ) {
                return tmp;
            }
            return create_FunctionCall(loc, v, args);
        } else {
            return symbol_resolve_external_generic_procedure_util(loc, idx, v, args, g, p);
        }
    }

    ASR::asr_t* create_ClassProcedure(const Location &loc,
                AST::fnarg_t* m_args, size_t n_args,
                    ASR::symbol_t *v,
                    ASR::expr_t *v_expr) {
        Vec<ASR::call_arg_t> args;
        visit_expr_list(m_args, n_args, args);
        ASR::ClassProcedure_t *v_class_proc = ASR::down_cast<ASR::ClassProcedure_t>(ASRUtils::symbol_get_past_external(v));
        ASR::ttype_t *type = nullptr;
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(v_class_proc->m_proc);
        ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
        if (first_array_arg) {
            ASR::dimension_t* array_dims;
            size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(first_array_arg), array_dims
            );
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
            type = ASRUtils::duplicate_type(al,
                            ASRUtils::get_FunctionType(func)->m_return_var_type,
                            &new_dims);
        } else {
            ASRUtils::ExprStmtWithScopeDuplicator node_duplicator(al, current_scope);
            type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
            type = node_duplicator.duplicate_ttype(type);
        }
        if (ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES(current_scope, v, current_function_dependencies);
        }
        ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
        ASRUtils::set_absent_optional_arguments_to_null(args, func, al, v_expr, v_class_proc->m_is_nopass);
        return ASRUtils::make_FunctionCall_t_util(al, loc,
                v, nullptr, args.p, args.size(), type, nullptr,
                v_expr, v_class_proc->m_is_nopass);
    }

    ASR::asr_t* create_GenericProcedure(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            return symbol_resolve_external_generic_procedure(loc, v,
                    args);
        } else {
            ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(v);
            int idx = ASRUtils::select_generic_procedure(args, *p, loc,
                    [&](const std::string &msg, const Location &loc) {
                            diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        },
                    false);
            if( idx == -1 ) {
                std::string v_name = ASRUtils::symbol_name(v);
                v = resolve_intrinsic_function(loc, v_name);
                if( !v ) {
                    diag.add(Diagnostic("Couldn't find any function " + v_name + ".",
                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
                return create_FunctionCall(loc, v, args);
            }
            ASR::symbol_t *final_sym = p->m_procs[idx];

            ASR::ttype_t *type = nullptr;
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(final_sym);
            ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
            if (first_array_arg) {
                ASR::dimension_t* array_dims;
                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::expr_type(first_array_arg), array_dims
                );
                Vec<ASR::dimension_t> new_dims;
                new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                type = ASRUtils::duplicate_type(al,
                                ASRUtils::get_FunctionType(func)->m_return_var_type,
                                &new_dims);
            } else {
                type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
                type = handle_return_type(type, loc, args, func);
            }
            if (ASRUtils::symbol_parent_symtab(final_sym)->get_counter() != current_scope->get_counter()) {
                ADD_ASR_DEPENDENCIES(current_scope, final_sym, current_function_dependencies);
            }
            ASRUtils::insert_module_dependency(final_sym, al, current_module_dependencies);
            ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
            return ASRUtils::make_FunctionCall_t_util(al, loc,
                final_sym, v, args.p, args.size(), type,
                nullptr, nullptr, false);
        }
    }

    ASR::asr_t* create_GenericProcedureWithASTNode(const AST::FuncCallOrArray_t& x,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        const Location& loc = x.base.base.loc;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            return symbol_resolve_external_generic_procedure_with_ast_node(x, v,
                    args);
        } else {
            ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(v);
            int idx = ASRUtils::select_generic_procedure(args, *p, loc,
                    [&](const std::string &msg, const Location &loc) {
                            diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        },
                    false);
            if( idx == -1 ) {
                bool is_function = true;
                v = intrinsic_as_node(x, is_function);
                if( !is_function ) {
                    return tmp;
                }
                return create_FunctionCall(loc, v, args);
            }
            ASR::symbol_t *final_sym = p->m_procs[idx];

            ASR::ttype_t *type = nullptr;
            ASR::symbol_t *cp_s = nullptr;
            if (ASR::is_a<ASR::ClassProcedure_t>(*final_sym)) {
                cp_s = ASRUtils::import_class_procedure(al, x.base.base.loc,
                    final_sym, current_scope);
                final_sym = ASR::down_cast<ASR::ClassProcedure_t>(final_sym)->m_proc;
            }
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(final_sym)))
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(final_sym));
            ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
            if (first_array_arg) {
                ASR::dimension_t* array_dims;
                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::expr_type(first_array_arg), array_dims);
                Vec<ASR::dimension_t> new_dims;
                new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                type = ASRUtils::duplicate_type(al,
                        ASRUtils::get_FunctionType(func)->m_return_var_type,
                        &new_dims);
            } else {
                type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
                type = handle_return_type(type, loc, args, func);
            }
            if (cp_s != nullptr) {
                if (ASRUtils::symbol_parent_symtab(cp_s)->get_counter() != current_scope->get_counter()) {
                    ADD_ASR_DEPENDENCIES(current_scope, cp_s, current_function_dependencies);
                }
                ASRUtils::insert_module_dependency(cp_s, al, current_module_dependencies);
                ASRUtils::insert_module_dependency(final_sym, al, current_module_dependencies);
                ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
                return ASRUtils::make_FunctionCall_t_util(al, loc,
                    cp_s, v, args.p, args.size(), type,
                    nullptr, nullptr, false);
            } else {
                if (ASRUtils::symbol_parent_symtab(final_sym)->get_counter() != current_scope->get_counter()) {
                    ADD_ASR_DEPENDENCIES(current_scope, final_sym, current_function_dependencies);
                }
                ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
                ASRUtils::insert_module_dependency(final_sym, al, current_module_dependencies);
                ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
                return ASRUtils::make_FunctionCall_t_util(al, loc,
                    final_sym, v, args.p, args.size(), type,
                    nullptr, nullptr, false);
            }
        }
    }

    void replace_ArrayItem_in_SubroutineCall(Allocator &al, bool legacy_array_sections, SymbolTable* current_scope) {

    class ReplaceArrayItemWithArraySection: public ASR::BaseExprReplacer<ReplaceArrayItemWithArraySection> {
        private:
            Allocator& al;
        public:
            ASR::expr_t** current_expr;

            ReplaceArrayItemWithArraySection(Allocator& al_) :
                al(al_), current_expr(nullptr) {}

            void replace_ArrayItem(ASR::ArrayItem_t* x) {
                Vec<ASR::array_index_t> array_indices; array_indices.reserve(al, x->n_args);
                ASRUtils::ASRBuilder b(al, x->base.base.loc);

                for ( size_t i = 0; i < x->n_args; i++ ) {
                    ASR::array_index_t array_index;
                    array_index.loc = x->m_args[i].loc;
                    array_index.m_left = x->m_args[i].m_right;
                    array_index.m_right = b.ArrayUBound(x->m_v, i + 1);
                    if ( ASRUtils::expr_value(array_index.m_right) ) {
                        array_index.m_right = ASRUtils::expr_value(array_index.m_right);
                    }
                    array_index.m_step = b.i32( i + 1 );
                    array_indices.push_back(al, array_index);
                }
                ASR::ttype_t* new_type = ASRUtils::duplicate_type_with_empty_dims(al, ASRUtils::expr_type(x->m_v));
                *current_expr = ASRUtils::EXPR(ASR::make_ArraySection_t(al, x->base.base.loc, x->m_v,
                    array_indices.p, array_indices.n, new_type, nullptr));
            }

    };

    class LegacyArraySectionsVisitor : public ASR::CallReplacerOnExpressionsVisitor<LegacyArraySectionsVisitor> {
        private:
            Allocator& al;
            ReplaceArrayItemWithArraySection replacer;
        public:
            ASR::expr_t** current_expr;
            LegacyArraySectionsVisitor(Allocator& al_) :
                al(al_), replacer(al_), current_expr(nullptr) {}

            void call_replacer_() {
                replacer.current_expr = current_expr;
                replacer.replace_expr(*current_expr);
                current_expr = replacer.current_expr;
            }

            void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
                Vec<ASR::stmt_t*> body;
                body.reserve(al, n_body);
                for (size_t i=0; i<n_body; i++) {
                    this->visit_stmt(*m_body[i]);
                    body.push_back(al, m_body[i]);
                }
                m_body = body.p;
                n_body = body.size();
            }

            void visit_Function(const ASR::Function_t &x) {
                ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);

                for (auto &a : xx.m_symtab->get_scope()) {
                    this->visit_symbol(*a.second);
                }

                transform_stmts(xx.m_body, xx.n_body);
            }

            void visit_Program(const ASR::Program_t &x) {
                ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);

                for (auto &a : xx.m_symtab->get_scope()) {
                    this->visit_symbol(*a.second);
                }

                transform_stmts(xx.m_body, xx.n_body);
            }

            void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
                if ( ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(x.m_name)) ) {
                    ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(x.m_name));
                    ASR::FunctionType_t* f_type = ASR::down_cast<ASR::FunctionType_t>(f->m_function_signature);
                    std::map<int, ASR::ttype_t*> array_arg_index;
                    for (size_t i = 0; i < f->n_args; i++) {
                        if (ASRUtils::is_array(ASRUtils::expr_type(f->m_args[i]))) {
                            array_arg_index[i] = f_type->m_arg_types[i];
                        }
                    }
                    // iterate only over args of type array.
                    for( auto it: array_arg_index ) {
                        ASR::expr_t* arg_expr = x.m_args[it.first].m_value;
                        if ( arg_expr != nullptr ) {
                            ASR::expr_t** current_expr_copy = current_expr;
                            current_expr = const_cast<ASR::expr_t**>(&(arg_expr));;
                            call_replacer_();
                            x.m_args[it.first].m_value = *current_expr;
                            current_expr = current_expr_copy;
                        }
                    }
                }
            }
    };

        if ( legacy_array_sections ) {
            LegacyArraySectionsVisitor v(al);
            ASR::asr_t* asr_owner = current_scope->asr_owner;
            if ( ASR::is_a<ASR::symbol_t>(*asr_owner) ) {
                ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(asr_owner);
                if ( ASR::is_a<ASR::Function_t>(*sym) ) {
                    ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sym);
                    v.visit_Function(*f);
                } else if ( ASR::is_a<ASR::Program_t>(*sym) ) {
                    ASR::Program_t* p = ASR::down_cast<ASR::Program_t>(sym);
                    v.visit_Program(*p);
                }
            }
        }
    }

    void validate_create_function_arguments(Vec<ASR::call_arg_t>& args, ASR::symbol_t *v){
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(f2);
        ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(v);

        // Currently present function is supporting only integer arguments
        // After implementing present below if condition should be removed
        if( to_lower(func->m_name) != "present" ){
            for( size_t i = 0; i < args.size(); i++ ) {
                if( args.p[i].m_value == nullptr ) {
                    continue;
                }
                ASR::expr_t* arg = args.p[i].m_value;
                ASR::ttype_t* arg_type = ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(arg)));
                ASR::ttype_t* orig_arg_type = ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(func_type->m_arg_types[i]));

                if( ASR::is_a<ASR::FunctionType_t>(*arg_type) ) continue;

                bool is_compile_time = true;
                size_t rhs_rank = ASRUtils::extract_n_dims_from_ttype(orig_arg_type);
                size_t lhs_rank = ASRUtils::extract_n_dims_from_ttype(arg_type);

                if( ASRUtils::is_array(arg_type) ){
                    ASR::Array_t* arr_lhs = ASR::down_cast<ASR::Array_t>(arg_type);
                    for(size_t i = 0; i < lhs_rank; i++){
                        if( !arr_lhs->m_dims[i].m_length || !(ASRUtils::expr_value(arr_lhs->m_dims[i].m_length)) ){
                            is_compile_time = false;
                            break;
                        }
                    }
                }

                if( ASRUtils::is_array(orig_arg_type) ){
                    ASR::Array_t* arr_rhs = ASR::down_cast<ASR::Array_t>(orig_arg_type);
                    for(size_t i = 0; i < rhs_rank; i++){
                        if( !arr_rhs->m_dims[i].m_length || !(ASRUtils::expr_value(arr_rhs->m_dims[i].m_length)) ){
                            is_compile_time = false;
                            break;
                        }
                    }
                }

                if( is_compile_time ){
                    if ( ASR::is_a<ASR::Array_t>(*arg_type) && ASR::is_a<ASR::Array_t>(*orig_arg_type) ){
                        ASR::Array_t* arr_rhs = ASR::down_cast<ASR::Array_t>(orig_arg_type);
                        ASR::Array_t* arr_lhs = ASR::down_cast<ASR::Array_t>(arg_type);
                        int lhs_ele = 1;
                        int rhs_ele = 1;
                        for (size_t i = 0; i < arr_rhs->n_dims; i++) {
                            std::int64_t rhs_dim = ASRUtils::extract_dim_value_int(arr_rhs->m_dims[i].m_length);
                            if( rhs_dim != -1 ){
                                rhs_ele *= rhs_dim;
                            }
                        }
                        for (size_t i = 0; i < arr_lhs->n_dims; i++) {
                            std::int64_t lhs_dim = ASRUtils::extract_dim_value_int(arr_lhs->m_dims[i].m_length);
                            if( lhs_dim != -1 ){
                                lhs_ele *= lhs_dim;
                            } else {
                                lhs_ele = rhs_ele;
                                break;
                            }
                        }
                        if( lhs_ele < rhs_ele ){
                            diag.add(Diagnostic("Array passed into function has `" + std::to_string(lhs_ele) +
                                "` elements but function expects `" + std::to_string(rhs_ele) + "`.",
                                Level::Error, Stage::Semantic, {Label("", {args.p[i].loc})}));
                            throw SemanticAbort();
                        }
                    }
                }

                if(!ASRUtils::check_equal_type(arg_type,orig_arg_type)){
                    std::string arg_str = ASRUtils::type_to_str_fortran(arg_type);
                    std::string orig_arg_str = ASRUtils::type_to_str_fortran(orig_arg_type);
                    diag.add(Diagnostic("Type mismatch in argument at argument (" + std::to_string(i+1) +
                                        "); passed `" + arg_str + "` to `" + orig_arg_str + "`.",
                                        Level::Error, Stage::Semantic, {Label("", {args.p[i].loc})}));
                    throw SemanticAbort();
                }
            }
        }

    }

    void legacy_array_sections_helper(ASR::symbol_t *v, Vec<ASR::call_arg_t>& args, const Location &loc) {
        ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(v));
        if (compiler_options.legacy_array_sections) {
            // call b(w(icon)) -> call b(w(icon:)) if b is expecting an array
            ASR::FunctionType_t* f_type = ASR::down_cast<ASR::FunctionType_t>(f->m_function_signature);
            std::map<int, ASR::ttype_t*> array_arg_idx;
            for (size_t i = 0; i < f->n_args; i++) {
                if (ASRUtils::is_array(ASRUtils::expr_type(f->m_args[i]))) {
                    array_arg_idx[i] = f_type->m_arg_types[i];
                }
            }
            Vec<ASR::call_arg_t> args_with_array_section;
            args_with_array_section.reserve(al, args.size());
            for (size_t i = 0; i < args.size(); i++) {
                // check if i is in array_arg_idx
                if (array_arg_idx.find(i) != array_arg_idx.end()) {
                    ASR::call_arg_t arg = args[i];
                    ASR::ttype_t* expected_arg_type = ASRUtils::duplicate_type(al, array_arg_idx[i]);
                    ASR::expr_t* arg_expr = arg.m_value;
                    if (ASR::is_a<ASR::ArrayItem_t>(*arg_expr)) {
                        ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(arg_expr);
                        ASR::expr_t* array_expr = array_item->m_v;
                        LCOMPILERS_ASSERT(array_item->n_args > 0);
                        ASR::array_index_t first_arg = array_item->m_args[0];
                        ASR::expr_t* idx = first_arg.m_right;

                        Vec<ASR::dimension_t> dims;
                        dims.reserve(al, 1);
                        ASR::dimension_t dim;
                        dim.loc = loc;
                        dim.m_length = nullptr;
                        dim.m_start = nullptr;
                        dims.push_back(al, dim);
                        ASR::asr_t* descriptor_array = ASR::make_Array_t(al, loc, ASRUtils::type_get_past_array(expected_arg_type),
                                                        dims.p, dims.size(), ASR::array_physical_typeType::DescriptorArray);

                        ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(expected_arg_type);
                        ASR::asr_t* expected_array = ASR::make_Array_t(al, loc, ASRUtils::type_get_past_array(expected_arg_type),
                                                        array_t->m_dims, array_t->n_dims, ASRUtils::extract_physical_type(expected_arg_type));

                        // make ArraySection
                        Vec<ASR::array_index_t> array_indices;
                        array_indices.reserve(al, array_item->n_args);

                        for (size_t i = 0; i < array_item->n_args; i++) {
                            array_indices.push_back(al, array_item->m_args[i]);
                        }

                        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, ASRUtils::extract_type(ASRUtils::expr_type(idx))));

                        ASR::expr_t* array_bound = ASRUtils::get_bound<SemanticAbort>(array_expr, 1, "ubound", al, diag);

                        ASR::array_index_t array_idx;
                        array_idx.loc = array_item->base.base.loc;
                        array_idx.m_left = idx;
                        array_idx.m_right = array_bound;
                        array_idx.m_step = one;

                        array_indices.p[0] = array_idx;

                        ASR::expr_t* array_section = ASRUtils::EXPR(ASR::make_ArraySection_t(al, array_item->base.base.loc,
                                                    array_expr, array_indices.p, array_indices.size(),
                                                    ASRUtils::TYPE(descriptor_array), nullptr));

                        ASR::asr_t* array_cast = ASRUtils::make_ArrayPhysicalCast_t_util(al, array_item->base.base.loc, array_section,
                                                ASRUtils::extract_physical_type(ASRUtils::TYPE(descriptor_array)), ASRUtils::extract_physical_type(expected_arg_type), ASRUtils::TYPE(expected_array), nullptr);

                        ASR::expr_t* array_section_cast = ASRUtils::EXPR(array_cast);

                        arg.m_value = array_section_cast;

                        args_with_array_section.push_back(al, arg);
                    } else {
                        args_with_array_section.push_back(al, args[i]);
                    }
                } else {
                    args_with_array_section.push_back(al, args[i]);
                }
            }
            args = args_with_array_section;
            // There can be a possibility that initially it is ArrayItem and now we realised
            // that it must be an ArraySection instead.
            array_arg_idx.clear();
            for ( size_t i = 0; i < args.size(); i++ ) {
                ASR::expr_t* arg_expr = args[i].m_value;
                if ( arg_expr && ASRUtils::is_array(ASRUtils::expr_type(arg_expr)) ) {
                    array_arg_idx[i] = ASRUtils::expr_type(arg_expr);
                }
            }
            // bool visit_required = false;
            for ( auto it: array_arg_idx ) {
                ASR::expr_t* func_arg = f->m_args[it.first];
                if ( !ASRUtils::is_array(ASRUtils::EXPR2VAR(func_arg)->m_type) ) {
                    // create array type with empty dimensions and physical type as PointerToDataArray
                    ASR::ttype_t* new_type = ASRUtils::duplicate_type_with_empty_dims(al, it.second, ASR::array_physical_typeType::PointerToDataArray, true);
                    ASRUtils::EXPR2VAR(func_arg)->m_type = new_type;
                    f_type->m_arg_types[it.first] = new_type;
                    // visit_required = true;
                }
            }
        }
    }

    ASR::asr_t* create_Function(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        ASR::ttype_t *return_type = nullptr;
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(f2);
        ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
        if (first_array_arg) {
            ASR::dimension_t* array_dims;
            size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(first_array_arg), array_dims
            );
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
            return_type = ASRUtils::duplicate_type(al,
                            ASRUtils::get_FunctionType(func)->m_return_var_type,
                            &new_dims);
        } else {
            return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
            return_type = handle_return_type(return_type, loc, args, func);
        }
        ASR::expr_t* value = nullptr;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            // Populate value
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                value = intrinsic_procedures.comptime_eval(f->m_name, al, loc, args, compiler_options);
                char *mod = ASR::down_cast<ASR::ExternalSymbol_t>(
                    current_scope->resolve_symbol(f->m_name))->m_module_name;
                current_module_dependencies.push_back(al, mod);
            }
        }
        if (ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES(current_scope, v, current_function_dependencies);
        }
        if ((_processing_dimensions || _processing_char_len) && _declaring_variable &&
            ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter() &&
            !ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            current_function_dependencies.push_back(al, ASRUtils::symbol_name(v));
        }
        ASR::Module_t* v_module = ASRUtils::get_sym_module0(f2);
        if( v_module ) {
            current_module_dependencies.push_back(al, v_module->m_name);
        }
        ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
        if (args.size() > func->n_args) {
            const Location args_loc { ASRUtils::get_vec_loc(args) };
            diag.add(diag::Diagnostic(
                    "More actual than formal arguments in procedure call",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {args_loc})}));
            throw SemanticAbort();
        }
        ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
        legacy_array_sections_helper(v, args, loc);
        validate_create_function_arguments(args, v);

        return ASRUtils::make_FunctionCall_t_util(al, loc, v, nullptr,
            args.p, args.size(), return_type, value, nullptr, false);
    }

    ASR::asr_t* create_FunctionFromFunctionTypeVariable(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v, bool is_dt_present=false) {
        ASR::FunctionType_t* func = ASR::down_cast<ASR::FunctionType_t>(ASRUtils::symbol_type(v));
        ASR::ttype_t *return_type = func->m_return_var_type;
        if (ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES(current_scope, v, current_function_dependencies);
        }
        // TODO: Uncomment later
        // ASRUtils::set_absent_optional_arguments_to_null(args, ASR::down_cast<ASR::Function_t>(v), al);
        if( is_dt_present ) {
            ASR::expr_t* dt = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(
                al, loc, args.p[0].m_value, v, ASRUtils::symbol_type(v), nullptr));
            return ASRUtils::make_FunctionCall_t_util(al, loc, v, nullptr,
                args.p + 1, args.size() - 1, return_type, nullptr, dt, false);
        } else {
            return ASRUtils::make_FunctionCall_t_util(al, loc, v, nullptr,
                args.p, args.size(), return_type, nullptr, nullptr, false);
        }
    }

    // `fn` is a local Function or GenericProcedure (that resolves to a
    // Function), or an ExternalSymbol that points to a Function or
    // GenericProcedure (that resolves to a Function). This function resolves
    // the GeneralProcedure (based on `args`) and repacks the final function
    // into an ExternalSymbol if needed. It returns a FunctionCall ASR node.
    //
    // `args` are arguments of the function call as a list of `expr` nodes.
    //
    // If `fn` is intrinsic, it will also try to evaluate it into the `value`
    // member of the returned `FunctionCall`.
    ASR::asr_t* create_FunctionCall(const Location &loc,
                ASR::symbol_t *fn, Vec<ASR::call_arg_t>& args) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(fn);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            return create_Function(loc, args, fn);
        } else {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
            return create_GenericProcedure(loc, args, fn);
        }
    }

    ASR::asr_t* create_FunctionCallWithASTNode(const AST::FuncCallOrArray_t& x,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args, bool is_dt_present=false) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            return create_Function(x.base.base.loc, args, v);
        } else if (ASR::is_a<ASR::Variable_t>(*f2)) {
            return create_FunctionFromFunctionTypeVariable(x.base.base.loc, args, v, is_dt_present);
        } else {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
            return create_GenericProcedureWithASTNode(x, args, v);
        }
    }

    void make_ArrayItem_from_struct_m_args(AST::fnarg_t* struct_m_args, size_t struct_n_args, ASR::expr_t* expr, ASR::asr_t* &array_item_node, const Location &loc) {
        if (struct_n_args == 0) {
            return;
        }
        ASR::asr_t* tmp_copy = tmp;
        Vec<ASR::array_index_t> indices;
        indices.reserve(al, struct_n_args);
        bool is_array_section = false;
        for( size_t j = 0; j < struct_n_args; j++ ) {
            is_array_section = is_array_section || (struct_m_args[j].m_step != nullptr);
            ASR::array_index_t index;
            if( struct_m_args[j].m_step == nullptr ) {
                this->visit_expr(*struct_m_args[j].m_end);
                index.m_right = ASRUtils::EXPR(tmp);
                index.m_left = nullptr;
                index.m_step = nullptr;
            } else {
                if( struct_m_args[j].m_start ) {
                    this->visit_expr(*struct_m_args[j].m_start);
                    index.m_left = ASRUtils::EXPR(tmp);
                } else {
                    index.m_left = ASRUtils::get_bound<SemanticAbort>(expr, j + 1, "lbound", al, diag);
                }
                if( struct_m_args[j].m_end ) {
                    this->visit_expr(*struct_m_args[j].m_end);
                    index.m_right = ASRUtils::EXPR(tmp);
                } else {
                    index.m_right = ASRUtils::get_bound<SemanticAbort>(expr, j + 1, "ubound", al, diag);
                }
                this->visit_expr(*struct_m_args[j].m_step);
                index.m_step = ASRUtils::EXPR(tmp);
            }
            index.loc = struct_m_args->loc;
            indices.push_back(al, index);
        }
        tmp = tmp_copy;
        if( is_array_section ) {
            Vec<ASR::dimension_t> array_section_dims;
            array_section_dims.reserve(al, struct_n_args);
            for( size_t j = 0; j < struct_n_args; j++ ) {
                if( struct_m_args[j].m_step != nullptr ) {
                    ASR::dimension_t empty_dim;
                    empty_dim.loc = loc;
                    empty_dim.m_start = nullptr;
                    empty_dim.m_length = nullptr;
                    array_section_dims.push_back(al, empty_dim);
                }
            }
            ASR::ttype_t *array_section_type =
                ASRUtils::duplicate_type(al, ASRUtils::extract_type(ASRUtils::expr_type(expr)),
                        &array_section_dims);
            array_item_node = ASR::make_ArraySection_t(al, loc, expr, indices.p,
                indices.size(), array_section_type, nullptr);
        } else {
            array_item_node = ASRUtils::make_ArrayItem_t_util(al, loc, expr, indices.p,
                indices.size(), ASRUtils::duplicate_type(al, ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(expr))),
                ASR::arraystorageType::ColMajor, nullptr);
        }
        array_item_node = (ASR::asr_t*) replace_with_common_block_variables(
            ASRUtils::EXPR(array_item_node));
    }

    ASR::asr_t* resolve_variable2(const Location &loc, const std::string &var_name,
            const std::string &dt_name, SymbolTable*& scope,
            AST::fnarg_t* dt_struct_m_args=nullptr, size_t dt_struct_n_args=0,
            AST::fnarg_t* member_struct_m_args=nullptr, size_t member_struct_n_args=0) {

        ASR::symbol_t *v = scope->resolve_symbol(dt_name);
        if (!v) {
            diag.add(Diagnostic("Variable '" + dt_name + "' not declared",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        ASR::Variable_t* v_variable = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(v));
        ASR::ttype_t* v_variable_m_type = ASRUtils::duplicate_type(al, ASRUtils::extract_type(v_variable->m_type));
        if (ASR::is_a<ASR::StructType_t>(*v_variable_m_type)) {
            ASR::ttype_t* v_type = v_variable_m_type;
            ASR::symbol_t *derived_type = nullptr;
            if (ASR::is_a<ASR::StructType_t>(*v_type)) {
                derived_type = ASR::down_cast<ASR::StructType_t>(v_type)->m_derived_type;
            }
            ASR::Struct_t *der_type;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*derived_type)) {
                ASR::ExternalSymbol_t* der_ext = ASR::down_cast<ASR::ExternalSymbol_t>(derived_type);
                ASR::symbol_t* der_sym = der_ext->m_external;
                if (der_sym == nullptr) {
                    diag.add(Diagnostic("'" + std::string(der_ext->m_name) + "' isn't a Derived type.",
                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                } else {
                    der_type = ASR::down_cast<ASR::Struct_t>(der_sym);
                }
            } else {
                der_type = ASR::down_cast<ASR::Struct_t>(derived_type);
            }
            ASR::Struct_t *par_der_type = der_type;
            // scope = der_type->m_symtab;
            // ASR::symbol_t* member = der_type->m_symtab->resolve_symbol(var_name);
            ASR::symbol_t* member = nullptr;
            while( par_der_type != nullptr && member == nullptr ) {
                scope = par_der_type->m_symtab;
                member = par_der_type->m_symtab->resolve_symbol(var_name);
                if( par_der_type->m_parent != nullptr ) {
                    par_der_type = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(par_der_type->m_parent));
                } else {
                    par_der_type = nullptr;
                }
            }
            if( member != nullptr ) {
                ASR::asr_t* v_var = ASR::make_Var_t(al, loc, v);
                make_ArrayItem_from_struct_m_args(
                    dt_struct_m_args, dt_struct_n_args, ASRUtils::EXPR(v_var), v_var, loc);
                ASR::asr_t* expr_ = (ASR::asr_t*) ASRUtils::getStructInstanceMember_t(
                    al, loc, v_var, v, member, current_scope);
                make_ArrayItem_from_struct_m_args(
                    member_struct_m_args, member_struct_n_args, ASRUtils::EXPR(expr_), expr_, loc);
                return expr_;
            }
            diag.add(Diagnostic("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }

        // Handle type parameter inquiry kind first because others depend on their type
        if (var_name == "kind") {
            ASRUtils::create_intrinsic_function create_func = ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(var_name);
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            ASR::asr_t* v_var = ASR::make_Var_t(al, loc, v);
            args.push_back(al, ASRUtils::EXPR(v_var));
            return create_func(al, loc, args, diag);
        }

        if (ASR::is_a<ASR::Complex_t>(*v_variable_m_type)) {
            if (var_name != "re" && var_name != "im") {
                diag.add(Diagnostic("Complex variable '" + dt_name + "' only has %re, %im, and %kind members, not '" + var_name + "'",
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }

            if (ASRUtils::is_array(v_variable->m_type)) {
                ASR::expr_t* desc_arr = ASRUtils::cast_to_descriptor(al, ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, v)));
                ASR::ttype_t* v_variable_arr_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(v_variable->m_type));
                int kind = ASRUtils::extract_kind_from_ttype_t(v_variable_arr_type);
                ASR::dimension_t* m_dims = nullptr;
                int n_dims = ASRUtils::extract_dimensions_from_ttype(v_variable_arr_type, m_dims);
                Vec<ASR::dimension_t> dim_vec;
                dim_vec.from_pointer_n_copy(al, m_dims, n_dims);
                ASR::ttype_t *real_type = ASR::down_cast<ASR::ttype_t>(
                    ASR::make_Real_t(al, loc, kind));
                ASR::ttype_t* complex_arr_ret_type = ASRUtils::duplicate_type(al, real_type, &dim_vec,
                    ASR::array_physical_typeType::DescriptorArray, true);
                if (var_name == "re") {
                    return ASR::make_ComplexRe_t(al, loc, desc_arr, complex_arr_ret_type, nullptr);
                } else {
                    return ASR::make_ComplexIm_t(al, loc, desc_arr, complex_arr_ret_type, nullptr);
                }
            } else {
                if (var_name == "re") {
                    ASR::expr_t *val = ASR::down_cast<ASR::expr_t>(ASR::make_Var_t(al, loc, v));
                    int kind = ASRUtils::extract_kind_from_ttype_t(v_variable_m_type);
                    ASR::ttype_t *dest_type = ASR::down_cast<ASR::ttype_t>(ASR::make_Real_t(al, loc, kind));
                    ImplicitCastRules::set_converted_value(
                        al, loc, &val, v_variable_m_type, dest_type, diag);
                    return (ASR::asr_t*)val;
                } else {
                    ASR::expr_t *val = ASRUtils::EXPR(ASR::make_Var_t(al, loc, v));
                    ASR::ttype_t *real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                        ASRUtils::extract_kind_from_ttype_t(v_variable_m_type)));
                    ASR::expr_t *complex_value = ASRUtils::expr_value(val);
                    ASR::expr_t *im_value = nullptr;
                    if (complex_value && ASR::is_a<ASR::ComplexConstant_t>(*complex_value)) {
                        ASR::ComplexConstant_t *c = ASR::down_cast<ASR::ComplexConstant_t>(complex_value);
                        im_value = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, c->m_im, real_type));
                    }
                    return ASR::make_ComplexIm_t(al, loc, val, real_type, im_value);
                }
            }
        } else if (ASR::is_a<ASR::String_t>(*v_variable_m_type)) {
            if (var_name != "len") {
                diag.add(Diagnostic("Character variable '" + dt_name + "' only has %len and %kind members, not '" + var_name + "'",
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
            ASR::asr_t* v_var = ASR::make_Var_t(al, loc, v);
            make_ArrayItem_from_struct_m_args(
                dt_struct_m_args, dt_struct_n_args, ASRUtils::EXPR(v_var), v_var, loc);
            return create_StringLen_from_expr(ASRUtils::EXPR(v_var), int32, loc);
        } else {
            diag.add(Diagnostic("Variable '" + dt_name + "' is not a derived type",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
    }

    ASR::symbol_t* resolve_deriv_type_proc(const Location &loc, const std::string &var_name,
            const std::string dt_name, ASR::ttype_t* dt_type, SymbolTable*& scope,
            ASR::symbol_t* parent=nullptr) {
        ASR::symbol_t* v = nullptr;
        ASR::Struct_t* der_type = nullptr;
        if( dt_type ) {
            dt_type = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable(dt_type));
        }
        if( parent == nullptr ) {
            if ( ASR::is_a<ASR::StructType_t>(*dt_type) ) {
                ASR::StructType_t* der = ASR::down_cast<ASR::StructType_t>(dt_type);
                der_type = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(der->m_derived_type));
            } else {
                diag.add(Diagnostic("Variable '" + dt_name + "' is not a derived type",
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        } else {
            v = ASRUtils::symbol_get_past_external(parent);
            der_type = ASR::down_cast<ASR::Struct_t>(v);
        }
        ASR::symbol_t* member = der_type->m_symtab->get_symbol(var_name);
        if( member != nullptr ) {
            scope = der_type->m_symtab;
        } else if( der_type->m_parent != nullptr ) {
            member = resolve_deriv_type_proc(loc, var_name, "", nullptr, scope, der_type->m_parent);
        } else {
            diag.add(Diagnostic("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        return member;
    }

    // TODO: Use Vec<expr_t*> instead of std::vector<expr_t*> for performance
    template <typename T>
    bool handle_intrinsic_node_args(const T& x,
        Vec<ASR::expr_t*>& args, std::vector<std::string>& kwarg_names,
        size_t min_args, size_t max_args, const std::string& intrinsic_name,
        bool raise_error=true) {
        // raise_error is used only when the error is not related to number of arguments
        size_t total_args = x.n_args + x.n_keywords;
        if( !(total_args <= max_args && total_args >= min_args) ) {
            if (min_args == max_args) {
                diag.add(diag::Diagnostic(
                    "Incorrect number of arguments "
                    "passed to the '" + intrinsic_name + "' intrinsic. "
                    "It accepts exactly " + std::to_string(min_args) +
                    " arguments.",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
            } else {
                diag.add(diag::Diagnostic(
                    "Incorrect number of arguments "
                    "passed to the '" + intrinsic_name + "' intrinsic. "
                    "It accepts at least " + std::to_string(min_args) +
                    " and at most " + std::to_string(max_args) + " arguments.",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
            }
        }
        args.reserve(al, max_args);

        for( size_t i = 0; i < max_args; i++ ) {
            args.push_back(al, nullptr);
        }
        for( size_t i = 0; i < x.n_args; i++ ) {
            this->visit_expr(*x.m_args[i].m_end);
            args.p[i] = ASRUtils::EXPR(tmp);
            if (intrinsic_name == "and" || intrinsic_name == "or" || intrinsic_name == "xor" || intrinsic_name == "repeat" || intrinsic_name == "selected_int_kind"
            || intrinsic_name == "selected_real_kind" || intrinsic_name == "selected_char_kind") {
                if( ASRUtils::is_array(ASRUtils::expr_type(args[i]))) {
                    diag.add(diag::Diagnostic(
                    "arguments of `" + intrinsic_name + "` intrinsic must be scalar",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            }
            if( false /*TODO : Remove that when `merge` uses proper DescriptorString as type*/ &&
             ASRUtils::is_character(*ASRUtils::expr_type(args.p[i])) &&
                !ASRUtils::is_descriptorString(ASRUtils::expr_type(args.p[i]))){
                // These intrinsic functions receive `DescriptorString` 
                // if(intrinsic_name == "merge"){
                //     args.p[i] = ASRUtils::cast_string_pointer_to_descriptor(al, args.p[i]);
                // }
            } else if(ASRUtils::is_descriptorString(ASRUtils::expr_type(args.p[i]))) {
                // Any compile-time intrinsic function doesn't need a cast from
                // descriptorString to pointerString. Only runtime ones need a cast.
                if(intrinsic_name != "present" && intrinsic_name != "len" &&
                    intrinsic_name != "get_command_argument" &&
                    intrinsic_name != "get_environment_variable" && 
                    intrinsic_name != "move_alloc" && 
                    intrinsic_name != "merge"){
                    args.p[i] = ASRUtils::cast_string_descriptor_to_pointer(al, args.p[i]);
                }
            }
        }
        for( size_t i = 0; i < x.n_keywords; i++ ) {
            std::string curr_kwarg_name = to_lower(x.m_keywords[i].m_arg);
            if( std::find(kwarg_names.begin(), kwarg_names.end(),
                          curr_kwarg_name) == kwarg_names.end() ) {
                if( !raise_error ) {
                    return false;
                }
                diag.add(Diagnostic(
                    "Unrecognized keyword argument " + curr_kwarg_name +
                    " passed to " + intrinsic_name + " intrinsic.",
                    Level::Error, Stage::Semantic, {
                        Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }

        for( size_t i = 0; i < x.n_keywords; i++ ) {
            std::string curr_kwarg_name = to_lower(x.m_keywords[i].m_arg);
            auto it = std::find(kwarg_names.begin(), kwarg_names.end(),
                                curr_kwarg_name);
            int64_t kwarg_idx = it - kwarg_names.begin();
            if( args[kwarg_idx] != nullptr ) {
                if( !raise_error ) {
                    return false;
                }
                diag.add(diag::Diagnostic(
                    curr_kwarg_name + " has already " +
                    "been specified as a positional/keyword " +
                    "argument to " + intrinsic_name,
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
            this->visit_expr(*x.m_keywords[i].m_value);
            args.p[kwarg_idx] = ASRUtils::EXPR(tmp);
        }
        fill_optional_args(intrinsic_name, args, x.base.base.loc);
        return true;
    }

    int64_t handle_kind(ASR::expr_t* kind) {
        if( kind == nullptr ) {
            return compiler_options.po.default_integer_kind;
        }

        ASR::expr_t* kind_value = ASRUtils::expr_value(kind);
        if( kind_value == nullptr ) {
            diag.add(diag::Diagnostic(
                "Only Integer literals or expressions "
                "which reduce to constant Integer are "
                "accepted as kind parameters.",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {kind->base.loc})}));
                throw SemanticAbort();
        }
        return ASR::down_cast<ASR::IntegerConstant_t>(kind_value)->m_n;
    }

    ASR::asr_t* create_ArrayBound(const AST::FuncCallOrArray_t& x, std::string& bound_name) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"array", "dim", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 3, bound_name);
        ASR::expr_t *v_Var = args[0], *dim = args[1], *kind = args[2];
        ASR::arrayboundType bound = ASR::arrayboundType::LBound;
        if( bound_name == "lbound" ) {
            bound = ASR::arrayboundType::LBound;
        } else if( bound_name == "ubound" ) {
            bound = ASR::arrayboundType::UBound;
        }
        int64_t kind_const = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, kind_const));
        ASR::expr_t* bound_value = nullptr;
        if (dim == nullptr) {
            int n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(v_Var));
            Vec<ASR::expr_t*> arr_args;
            arr_args.reserve(al, 0);
            ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
            for (int i = 1; i <= n_dims; i++) {
                ASR::expr_t* dim_ = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, i, int_type));
                arr_args.push_back(al, ASRUtils::EXPR(ASR::make_ArrayBound_t(al, x.base.base.loc, v_Var, dim_, type,
                                      bound, bound_value)));
            }
            return ASRUtils::make_ArrayConstructor_t_util(al, x.base.base.loc, arr_args.p,
                arr_args.size(), type, ASR::arraystorageType::ColMajor);

        }

        ASR::expr_t* dim_value = ASRUtils::expr_value(dim);
        if( ASRUtils::is_value_constant(dim_value) ) {
            int64_t const_dim = -1;
            if( !ASRUtils::extract_value(dim_value, const_dim) ) {
                LCOMPILERS_ASSERT(false);
            }
            ASR::dimension_t* v_Var_dims = nullptr;
            int v_Var_n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(v_Var), v_Var_dims);
            if( const_dim > v_Var_n_dims || const_dim < 1) {
                diag.add(Diagnostic(
                    "Dimension " + std::to_string(const_dim) +
                    " is invalid. Rank of the array, " +
                    std::to_string(v_Var_n_dims),
                    Level::Error, Stage::Semantic, {
                        Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
            const_dim = const_dim - 1;
            if( v_Var_dims[const_dim].m_start && v_Var_dims[const_dim].m_length ) {
                ASR::expr_t* v_Var_start = ASRUtils::expr_value(v_Var_dims[const_dim].m_start);
                ASR::expr_t* v_Var_length = ASRUtils::expr_value(v_Var_dims[const_dim].m_length);
                if( bound == ASR::arrayboundType::LBound &&
                    ASRUtils::is_value_constant(v_Var_start) ) {
                    int64_t const_lbound = -1;
                    if( !ASRUtils::extract_value(v_Var_start, const_lbound) ) {
                        LCOMPILERS_ASSERT(false);
                    }
                    bound_value = make_ConstantWithType(make_IntegerConstant_t,
                                    const_lbound, type, x.base.base.loc);
                } else if( bound == ASR::arrayboundType::UBound &&
                ASRUtils::is_value_constant(v_Var_start) &&
                ASRUtils::is_value_constant(v_Var_length) ) {
                    int64_t const_lbound = -1;
                    if( !ASRUtils::extract_value(v_Var_start, const_lbound) ) {
                        LCOMPILERS_ASSERT(false);
                    }
                    int64_t const_length = -1;
                    if( !ASRUtils::extract_value(v_Var_length, const_length) ) {
                        LCOMPILERS_ASSERT(false);
                    }
                    bound_value = make_ConstantWithType(make_IntegerConstant_t,
                                    const_lbound + const_length - 1, type, x.base.base.loc);
                }
            }
        }
        return ASR::make_ArrayBound_t(al, x.base.base.loc, v_Var, dim, type,
                                      bound, bound_value);
    }

    ASR::asr_t* create_ArraySize(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"array", "dim", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 3, std::string("size"));
        ASR::expr_t *v_Var = args[0], *dim = args[1], *kind = args[2];
        int64_t kind_const = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, kind_const));
        ASR::dimension_t* m_dims = nullptr;
        int n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(v_Var), m_dims);
        int64_t compile_time_size = 1;
        if (dim != nullptr) {
            int32_t dim_idx = -1;
            ASRUtils::extract_value(dim, dim_idx);
            if (dim_idx == -1) {
                compile_time_size = -1;
            } else {
                ASR::dimension_t m_dim = m_dims[dim_idx - 1];
                ASR::expr_t* length_expr = m_dim.m_length;
                int64_t length = -1;
                ASRUtils::extract_value(length_expr, length);
                if (length == -1) {
                    compile_time_size = -1;
                } else {
                    compile_time_size = length;
                }
            }
        } else {
            for( int i = 0; i < n_dims; i++ ) {
                ASR::dimension_t m_dim = m_dims[i];
                ASR::expr_t* length_expr = m_dim.m_length;
                int64_t length = -1;
                ASRUtils::extract_value(length_expr, length);
                if( length == -1 ) {
                    compile_time_size = -1;
                    break ;
                }
                compile_time_size *= length;
            }
        }
        ASR::expr_t* size_compiletime = nullptr;
        if( compile_time_size != -1 ) {
            size_compiletime = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                                compile_time_size, type));
        }
        //if v_Var is a Function, give error
        if(ASR::is_a<ASR::Var_t>(*v_Var)) {
            ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(v_Var);
            ASR::symbol_t* sym = var->m_v;
            if(ASR::is_a<ASR::Function_t>(*sym)) {
                diag.add(Diagnostic("Argument of `size` must be an array",
                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }

        return ASRUtils::make_ArraySize_t_util(al, x.base.base.loc, v_Var, dim, type, size_compiletime, false);
    }

    ASR::asr_t* create_StringLen_from_expr(ASR::expr_t* v, ASR::ttype_t* type, const Location& loc) {
        ASR::expr_t* len_compiletime = nullptr;
        if( ASRUtils::is_array(ASRUtils::expr_type(v)) ) {
            ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(v)));
            ASR::String_t* str = ASR::down_cast<ASR::String_t>(arr->m_type);
            int64_t length;
            len_compiletime = ASRUtils::extract_value(str->m_len, length) ? 
            make_ConstantWithType(make_IntegerConstant_t, length, type, loc) : nullptr;
            // TODO: If possible try to use m_len of `character(len=m_len)`
            int n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(v));
            Vec<ASR::array_index_t> lbs; lbs.reserve(al, n_dims);
            for( int i = 0; i < n_dims; i++ ) {
                ASR::array_index_t index;
                index.loc = loc;
                index.m_left = nullptr;
                index.m_right = ASRUtils::get_bound<SemanticAbort>(v, i + 1, "lbound", al, diag);
                index.m_step = nullptr;
                lbs.push_back(al, index);
            }
            v = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc, v, lbs.p, lbs.size(),
                    ASRUtils::extract_type(ASRUtils::expr_type(v)),
                        ASR::arraystorageType::ColMajor, nullptr));
        } else if(ASR::is_a<ASR::ArrayItem_t>(*v)) {
            ASR::ArrayItem_t* arr_item = ASR::down_cast<ASR::ArrayItem_t>(v);
            ASR::Var_t* arr_var = ASR::down_cast<ASR::Var_t>(arr_item->m_v);
            ASR::symbol_t* arr_sym = arr_var->m_v;
            ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(ASRUtils::symbol_type(arr_sym)));
            ASR::String_t* str = ASR::down_cast<ASR::String_t>(arr->m_type);
            int length;
            len_compiletime = ASRUtils::extract_value(str->m_len, length) ? make_ConstantWithType(
                make_IntegerConstant_t, length, type, loc) : nullptr;
        }

        std::string input_string;
        if( ASRUtils::extract_string_value(ASRUtils::expr_value(v), input_string) ) {
            len_compiletime = make_ConstantWithType(
                make_IntegerConstant_t, input_string.size(), type, loc);
        }
        return ASR::make_StringLen_t(al, loc, v, type, len_compiletime);
    }

    ASR::asr_t* create_StringLen(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"string", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, std::string("len"));
        ASR::expr_t *v_var = args[0], *kind = args[1];
        int64_t kind_const = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, kind_const));
        return create_StringLen_from_expr(v_var, type, x.base.base.loc);
    }

    ASR::asr_t* create_ArrayReshape(const AST::FuncCallOrArray_t& x) {
        if( x.n_args + x.n_keywords < 2 || x.n_args + x.n_keywords > 4 ) {
            diag.add(Diagnostic("reshape expects number at least 2 and at most 4 arguments, got " +
                                std::to_string(x.n_args + x.n_keywords) + " arguments instead.",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        AST::expr_t* source = nullptr;
        AST::expr_t* shape = nullptr;
        AST::expr_t* order = nullptr;
        AST::expr_t* pad = nullptr;
        if ( x.n_args > 0 ) {
            source = x.m_args[0].m_end;
        }
        if ( x.n_args > 1 ) {
            shape = x.m_args[1].m_end;
        }
        if (x.n_args > 2){
            pad = x.m_args[2].m_end;
        }
        if (x.n_args > 3){
            pad = x.m_args[2].m_end;
            order = x.m_args[3].m_end;
        }
        for( size_t i=0;i<x.n_keywords;i++ ) {
            if( to_lower(x.m_keywords[i].m_arg) == "source" ) {
                source = x.m_keywords[i].m_value;
            } else if( to_lower(x.m_keywords[i].m_arg) == "shape" ) {
                shape = x.m_keywords[i].m_value;
            } else if( to_lower(x.m_keywords[i].m_arg) == "pad" ) {
                pad = x.m_keywords[i].m_value;
            } else if( to_lower(x.m_keywords[i].m_arg) == "order" ) {
                order = x.m_keywords[i].m_value;
            } else {
                diag.add(Diagnostic("Unrecognized keyword argument " +
                                    std::string(x.m_keywords[1].m_arg) + " passed to reshape.",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
        this->visit_expr(*source);
        ASR::expr_t* array = ASRUtils::EXPR(tmp);
        this->visit_expr(*shape);
        ASR::expr_t* newshape = ASRUtils::EXPR(tmp);
        ASR::expr_t* order_expr = nullptr;
        ASR::expr_t* pad_expr = nullptr;
        if (order!=nullptr){
            this->visit_expr(*order);
            order_expr = ASRUtils::EXPR(tmp);
        }
        if (pad != nullptr){
            this->visit_expr(*pad);
            pad_expr = ASRUtils::EXPR(tmp);
        }
        if( !ASRUtils::is_array(ASRUtils::expr_type(array)) ) {
            diag.add(Diagnostic("reshape accepts arrays for `source` argument, found " +
                ASRUtils::type_to_str_fortran(ASRUtils::expr_type(array)) +
                " instead.", Level::Error, Stage::Semantic, {Label("", {array->base.loc})}));
            throw SemanticAbort();
        }
        if( !ASRUtils::is_array(ASRUtils::expr_type(newshape)) ) {
            diag.add(Diagnostic("reshape accepts arrays for `shape` argument, found " +
                ASRUtils::type_to_str_fortran(ASRUtils::expr_type(newshape)) +
                " instead.", Level::Error, Stage::Semantic, {Label("", {shape->base.loc})}));
            throw SemanticAbort();
        }
        if (order_expr) {
            if (!ASRUtils::is_array(ASRUtils::expr_type(order_expr))){
                diag.add(Diagnostic("reshape accepts arrays for `order` argument, found " +
                    ASRUtils::type_to_str_fortran(ASRUtils::expr_type(order_expr)) +
                    " instead.", Level::Error, Stage::Semantic, {Label("", {order->base.loc})}));
                throw SemanticAbort();
            }
        }
        if (pad_expr) {
            if (!ASRUtils::is_array(ASRUtils::expr_type(pad_expr))){
                diag.add(Diagnostic("reshape accepts arrays for `pad` argument, found " +
                    ASRUtils::type_to_str_fortran(ASRUtils::expr_type(pad_expr)) +
                    " instead.", Level::Error, Stage::Semantic, {Label("", {pad->base.loc})}));
                throw SemanticAbort();
            } else if ( (ASRUtils::type_to_str_fortran(ASRUtils::expr_type(pad_expr)) != ASRUtils::type_to_str_fortran(ASRUtils::expr_type(array)))||
            (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(pad_expr)) != ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(array))) ){
                diag.add(Diagnostic("`pad` argument of reshape intrinsic must have same type and kind as `source` argument, found pad type " +
                    ASRUtils::type_to_str_fortran(ASRUtils::expr_type(pad_expr)) + " and kind " + std::to_string(ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(pad_expr)))
                     + " source type " + ASRUtils::type_to_str_fortran(ASRUtils::expr_type(array)) + " and kind " + std::to_string(ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(array))) +
                    " instead.", Level::Error, Stage::Semantic, {Label("", {pad->base.loc})}));
                throw SemanticAbort();
            }
        }
        ASR::array_physical_typeType array_physical_type = ASRUtils::extract_physical_type(
                                                            ASRUtils::expr_type(array));
        ASR::ttype_t* shape_type = ASRUtils::expr_type(newshape);
        ASR::array_physical_typeType shape_physical_type = ASRUtils::extract_physical_type(shape_type);
        if (shape_physical_type != ASR::array_physical_typeType::FixedSizeArray){
            diag.add(Diagnostic("`shape` array in reshape intrinsic should be of constant size",
                            Level::Error, Stage::Semantic, {Label("not a constant size array", {newshape->base.loc})}));
            throw SemanticAbort();
        }
        // the size (i.e. number of elements) of 'newshape' array determines
        // the dimension size of 'ArrayReshape'
        ASR::Array_t* newshape_array_type = ASR::down_cast<ASR::Array_t>(ASRUtils::expr_type(newshape));
        LCOMPILERS_ASSERT_MSG(newshape_array_type->n_dims == 1, "newshape must be a 1D array");
        size_t newshape_dims = ASR::down_cast<ASR::IntegerConstant_t>(newshape_array_type->m_dims[0].m_length)->m_n;
        ASR::ttype_t* arr_element_type = ASRUtils::extract_type(ASRUtils::expr_type(array));

        ASR::ttype_t* reshape_ttype = ASRUtils::TYPE(ASR::make_Array_t(al, arr_element_type->base.loc, arr_element_type,
                                                    nullptr, newshape_dims, ASR::array_physical_typeType::FixedSizeArray));

        size_t n_dims_array_reshape = ASRUtils::extract_n_dims_from_ttype(reshape_ttype);

        Vec<ASR::dimension_t> dims;
        dims.reserve(al, n_dims_array_reshape);

        Location loc = newshape->base.loc;
        if (ASR::is_a<ASR::ArrayConstructor_t>(*newshape) &&
            ASR::down_cast<ASR::ArrayConstructor_t>(newshape)->m_value != nullptr) {
            newshape = ASR::down_cast<ASR::ArrayConstructor_t>(newshape)->m_value;
        }

        // if 'newshape' is an ArrayConstant, then assign all of it's
        // elements as dimensions
        if (ASR::is_a<ASR::ArrayConstant_t>(*newshape)) {
            ASR::ArrayConstant_t* const_newshape = ASR::down_cast<ASR::ArrayConstant_t>(newshape);
            ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            int new_shape_size = 1;
            for (size_t i=0; i < n_dims_array_reshape; i++) {
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int_type));
                dim.m_length = ASRUtils::fetch_ArrayConstant_value(al, const_newshape, i);
                dims.push_back(al, dim);
                new_shape_size *= ASR::down_cast<ASR::IntegerConstant_t>(dim.m_length)->m_n;
            }
            int64_t array_size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(array));
            if (array_size != -1 &&  new_shape_size > array_size) {
                if (!pad){
                    diag.add(Diagnostic("reshape accepts `source` array with size greater than or equal to size specified by `shape` array",
                                        Level::Error, Stage::Semantic, {Label("`shape` specifies size of " +
                                        std::to_string(new_shape_size) + " which exceeds the `source` array size of " +
                                        std::to_string(array_size), {x.base.base.loc})}));
                    throw SemanticAbort();
                } else if ( ASR::is_a<ASR::ArrayConstant_t>(*pad_expr) ) {
                    if ( ASR::is_a<ASR::ArrayConstant_t>(*array) ) {
                        ASR::ttype_t* a_type = ASRUtils::expr_type(array);
                        a_type = ASRUtils::type_get_past_pointer(a_type);
                        ASR::Array_t* a_type_ = ASR::down_cast<ASR::Array_t>(a_type);
                        Vec<ASR::expr_t*> elements;
                        elements.reserve(al, array_size);
                        ASR::ArrayConstant_t* const_array = ASR::down_cast<ASR::ArrayConstant_t>(array);
                        ASR::ArrayConstant_t* const_pad = ASR::down_cast<ASR::ArrayConstant_t>(pad_expr);
                        for (int i = 0; i < array_size; i++) {
                            elements.push_back(al, ASRUtils::fetch_ArrayConstant_value(al, const_array, i));
                        }
                        int64_t diff = new_shape_size - array_size;
                        int pad_size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(pad_expr));
                        for (int i = 0; i < diff; i++) {
                            elements.push_back(al, ASRUtils::fetch_ArrayConstant_value(al, const_pad, i % pad_size));
                        }
                        size_t curr_idx = elements.size();
                        ASR::ttype_t* new_type = ASRUtils::TYPE(
                            ASR::make_Array_t(al, a_type_->base.base.loc, a_type_->m_type, dims.p, dims.n,
                                            a_type_->m_physical_type)
                        );
                        void *data = ASRUtils::set_ArrayConstant_data(elements.p, curr_idx, a_type_->m_type);
                        int64_t n_data = curr_idx * ASRUtils::extract_kind_from_ttype_t(a_type_->m_type);
                        if (ASRUtils::is_character(*a_type_->m_type)) {
                            int64_t len;
                            if(!ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(a_type_->m_type)->m_len, len)){LCOMPILERS_ASSERT(false);}
                            n_data = curr_idx * len;
                        }
                        array = ASRUtils::EXPR(
                            ASR::make_ArrayConstant_t(al, loc, n_data, data, new_type,
                                                    ASR::arraystorageType::ColMajor)
                        );
                    }
                }
            }
            if (order_expr && ASR::is_a<ASR::ArrayConstant_t>(*order_expr)){
                ASR::ArrayConstant_t* const_order = ASR::down_cast<ASR::ArrayConstant_t>(order_expr);
                std::vector<int> elements;
                int64_t n = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(order_expr));
                for (int64_t i=0; i <  n; i++) {
                    if (ASR::is_a<ASR::IntegerConstant_t>(*ASRUtils::fetch_ArrayConstant_value(al, const_order, i))){
                        elements.push_back(ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_order, i))->m_n);
                    } else {
                        diag.add(Diagnostic("reshape accepts `order` array with integer elements",
                                            Level::Error, Stage::Semantic, {Label("", {order->base.loc})}));
                        throw SemanticAbort();
                    }
                }
                std::set<int> unique_elements(elements.begin(), elements.end());
                for (int i=1; i<=n; i++){
                    if (unique_elements.find(i) == unique_elements.end()){
                        diag.add(Diagnostic("reshape accepts `order` array as a permutation of elements from 1 to " + std::to_string(n),
                                            Level::Error, Stage::Semantic, {Label("", {order->base.loc})}));
                        throw SemanticAbort();
                    }
                }
                if (ASR::is_a<ASR::ArrayConstant_t>(*array) && ASR::is_a<ASR::ArrayConstant_t>(*newshape)){
                    ASR::ttype_t* a_type = ASRUtils::expr_type(array);
                    a_type = ASRUtils::type_get_past_pointer(a_type);
                    ASR::Array_t* a_type_ = ASR::down_cast<ASR::Array_t>(a_type);
                    Vec<ASR::expr_t*> elements_;
                    elements_.reserve(al, array_size);
                    ASR::ArrayConstant_t* const_array = ASR::down_cast<ASR::ArrayConstant_t>(array);
                    ASR::ArrayConstant_t* const_shape = ASR::down_cast<ASR::ArrayConstant_t>(newshape);
                    int64_t array_size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(array));
                    std::map<int, int> index_map;
                    for (int i=0; i<array_size; i++){
                        int temp = i;
                        std::vector<int> d(n, 0);
                        for (int j=0; j<n; j++){
                            int dim = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_order, j))->m_n - 1;
                            int shape_dim = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_shape, dim))->m_n;
                            d[j] = temp % shape_dim;
                            temp = temp / shape_dim;
                        }
                        std::vector<int> I(n, 0);
                        for(int j=0; j<n; j++){
                            int order_ = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_order, j))->m_n - 1;
                            I[order_] = d[j];
                        }

                        int R = 0, stride = 1;
                        for (int j=0; j<n; j++){
                            R += I[j] * stride;
                            stride *= ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_shape, j))->m_n;
                        }
                        index_map[R] = i;
                    }
                    for (int i=0; i<array_size; i++){
                        elements_.push_back(al, ASRUtils::fetch_ArrayConstant_value(al, const_array, index_map[i]));
                    }

                    size_t curr_idx = elements_.size();
                    ASR::ttype_t* new_type = ASRUtils::TYPE(
                        ASR::make_Array_t(al, a_type_->base.base.loc, a_type_->m_type, dims.p, dims.n,
                                        a_type_->m_physical_type)
                    );
                    void *data = ASRUtils::set_ArrayConstant_data(elements_.p, curr_idx, a_type_->m_type);
                    int64_t n_data = curr_idx * ASRUtils::extract_kind_from_ttype_t(a_type_->m_type);
                    if (ASRUtils::is_character(*a_type_->m_type)) {
                        int64_t len;
                        if(!ASRUtils::extract_value(
                            ASR::down_cast<ASR::String_t>(a_type_->m_type)->m_len, len)){
                            LCOMPILERS_ASSERT_MSG(false, "String length should be a constant");
                        }
                        n_data = curr_idx * len;
                    }
                    array = ASRUtils::EXPR(
                        ASR::make_ArrayConstant_t(al, loc, n_data, data, new_type,
                                                ASR::arraystorageType::ColMajor)
                    );
                }
            }
        } else {
            // otherwise empty dimensions
            dims.reserve(al, n_dims_array_reshape);
            for (size_t i=0; i < n_dims_array_reshape; i++) {
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = nullptr;
                dim.m_length = nullptr;
                dims.push_back(al, dim);
            }
        }

        reshape_ttype = ASRUtils::duplicate_type(al, reshape_ttype, &dims, array_physical_type, true);
        newshape = ASRUtils::cast_to_descriptor(al, newshape);
        // TODO: 'value' is assigned as nullptr always to ArrayReshape, when both
        // 'array' and 'newshape' are ArrayConstant's we can set 'value'
        // as well
        return ASR::make_ArrayReshape_t(al, x.base.base.loc, array, newshape, reshape_ttype, nullptr);
    }

    ASR::asr_t* create_ArrayIsContiguous(const AST::FuncCallOrArray_t& x) {
        if (x.n_args != 1 || x.n_keywords > 0) {
            diag.add(Diagnostic("is_contiguous expects exactly one array argument, got " +
                                std::to_string(x.n_args) + " arguments instead.",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

        AST::expr_t* source = x.m_args[0].m_end;
        this->visit_expr(*source);
        ASR::expr_t* array = ASRUtils::EXPR(tmp);
        if (!ASRUtils::is_array(ASRUtils::expr_type(array))) {
            diag.add(Diagnostic("is_contiguous expects an array argument, found " +
                                ASRUtils::type_to_str_fortran(ASRUtils::expr_type(array)) + " instead.",
                                Level::Error, Stage::Semantic, {Label("", {array->base.loc})}));
            throw SemanticAbort();
        }

        ASR::ttype_t* a_type = ASRUtils::TYPE(ASR::make_Logical_t(
                                            al, x.base.base.loc, compiler_options.po.default_integer_kind));
        return ASR::make_ArrayIsContiguous_t(al, x.base.base.loc, array, a_type, nullptr);
    }

    ASR::asr_t* create_LFLen(const AST::FuncCallOrArray_t& x) {
        if (x.n_args != 1 || x.n_keywords > 0) {
            diag.add(Diagnostic("_lfortran_len expects exactly one argument, got " +
                                std::to_string(x.n_args) + " arguments instead.",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
        }

        AST::expr_t* source = x.m_args[0].m_end;
        this->visit_expr(*source);
        ASR::expr_t* arg = ASRUtils::EXPR(tmp);

        if (ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(arg))) 
            return ASR::make_ListLen_t(al, x.base.base.loc, arg, 
                                 ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4)), nullptr);
        else if (ASR::is_a<ASR::Set_t>(*ASRUtils::expr_type(arg)))
            return ASR::make_SetLen_t(al, x.base.base.loc, arg, 
                                 ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4)), nullptr);
        else {
            std::string arg_type_str = ASRUtils::type_to_str_fortran(ASRUtils::expr_type(arg));
            diag.add(Diagnostic("Argument of type '" + arg_type_str + "' for _lfortran_len has not been implemented yet",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
    }

    ASR::asr_t* create_LFGetItem(const AST::FuncCallOrArray_t& x) {
        if (x.n_args != 2 || x.n_keywords > 0) {
            diag.add(Diagnostic("_lfortran_get_item expects exactly two arguments, got " +
                                std::to_string(x.n_args) + " arguments instead.",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
        }
        
        Vec<ASR::expr_t *> args;
        args.reserve(al, 2);

        for (int i=0;i<2;i++){
            AST::expr_t* source = x.m_args[i].m_end;
            this->visit_expr(*source);
            args.push_back(al, ASRUtils::EXPR(tmp));
        }   

        if (ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(args[0]))) {

            if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(args[1]))) {
                std::string arg_type_str = ASRUtils::type_to_str_fortran(ASRUtils::expr_type(args[1]));
                diag.add(Diagnostic("Index of a list must be an integer not '" + arg_type_str + "'",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            }
            return ASR::make_ListItem_t(al, x.base.base.loc, args[0], args[1],
                                        ASRUtils::get_contained_type(ASRUtils::expr_type(args[0])), nullptr);
        } else {
            std::string arg_type_str = ASRUtils::type_to_str_fortran(ASRUtils::expr_type(args[0]));
            diag.add(Diagnostic("Argument of type '" + arg_type_str + "' for _lfortran_get_item has not been implemented yet",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
    }

    ASR::asr_t* create_ListConstant(const AST::FuncCallOrArray_t& x) {
        if (x.n_keywords > 0) {
            diag.add(Diagnostic("_lfortran_list_constant expects no keyword arguments",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

        if (x.n_args == 0) {
            diag.add(Diagnostic("As of now _lfortran_list_constant expects atleast one argument",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

            
        
        AST::expr_t* source = nullptr;
        ASR::ttype_t *contained_type = nullptr;
        Vec<ASR::expr_t *> args;
        args.reserve(al, 1);
        

        for (size_t i=0;i<x.n_args;i++) {
            source = x.m_args[i].m_end;
            this->visit_expr(*source);
            ASR::expr_t* arg = ASRUtils::EXPR(tmp);
            args.push_back(al, arg);
            ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);


            if (contained_type && !ASRUtils::check_equal_type(contained_type, arg_type)) {
                std::string contained_type_str = ASRUtils::type_to_str_fortran(contained_type);
                std::string arg_type_str = ASRUtils::type_to_str_fortran(arg_type);
                diag.add(Diagnostic(
                    "Type mismatch in _lfortran_list_constant, the types must be compatible",
                    Level::Error, Stage::Semantic, {
                        Label("Types mismatch (found '" + 
                    arg_type_str + "', expected '" + contained_type_str +  "')",{arg->base.loc})
                    }));
                throw SemanticAbort();
            } else if (!contained_type) {
                contained_type = arg_type;
            }
        }


        return ASR::make_ListConstant_t(al, x.base.base.loc, args.p, args.n, 
                                        ASRUtils::TYPE(ASR::make_List_t(al, x.base.base.loc, contained_type)));
    }

    ASR::asr_t* create_ListCount(const AST::FuncCallOrArray_t& x) {
        if (x.n_keywords > 0 || x.n_args != 2) {
            diag.add(Diagnostic("_lfortran_list_count expects exactly two arguments",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
            
        
        AST::expr_t* source = x.m_args[0].m_end;
        this->visit_expr(*source);
        ASR::expr_t* list = ASRUtils::EXPR(tmp);
        ASR::ttype_t *contained_type = ASRUtils::get_contained_type(ASRUtils::expr_type(list));
        
        source = x.m_args[1].m_end;
        this->visit_expr(*source);
        ASR::expr_t* arg = ASRUtils::EXPR(tmp);
        ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);


        if (contained_type && !ASRUtils::check_equal_type(contained_type, arg_type)) {
            std::string contained_type_str = ASRUtils::type_to_str_fortran(contained_type);
            std::string arg_type_str = ASRUtils::type_to_str_fortran(arg_type);
            diag.add(Diagnostic(
                "Type mismatch in _lfortran_list_constant, the types must be compatible",
                Level::Error, Stage::Semantic, {
                    Label("Types mismatch (found '" + 
                arg_type_str + "', expected '" + contained_type_str +  "')",{arg->base.loc})
                }));
            throw SemanticAbort();
        }         


        return ASR::make_ListCount_t(al, x.base.base.loc, list, arg, arg_type, nullptr);
    }

    ASR::asr_t* create_SetConstant(const AST::FuncCallOrArray_t& x) {
        if (x.n_keywords > 0) {
            diag.add(Diagnostic("_lfortran_set_constant expects no keyword arguments",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

        if (x.n_args == 0) {
            diag.add(Diagnostic("_lfortran_set_constant expects at least one argument",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

            
        
        AST::expr_t* source = nullptr;
        ASR::ttype_t *contained_type = nullptr;
        Vec<ASR::expr_t *> args;
        args.reserve(al, 1);
        

        for (size_t i=0;i<x.n_args;i++) {
            source = x.m_args[i].m_end;
            this->visit_expr(*source);
            ASR::expr_t* arg = ASRUtils::EXPR(tmp);
            args.push_back(al, arg);
            ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);


            if (contained_type && !ASRUtils::check_equal_type(contained_type, arg_type)) {
                std::string contained_type_str = ASRUtils::type_to_str_fortran(contained_type);
                std::string arg_type_str = ASRUtils::type_to_str_fortran(arg_type);
                diag.add(Diagnostic(
                    "Type mismatch in _lfortran_set_constant, the types must be compatible",
                    Level::Error, Stage::Semantic, {
                        Label("Types mismatch (found '" + 
                    arg_type_str + "', expected '" + contained_type_str +  "')",{arg->base.loc})
                    }));
                throw SemanticAbort();
            } else if (!contained_type) {
                contained_type = arg_type;
            }
        }


        return ASR::make_SetConstant_t(al, x.base.base.loc, args.p, args.n, 
                                        ASRUtils::TYPE(ASR::make_Set_t(al, x.base.base.loc, contained_type)));
    }

    ASR::asr_t* create_BitCast(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"source", "mold", "size"};
        handle_intrinsic_node_args(x, args, kwarg_names, 2, 3, "transfer");
        ASR::expr_t *source = args[0], *mold = args[1], *size = args[2];
        if( size && !ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(size)) ) {
            diag.add(Diagnostic("size argument to `transfer` intrinsic must "
                                "be of Integer type.",
                                Level::Error, Stage::Semantic, {Label("", {size->base.loc})}));
            throw SemanticAbort();
        }
        Vec<ASR::dimension_t> new_dims;
        new_dims.reserve(al, 1);
        if( size ) {
            ASR::expr_t* one = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, x.base.base.loc, 1,
                    ASRUtils::expr_type(size)));
            ASR::dimension_t size_dim;
            size_dim.loc = size->base.loc;
            size_dim.m_start = one;
            size_dim.m_length = size;
            new_dims.push_back(al, size_dim);
        } else {
            if( ASR::is_a<ASR::ArrayConstant_t>(*mold) ||
                ASRUtils::is_array(ASRUtils::expr_type(mold)) ) {
                // TODO: Make resulting array size more efficient by
                // considering bit length of source.
                ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
                ASR::expr_t* one = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, 1,
                                                int_type));
                ASR::expr_t* b64 = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, 64,
                                                int_type));
                ASR::dimension_t size_dim;
                size_dim.loc = x.base.base.loc;
                size_dim.m_start = one;
                size_dim.m_length = b64;
                new_dims.push_back(al, size_dim);
            }
        }
        ASR::ttype_t* type = ASRUtils::type_get_past_allocatable(ASRUtils::duplicate_type(al, ASRUtils::expr_type(mold), &new_dims));
        ASR::expr_t *transfer_value = nullptr, *source_value = ASRUtils::expr_value(source),
            *mold_value = ASRUtils::expr_value(mold), *size_value = nullptr;
        if(size) size_value = ASRUtils::expr_value(size);

        if (source_value && mold_value) {
            std::vector<uint8_t> source_bits;
            if (ASR::is_a<ASR::IntegerConstant_t>(*source_value)) {
                int64_t val = ASR::down_cast<ASR::IntegerConstant_t>(source_value)->m_n;
                source_bits.assign(reinterpret_cast<uint8_t*>(&val),
                                reinterpret_cast<uint8_t*>(&val) + sizeof(val));
            } else if (ASR::is_a<ASR::RealConstant_t>(*source_value)) {
                if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(mold)) == 4) {
                    float val = ASR::down_cast<ASR::RealConstant_t>(source_value)->m_r;
                    source_bits.assign(reinterpret_cast<uint8_t*>(&val),
                                    reinterpret_cast<uint8_t*>(&val) + sizeof(val));
                } else {
                    double val = ASR::down_cast<ASR::RealConstant_t>(source_value)->m_r;
                    source_bits.assign(reinterpret_cast<uint8_t*>(&val),
                                    reinterpret_cast<uint8_t*>(&val) + sizeof(val));
                }
            } else if (ASRUtils::is_array(ASRUtils::expr_type(source)) && ASRUtils::is_value_constant(source_value)) {
                ASR::ArrayConstant_t* const_source = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(source_value));
                ASR::ttype_t* source_type = ASRUtils::expr_type(source_value);
                int kind = ASRUtils::extract_kind_from_ttype_t(source_type);
                size_t n_elements = ASRUtils::get_fixed_size_of_array(source_type);

                if (ASRUtils::is_integer(*source_type)) {
                    if (kind == 4) {
                        for (size_t i=0; i < n_elements; i++) {
                            int32_t val = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_source, i))->m_n;
                            source_bits.insert(source_bits.end(),
                                reinterpret_cast<uint8_t*>(&val),
                                reinterpret_cast<uint8_t*>(&val) + sizeof(val));
                        }
                    } else {
                        for (size_t i=0; i < n_elements; i++) {
                            int64_t val = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_source, i))->m_n;
                            source_bits.insert(source_bits.end(),
                                reinterpret_cast<uint8_t*>(&val),
                                reinterpret_cast<uint8_t*>(&val) + sizeof(val));
                        }
                    }
                } else if (ASRUtils::is_real(*source_type)) {
                    if (kind == 4) {
                        for (size_t i=0; i < n_elements; i++) {
                            float val = ASR::down_cast<ASR::RealConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_source, i))->m_r;
                            source_bits.insert(source_bits.end(),
                                reinterpret_cast<uint8_t*>(&val),
                                reinterpret_cast<uint8_t*>(&val) + sizeof(val));
                        }
                    } else {
                        for (size_t i=0; i < n_elements; i++) {
                            double val = ASR::down_cast<ASR::RealConstant_t>(ASRUtils::fetch_ArrayConstant_value(al, const_source, i))->m_r;
                            source_bits.insert(source_bits.end(),
                                reinterpret_cast<uint8_t*>(&val),
                                reinterpret_cast<uint8_t*>(&val) + sizeof(val));
                        }
                    }
                }
            } else {
                return ASR::make_BitCast_t(al, x.base.base.loc, source, mold, size, type, nullptr);
            }

            std::size_t target_size = 0;
            if (size_value) {
                target_size = ASR::down_cast<ASR::IntegerConstant_t>(size_value)->m_n;
            } else {
                if (ASR::is_a<ASR::Integer_t>(*type)) {
                    target_size = ASR::down_cast<ASR::Integer_t>(type)->m_kind;
                } else if (ASR::is_a<ASR::Real_t>(*type)) {
                    target_size = ASR::down_cast<ASR::Real_t>(type)->m_kind;
                } else if (ASR::is_a<ASR::Complex_t>(*type)) {
                    target_size = 2 * ASR::down_cast<ASR::Complex_t>(type)->m_kind;
                } else if (ASR::is_a<ASR::Logical_t>(*type)) {
                    target_size = ASR::down_cast<ASR::Logical_t>(type)->m_kind;
                } else if (ASR::is_a<ASR::String_t>(*type)) {
                    int len = -1; ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(type)->m_len, len);
                    target_size = len;
                }
            }
            std::vector<uint8_t> result_bits(source_bits.begin(),
                                            source_bits.begin() + std::min(source_bits.size(), target_size));

            if (ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(mold))) {
                int64_t new_value = 0;
                std::memcpy(&new_value, result_bits.data(), std::min(sizeof(new_value), result_bits.size()));
                transfer_value = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, new_value, ASRUtils::expr_type(mold)));
            } else if (ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(mold))) {
                if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(mold)) == 8) {
                    double new_value = 0.0;
                    std::memcpy(&new_value, result_bits.data(), std::min(sizeof(new_value), result_bits.size()));
                    transfer_value = ASRUtils::EXPR(
                        ASR::make_RealConstant_t(al, x.base.base.loc, new_value, ASRUtils::expr_type(mold)));
                } else {
                    float new_value = 0.0;
                    std::memcpy(&new_value, result_bits.data(), std::min(sizeof(new_value), result_bits.size()));
                    transfer_value = ASRUtils::EXPR(
                        ASR::make_RealConstant_t(al, x.base.base.loc, new_value, ASRUtils::expr_type(mold)));
                }
            } else if (ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(mold))) {
                std::string new_value = "";
                for (size_t i = 0; i < result_bits.size(); i++) {
                    new_value.push_back(result_bits[i]);
                }
                Str s; s.from_str_view(new_value);
                transfer_value = ASRUtils::EXPR(
                    ASR::make_StringConstant_t(al, mold->base.loc, s.c_str(al), ASRUtils::expr_type(mold)));
            } else {
                return ASR::make_BitCast_t(al, x.base.base.loc, source, mold, size, type, nullptr);
            }
        }
        return ASR::make_BitCast_t(al, x.base.base.loc, source, mold,
                                     size, type, transfer_value);
    }

    ASR::asr_t* create_Cmplx(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"x", "y", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 3, "cmplx");
        ASR::expr_t *x_ = args[0], *y_ = args[1], *kind = args[2];
        if (x_ == nullptr) {
            diag.add(diag::Diagnostic("The first argument of `cmplx` intrinsic"
                    " must be present",
                    diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        if( ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(x_)) ) {
            if( y_ != nullptr ) {
                diag.add(diag::Diagnostic(
                    "The first argument of `cmplx` intrinsic"
                    " is of complex type, the second argument "
                    "in this case must be absent",
                    diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
            if (!ASR::is_a<ASR::Var_t>(*x_)) {
                const ASR::ComplexConstructor_t* complex_expr = ASR::down_cast<ASR::ComplexConstructor_t>(x_);
                const ASR::expr_t* real_part_expr = complex_expr->m_re;
                const ASR::expr_t* imag_part_expr = complex_expr->m_im;

                if (!ASR::is_a<ASR::RealConstant_t>(*real_part_expr)) {
                    diag.add(diag::Diagnostic("Expected a real constant for the real part",
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                } else if (!ASR::is_a<ASR::RealConstant_t>(*imag_part_expr)) {
                    diag.add(diag::Diagnostic("Expected a real constant for the imaginary part",
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            }
            int64_t kind_value = handle_kind(kind);
            if (kind_value != ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(x_))) {
                return ASR::make_Cast_t(al, x.base.base.loc, x_, ASR::cast_kindType::ComplexToComplex, 
                                                            ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc, kind_value)), nullptr);
            }
            return (ASR::asr_t*) x_;
        }
        int64_t kind_value = handle_kind(kind);
        ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al,
                                x.base.base.loc, kind_value));
        if( y_ == nullptr ) {
            y_ = ASRUtils::EXPR(ASR::make_RealConstant_t(al, x.base.base.loc,
                                                         0.0, real_type));
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc,
                                kind_value));
        ASR::expr_t* x_value = ASRUtils::expr_value(x_);
        ASR::expr_t* y_value = ASRUtils::expr_value(y_);
        ASR::expr_t* cc_expr = nullptr;
        double x_value_ = 0.0;
        double y_value_ = 0.0;
        if (x_value && y_value && ASRUtils::extract_value(x_value, x_value_) && ASRUtils::extract_value(y_value, y_value_)) {
            cc_expr = ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, x.base.base.loc,
                                                                 x_value_, y_value_, type));
        }
        // Cast x_ or y_ as necessary
        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &x_,
                                            ASRUtils::extract_type(ASRUtils::expr_type(x_)), real_type, diag);
        ImplicitCastRules::set_converted_value(al, x.base.base.loc, &y_,
                                            ASRUtils::extract_type(ASRUtils::expr_type(y_)), real_type, diag);
        return ASR::make_ComplexConstructor_t(al, x.base.base.loc, x_, y_, type, cc_expr);
    }

    ASR::asr_t* create_NullPointerConstant(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"mold"};
        handle_intrinsic_node_args(x, args, kwarg_names, 0, 1, "null");
        ASR::expr_t *mold_ = args[0];
        ASR::ttype_t* null_ptr_type_ = nullptr;
        if( mold_ ) {
            null_ptr_type_ = ASRUtils::expr_type(mold_);
        } else {
            LCOMPILERS_ASSERT(current_variable_type_ != nullptr);
            null_ptr_type_ = current_variable_type_;
        }
        return ASR::make_PointerNullConstant_t(al, x.base.base.loc, null_ptr_type_);
    }

    ASR::asr_t* create_Associated(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"pointer", "target"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "associated");
        ASR::expr_t *ptr_ = args[0], *tgt_ = args[1];
        ASR::ttype_t* associated_type_ = ASRUtils::TYPE(ASR::make_Logical_t(
                                            al, x.base.base.loc, compiler_options.po.default_integer_kind));
        return ASR::make_PointerAssociated_t(al, x.base.base.loc, ptr_, tgt_, associated_type_, nullptr);
    }

    ASR::asr_t* create_Iachar(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"C", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "iachar");
        ASR::expr_t *arg = args[0], *kind = args[1];
        int64_t kind_value = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, kind_value));
        ASR::expr_t* iachar_value = nullptr;
        ASR::expr_t* arg_value = ASRUtils::expr_value(arg);
        if( arg_value ) {
            std::string arg_str;
            bool is_const_value = ASRUtils::is_value_constant(arg_value, arg_str);
            if( is_const_value ) {
                int64_t ascii_code = uint8_t(arg_str[0]);
                iachar_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                ascii_code, type));
            }
        }
        return ASR::make_Iachar_t(al, x.base.base.loc, arg, type, iachar_value);
    }

    ASR::asr_t* create_Complex(const AST::FuncCallOrArray_t& x) {
        const Location &loc = x.base.base.loc;
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"x", "y"};
        handle_intrinsic_node_args(x, args, kwarg_names, 2, 2, "complex");

        ASR::ttype_t *arg_type0 = ASRUtils::expr_type(args[0]);
        ASR::ttype_t *arg_type1 = ASRUtils::expr_type(args[1]);
        if(!((is_integer(*arg_type0) && is_integer(*arg_type1))
            || (is_real(*arg_type0) && is_real(*arg_type1))
            || (is_integer(*arg_type0) && is_real(*arg_type1))
            || (is_real(*arg_type0) && is_integer(*arg_type1)))) {
            diag.add(Diagnostic("Unexpected args, Complex expects (int, int) or (real, real) "
                "or (int, real) or (real, int) as arguments", Level::Error, Stage::Semantic, {
                Label("", {loc})}));
            throw SemanticAbort();
        }

        ASR::expr_t* value = nullptr;
        ASR::ttype_t* ret_type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc, 4));
        int max_ret_kind = 4;
        for (size_t i = 0; i < args.size(); i++) {
            if (ASRUtils::is_real(*ASRUtils::expr_type(args[i]))) {
                max_ret_kind = std::max(max_ret_kind,
                    ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[i])));
            }
        }
        ASRUtils::set_kind_to_ttype_t(ret_type, max_ret_kind);

        if (ASRUtils::all_args_evaluated(args)) {
            double re, im;
            ASRUtils::extract_value(ASRUtils::expr_value(args[0]), re);
            ASRUtils::extract_value(ASRUtils::expr_value(args[1]), im);
            value = ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, loc, re, im, ret_type));
        }

        ASR::ttype_t* expected_arg_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
        ASRUtils::set_kind_to_ttype_t(expected_arg_type, max_ret_kind);

        ASR::expr_t* re = CastingUtil::perform_casting(args[0], expected_arg_type, al, loc);
        ASR::expr_t* im = CastingUtil::perform_casting(args[1], expected_arg_type, al, loc);

        return ASR::make_ComplexConstructor_t(al, loc, re, im, ret_type, value);
    }

    IntrinsicSignature get_intrinsic_signature(std::string& var_name) {
        if( name2signature.find(var_name) == name2signature.end() ) {
            return IntrinsicSignature({}, 1, 1);
        }
        return name2signature[var_name];
    }

    bool is_intrinsic_registry_function(std::string var_name) {
        bool is_specific_type_intrinsic = intrinsic_mapping.count(var_name);
        if (intrinsic_procedures_as_asr_nodes.is_intrinsic_present_in_ASR(var_name) ||
            intrinsic_procedures_as_asr_nodes.is_kind_based_selection_required(var_name) ||
            ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(var_name) ||
            ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(var_name) ||
            ASRUtils::IntrinsicImpureFunctionRegistry::is_intrinsic_function(var_name) ||
            is_specific_type_intrinsic) {
            return true;
        }
        return false;
    }

    bool is_intrinsic_registry_subroutine( std::string var_name ) {
        if ( ASRUtils::IntrinsicImpureSubroutineRegistry::is_intrinsic_subroutine(var_name) ) {
            return true;
        }
        return false;
    }

    void fill_optional_kind_arg(std::string &name, Vec<ASR::expr_t*> &args) {
        if (name == "aimag") {
            if (args.size() == 1) {
                Location &loc = args[0]->base.loc;
                int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, kind));
                ASRUtils::ASRBuilder b(al, loc);
                args.push_back(al, b.i_t(kind, type));
            }
        }
    }

    void scalar_kind_arg(std::string &name, Vec<ASR::expr_t*> &args) {
        std::unordered_map<std::string, int> kind_arg_index_map = {
            {"logical", 1}, {"storage_size", 1}, {"anint", 1}, {"nint", 1}, {"aint", 1},
            {"floor", 1}, {"ceiling", 1}, {"aimag", 1}, {"maskl", 1}, {"maskr", 1},
            {"ichar", 1}, {"char", 1}, {"achar", 1}, {"iachar", 1}, {"real", 1},
            {"int", 1}, {"cmplx", 1}, {"len_trim", 1}, {"len", 1}, {"shape", 1},
            {"ieee_real", 1}, {"ieee_int", 2}, {"lbound", 2}, {"ubound", 2}, {"size", 2},
            {"verify", 3}, {"index", 3}, {"scan", 3}
        };

        auto it = kind_arg_index_map.find(name);
        if (it != kind_arg_index_map.end()) {
            int kind_arg_index = it->second;

            if (args[kind_arg_index]) {
                if (ASRUtils::is_array(ASRUtils::expr_type(args[kind_arg_index]))) {
                    diag.add(Diagnostic(
                        "`kind` argument of `" + name + "` intrinsic must be a scalar",
                        Level::Error, Stage::Semantic, {
                            Label("", {args[kind_arg_index]->base.loc})
                        }));
                    throw SemanticAbort();
                }
            }
        }
    }

    void fill_optional_args(std::string intrinsic_name, Vec<ASR::expr_t*> &args, const Location &loc) {
        ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4));
        ASR::ttype_t *bool_type = ASRUtils::TYPE(
                    ASR::make_Logical_t(al, loc, 4));
        if (intrinsic_name == "selected_real_kind") {
            ASR::expr_t* zero = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 0,
                                                int_type));
            ASR::expr_t* two = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 2,
                                                int_type));
            if (args[0] == nullptr) {
                args.p[0] = zero;
            }
            if (args[1] == nullptr) {
                args.p[1] = zero;
            }
            if (args[2] == nullptr) {
                args.p[2] = two;
            }
        } else if (intrinsic_name == "verify" || intrinsic_name == "index" || intrinsic_name == "scan") {
            ASR::ttype_t *bool_type = ASRUtils::TYPE(
                    ASR::make_Logical_t(al, loc, 4));
            ASR::expr_t* f = ASRUtils::EXPR(
                ASR::make_LogicalConstant_t(al, loc, false, bool_type));
            ASR::expr_t* four = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 4, int_type));
            if (args[2] == nullptr) {
                args.p[2] = f;
            }
            if (args[3] == nullptr) {
                args.p[3] = four;
            }
        } else if (intrinsic_name == "real") {
            if (args[1] == nullptr) {
                if (is_complex(*ASRUtils::expr_type(args[0]))) {
                    int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
                    ASR::expr_t* val = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc, kind, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, kind))));
                    args.p[1] = val;
                }
            }
        } else if (intrinsic_name == "ishftc"){
            if(args[2] == nullptr){
                int value;
                int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
                value = kind*8;
                ASR::expr_t* val = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, value, int_type));
                args.p[2] = val;
            }
        } else if (intrinsic_name == "out_of_range"){
            if(args[2] == nullptr){
                bool value = false;
                ASR::expr_t* val = ASRUtils::EXPR(
                    ASR::make_LogicalConstant_t(al, loc, value, bool_type));
                args.p[2] = val;
            }
        }
    }

    void check_argument_type(const std::string& intrinsic_name, Vec<ASR::expr_t*>& args, const Location& loc, ASR::ttype_t* required_type, int required_kind = -1) {
        for (size_t i = 0; i < args.size(); i++) {
            if (args[i] != nullptr) {
                ASR::ttype_t* arg_type = ASRUtils::expr_type(args[i]);
                if (required_kind != -1) {
                    int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    if (arg_type != required_type || kind != required_kind) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of " + ASRUtils::type_to_str_fortran(required_type) + " type with kind " + std::to_string(required_kind),
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                } else {
                    if (arg_type != required_type) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of " + ASRUtils::type_to_str_fortran(required_type) + " type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            }
        }
    }

    void check_specific_type_intrinsics(std::string intrinsic_name, Vec<ASR::expr_t*> &args, const Location &loc) {
        std::set<std::string>array_intrinsic_mapping_names = {"min0", "amin0", "min1", "amin1", "dmin1", "max0", "amax0", "max1", "amax1", "dmax1"};
        if (intrinsic_mapping.find(intrinsic_name) == intrinsic_mapping.end()) {
            return;
        }

        size_t arg_size = args.size();
        if(intrinsic_name == "dint" || intrinsic_name == "dnint") {
            arg_size = 1;
            if (args[1]) {
                diag.add(Diagnostic("Too many arguments to call `" + intrinsic_name + "`",
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        }

        for (size_t i = 0; i < arg_size; i++) {
            std::string argument_type = "";
            if (array_intrinsic_mapping_names.find(intrinsic_name) != array_intrinsic_mapping_names.end()) {
                argument_type = intrinsic_mapping[intrinsic_name].second[0];
            } else {
                if(i < intrinsic_mapping[intrinsic_name].second.size()){
                    argument_type = intrinsic_mapping[intrinsic_name].second[i];
                } else {
                    diag.add(Diagnostic("Too many arguments to call `" + intrinsic_name + "`",
                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
            if (argument_type == "int4") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    if (!is_integer(*arg_type)) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of integer type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            } else if (argument_type == "real") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    if (!is_real(*arg_type)) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of real type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            } else if (argument_type == "real4") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    if (!is_real(*arg_type) || kind != 4) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of single precision real type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            } else if (argument_type == "real8") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    if (!is_real(*arg_type) || kind != 8) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of double precision real type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            } else if (argument_type == "complex") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    if (!is_complex(*arg_type)) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of complex type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            } else if (argument_type == "complex4") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    if (!is_complex(*arg_type) || kind != 4) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of single precision complex type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            } else if (argument_type == "complex8") {
                if (args[i] != nullptr) {
                    ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                    int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    if (!is_complex(*arg_type) || kind != 8) {
                        diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                            " must be of double precision complex type",
                                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            }
        }
    }

    void is_coarray_or_atomic(std::string intrinsic_name, const Location& loc){
        std::vector<std::string> coarray_intrinsics, atomic_intrinsics;
        coarray_intrinsics = {"co_broadcast", "co_max", "co_min", "co_reduce", "co_sum", "lcobound", "ucobound", "failed_images",
            "image_status", "get_team", "image_index", "num_images", "stopped_images", "team_number", "this_image", "coshape", "corank",
            "event_query"};
        atomic_intrinsics = {"atomic_add", "atomic_and", "atomic_cas", "atomic_define", "atomic_fetch_add", "atomic_fetch_and",
            "atomic_fetch_or", "atomic_fetch_xor", "atomic_or", "atomic_ref", "atomic_xor"};
        if (std::find(coarray_intrinsics.begin(), coarray_intrinsics.end(), intrinsic_name) != coarray_intrinsics.end()) {
            diag.add(diag::Diagnostic(
                    "Coarrays are not supported yet",
                    diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        } else if (std::find(atomic_intrinsics.begin(), atomic_intrinsics.end(), intrinsic_name) != atomic_intrinsics.end()) {
            diag.add(diag::Diagnostic(
                "Atomic operations are not supported yet",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
        }
    }

    /**
     * Broadcast `create_func` to elements of `args`, out of which atleast
     * one element is an array (i.e not a scalar)
     *
     * e.g. of broadcasting:
     * min([-1, 2, 3], 2, 5, [4, 4, 5], [5, -8, 7]) is broadcasted as:
     * [min(-1, 2, 5, 4, 5), min(2, 2, 5, 4, -8), min(3, 2, 5, 5, 7)]
     *
    */
    void compiletime_broadcast_elemental_intrinsic(
        Vec<ASR::expr_t*> args,
        ASR::ArrayConstant_t** result_array,
        std::vector<int> array_indices_in_args,
        ASRUtils::create_intrinsic_function create_func,
        const Location& loc,
        Allocator& al)
    {
        ASR::expr_t* first_arg_ = ASRUtils::expr_value(args[array_indices_in_args[0]]);
        size_t max_array_size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(first_arg_));
        ASR::ttype_t* array_type = ASRUtils::expr_type(first_arg_);
        Vec<ASR::expr_t*> new_expr; new_expr.reserve(al, max_array_size);

        for (size_t i = 0; i < max_array_size; i++) {
            Vec<ASR::expr_t*> intrinsic_args;
            intrinsic_args.reserve(al, args.size());

            for (size_t j = 0; j < args.size(); j++) {
                if (std::find(array_indices_in_args.begin(), array_indices_in_args.end(), j) != array_indices_in_args.end()) {
                    // Current argument is an array

                    ASR::expr_t* arg_ = ASRUtils::expr_value(args[j]);
                    ASR::ArrayConstant_t* array_arg = ASR::down_cast<ASR::ArrayConstant_t>(arg_);
                    if (max_array_size != (size_t) ASRUtils::get_fixed_size_of_array(array_arg->m_type)) {
                        diag.add(diag::Diagnostic("Different shape of arguments for broadcasting " +
                            std::to_string(max_array_size) + " and " + std::to_string(ASRUtils::get_fixed_size_of_array(array_arg->m_type)),
                            diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    intrinsic_args.push_back(al, ASRUtils::fetch_ArrayConstant_value(al, array_arg, i));
                } else {
                    // Current argument is a scalar, use as is
                    intrinsic_args.push_back(al, args[j]);
                }
            }
            // Call the intrinsic function for the current combination of arguments
            // result_array->m_args[i] = ASRUtils::expr_value(ASRUtils::EXPR(create_func(al, loc, intrinsic_args, diag)));
            if (create_func(al, loc, intrinsic_args, diag) == nullptr) {
                throw SemanticAbort();
            }
            ASR::expr_t* result = ASRUtils::expr_value(ASRUtils::EXPR(create_func(al, loc, intrinsic_args, diag)));
            array_type = ASRUtils::expr_type(result);
            new_expr.push_back(al, result);
            // ASRUtils::set_ArrayConstant_value(result_array, result, i);
        }
        ASR::Array_t* result_arr_type = ASR::down_cast<ASR::Array_t>((*result_array)->m_type);
        (*result_array)->m_type = ASRUtils::make_Array_t_util(al, result_arr_type->base.base.loc,
                                    array_type, result_arr_type->m_dims,
                                    result_arr_type->n_dims, ASR::abiType::Source, false,
                                    result_arr_type->m_physical_type);
        ASR::expr_t* new_expr_ = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, (*result_array)->base.base.loc,
                                    new_expr.p, new_expr.n, (*result_array)->m_type, (*result_array)->m_storage_format));
        if (ASR::is_a<ASR::ArrayConstant_t>(*new_expr_)) {
            (*result_array) = ASR::down_cast<ASR::ArrayConstant_t>(new_expr_);
        } else {
            (*result_array) = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(new_expr_));
        }
    }

    std::vector<int> find_array_indices_in_args(const Vec<ASR::expr_t*>& args) {
        std::vector<int> array_indices_in_args;

        for (size_t i = 0; i < args.size(); i++) {
            if (!args[i]) {
                continue;
            }
            ASR::expr_t* arg = ASRUtils::expr_value(args[i]);
            if (arg && ASR::is_a<ASR::ArrayConstant_t>(*arg)) {
                array_indices_in_args.push_back(i);
            }
        }
        return array_indices_in_args;
    }

    ASR::expr_t* fetch_arrayconstant(ASR::expr_t* var_value) {
        if (!var_value) return nullptr;
        return ASR::is_a<ASR::ArrayConstant_t>(*var_value) ? var_value : ASR::is_a<ASR::Var_t>(*var_value) ?
                fetch_arrayconstant(ASRUtils::EXPR2VAR(var_value)->m_value) : nullptr;
    }

    ASR::symbol_t* intrinsic_as_node(const AST::FuncCallOrArray_t &x,
                                     bool& is_function) {
        std::string var_name = to_lower(x.m_func);
        if (ASR::asr_t* result = handle_intrinsics_dble_float_dfloat_shifta(x, al)) {
            is_function = false;
            tmp = result;
            return nullptr;
        }
        std::string specific_var_name = var_name;
        bool is_specific_type_intrinsic = intrinsic_mapping.count(var_name);
        if( is_intrinsic_registry_function(var_name)) {
            is_function = false;
            if (is_specific_type_intrinsic) {
                specific_var_name = var_name;
                var_name = intrinsic_mapping[var_name].first;
            }
            if( ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(var_name) ||
                    ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(var_name) ) {
                IntrinsicSignature signature = get_intrinsic_signature(var_name);
                Vec<ASR::expr_t*> args;
                bool signature_matched = false;
                signature_matched = handle_intrinsic_node_args(
                    x, args, signature.kwarg_names,
                    signature.positional_args, signature.max_args,
                    var_name, false);
                check_specific_type_intrinsics(specific_var_name, args, x.base.base.loc);
                if( !signature_matched ) {
                    diag.add(Diagnostic("No matching signature found for intrinsic " + var_name,
                                        Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
                if( ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(var_name) ) {
                    const bool are_all_args_evaluated { ASRUtils::all_args_evaluated(args, true) };
                    fill_optional_kind_arg(var_name, args);
                    tmp = nullptr;
                    scalar_kind_arg(var_name, args);
                    ASRUtils::create_intrinsic_function create_func =
                        ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(var_name);

                    std::vector<int> array_indices_in_args = find_array_indices_in_args(args);
                    std::vector<std::string> inquiry_functions = {"epsilon", "radix", "range", "precision", "rank", "tiny", "huge", "bit_size", "new_line", "digits",
                        "maxexponent", "minexponent", "storage_size", "kind", "is_contiguous"};
                    if (are_all_args_evaluated &&
                        (std::find(inquiry_functions.begin(), inquiry_functions.end(), var_name) == inquiry_functions.end()) &&
                        !array_indices_in_args.empty())
                    {
                        ASR::expr_t* arg = ASRUtils::expr_value(args[array_indices_in_args[0]]);
                        ASRUtils::ExprStmtDuplicator expr_duplicator(al);
                        ASR::expr_t* arg_ = expr_duplicator.duplicate_expr(arg);
                        ASR::ArrayConstant_t* result_array = ASR::down_cast<ASR::ArrayConstant_t>(arg_);

                        compiletime_broadcast_elemental_intrinsic(args, &result_array, array_indices_in_args, create_func, x.base.base.loc, al);;
                        tmp = (ASR::asr_t*) result_array;
                    } else {
                        tmp = create_func(al, x.base.base.loc, args, diag);
                    }
                } else if ( ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(var_name) ) {
                    if(var_name == "dot_product"){
                        ASR::expr_t *matrix_a = args[0], *matrix_b = args[1];
                        ASR::ttype_t *type_a = ASRUtils::expr_type(matrix_a);
                        ASR::ttype_t *type_b = ASRUtils::expr_type(matrix_b);
                        if((ASRUtils::is_real(*type_b) && ASRUtils::is_integer(*type_a)) ||
                            (ASRUtils::is_complex(*type_b) && ASRUtils::is_integer(*type_a)) ||
                            (ASRUtils::is_complex(*type_b) && ASRUtils::is_real(*type_a)) ){
                            ImplicitCastRules::set_converted_value(al, x.base.base.loc, &matrix_a,
                                            type_a, ASRUtils::type_get_past_allocatable(type_b), diag);
                        } else if((ASRUtils::is_real(*type_a) && ASRUtils::is_integer(*type_b)) ||
                                   (ASRUtils::is_complex(*type_a) && ASRUtils::is_integer(*type_b)) ||
                                    (ASRUtils::is_complex(*type_a) && ASRUtils::is_real(*type_b)) ){
                            ImplicitCastRules::set_converted_value(al, x.base.base.loc, &matrix_b,
                                            type_b, ASRUtils::type_get_past_allocatable(type_a), diag);
                        }
                        args.p[0] = matrix_a;
                        args.p[1] = matrix_b;
                    }
                    ASRUtils::create_intrinsic_function create_func =
                        ASRUtils::IntrinsicArrayFunctionRegistry::get_create_function(var_name);
                    tmp = create_func(al, x.base.base.loc, args, diag);
                }

            } else if( ASRUtils::IntrinsicImpureFunctionRegistry::is_intrinsic_function(var_name) ) {
                Vec<ASR::expr_t*> args;
                args.reserve(al, 1);
                for( size_t i = 0; i < x.n_args; i++ ) {
                    this->visit_expr(*x.m_args[i].m_end);
                    ASR::expr_t* expr_arg = ASRUtils::EXPR(tmp);
                    if(ASRUtils::is_descriptorString(ASRUtils::expr_type(expr_arg))){
                       expr_arg = ASRUtils::cast_string_descriptor_to_pointer(al, expr_arg);
                    }
                    args.push_back(al, expr_arg);
                }
                ASRUtils::create_intrinsic_function create_func =
                    ASRUtils::IntrinsicImpureFunctionRegistry::get_create_function(var_name);
                tmp = create_func(al, x.base.base.loc, args, diag);
            } else if( var_name == "size" ) {
                tmp = create_ArraySize(x);
            } else if( var_name == "lbound" || var_name == "ubound" ) {
                tmp = create_ArrayBound(x, var_name);
            } else if( var_name == "transfer" ) {
                tmp = create_BitCast(x);
            } else if( var_name == "cmplx" ) {
                tmp = create_Cmplx(x);
            } else if( var_name == "reshape" ) {
                tmp = create_ArrayReshape(x);
            } else if( var_name == "iachar" ) {
                tmp = create_Iachar(x);
            } else if( var_name == "len" ) {
                tmp = create_StringLen(x);
            } else if( var_name == "null" ) {
                tmp = create_NullPointerConstant(x);
            } else if( var_name == "associated" ) {
                tmp = create_Associated(x);
            } else if( var_name == "complex" ) {
                tmp = create_Complex(x);
            } else if( var_name == "is_contiguous" ) {
                tmp = create_ArrayIsContiguous(x);
            } else if( startswith(var_name, "_lfortran_") ) {
                // LFortran specific
                
                if ( var_name == "_lfortran_len")
                    tmp = create_LFLen(x);
                else if ( var_name == "_lfortran_get_item")
                    tmp = create_LFGetItem(x);
                else if ( var_name == "_lfortran_list_constant")
                    tmp = create_ListConstant(x);
                else if ( var_name == "_lfortran_list_count")
                    tmp = create_ListCount(x);
                else if ( var_name == "_lfortran_set_constant")
                    tmp = create_SetConstant(x);
            } else {
                throw LCompilersException("create_" + var_name + " not implemented yet.");
            }
            if (tmp == nullptr) {
                throw SemanticAbort();
            }
            return nullptr;
        }
        return resolve_intrinsic_function(x.base.base.loc, var_name);
    }

    ASR::asr_t* create_PointerToCptr(const AST::FuncCallOrArray_t& x) {
        Vec<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"X"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 1, std::string("c_loc"));
        ASR::expr_t *v_Var = args[0];
        if( !ASR::is_a<ASR::GetPointer_t>(*v_Var) &&
            !ASRUtils::is_pointer(ASRUtils::expr_type(v_Var)) ) {
            ASR::ttype_t* ptr_type = ASRUtils::make_Pointer_t_util(al, x.base.base.loc,
                ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(v_Var)));
            v_Var = ASRUtils::EXPR(ASR::make_GetPointer_t(al, x.base.base.loc,
                            v_Var, ptr_type, nullptr));
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_CPtr_t(al, x.base.base.loc));
        return ASR::make_PointerToCPtr_t(al, x.base.base.loc, v_Var, type, nullptr);
    }

    ASR::asr_t* handle_intrinsic_float_dfloat(Allocator &al, Vec<ASR::call_arg_t> args,
                                        const Location &loc, int kind) {
        ASR::expr_t *arg = nullptr, *value = nullptr;
        ASR::ttype_t *type = nullptr;
        if (args.size() > 0) {
            arg = args[0].m_value;
            type = ASRUtils::expr_type(arg);
        }
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, kind));
        if (!arg) {
            return ASR::make_RealConstant_t(al, loc, 0.0, to_type);
        }
        if (ASRUtils::is_integer(*type)) {
            if (ASRUtils::expr_value(arg) != nullptr) {
                double dval = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(arg))->m_n;
                value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                                loc, dval, to_type));
            }
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::IntegerToReal,
                to_type, value));
        } else {
            diag.add(Diagnostic("Argument of intrinsic must be an integer",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        // TODO: Make this work if the argument is, let's say, a class.
        return nullptr;
    }

    // special handling of 'dble', 'float', 'dfloat', 'shifta' intrinsics
    // maybe once those are moved to IntrinsicElementalFunction, this might
    // not be needed
    ASR::asr_t* handle_intrinsics_dble_float_dfloat_shifta(
        const AST::FuncCallOrArray_t &x,
        Allocator &al
    ) {
        ASR::asr_t* asr_node { nullptr };
        std::string var_name = to_lower(x.m_func);
        Vec<ASR::call_arg_t> args;
        if (var_name == "dble") {
            visit_expr_list(x.m_args, x.n_args, args);
            asr_node = handle_intrinsic_dble(al, args, x.base.base.loc);
        } else if (var_name == "float" ) {
            visit_expr_list(x.m_args, x.n_args, args);
            asr_node = handle_intrinsic_float_dfloat(al, args, x.base.base.loc, 4);
        } else if (var_name == "dfloat" ) {
            visit_expr_list(x.m_args, x.n_args, args);
            asr_node = handle_intrinsic_float_dfloat(al, args, x.base.base.loc, 8);
        } else if (var_name == "shifta") {
            visit_expr_list(x.m_args, x.n_args, args);
            asr_node = create_Shifta(x.base.base.loc, args);
        }
        return asr_node;
    }

    ASR::asr_t* handle_intrinsic_dble(Allocator &al, Vec<ASR::call_arg_t> args,
                                        const Location &loc) {
        ASR::expr_t *arg = nullptr, *value = nullptr;
        ASR::ttype_t *type = nullptr;
        if (args.size() > 0) {
            arg = args[0].m_value;
            type = ASRUtils::expr_type(arg);
        }
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 8));
        if (!arg) {
            return ASR::make_RealConstant_t(al, loc, 0.0, to_type);
        }
        if (ASRUtils::is_integer(*type)) {
            if (ASRUtils::expr_value(arg) != nullptr) {
                double dval = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(arg))->m_n;
                value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                                loc, dval, to_type));
            }
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::IntegerToReal,
                to_type, value));
        } else if (ASRUtils::is_logical(*type)) {
            if (ASRUtils::expr_value(arg) != nullptr) {
                double dval = ASR::down_cast<ASR::LogicalConstant_t>(
                                        ASRUtils::expr_value(arg))->m_value;
                value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                                loc, dval, to_type));
            }
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::LogicalToReal,
                to_type, value));
        } else if (ASRUtils::is_real(*type)) {
            // float() always returns 64-bit floating point numbers.
            if (ASRUtils::extract_kind_from_ttype_t(type) != 8) {
                return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                    al, loc, arg, ASR::cast_kindType::RealToReal,
                    to_type, value));
            }
            return (ASR::asr_t *)arg;
        } else if (ASRUtils::is_complex(*type)) {
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                    al, loc, arg, ASR::cast_kindType::ComplexToReal,
                    to_type, value));
        } else {
            std::string stype = ASRUtils::type_to_str_fortran(type);
            diag.add(Diagnostic("Conversion of '" + stype + "' to float is not Implemented",
                                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        // TODO: Make this work if the argument is, let's say, a class.
        return nullptr;
    }

    template <class Call>
    void create_implicit_interface_function(const Call &x, std::string func_name, bool add_return, ASR::ttype_t* old_type) {
        is_implicit_interface = true;
        implicit_interface_parent_scope = current_scope;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        Vec<ASR::call_arg_t> c_args;
        visit_expr_list(x.m_args, x.n_args, c_args);

        Vec<ASR::expr_t*> args;
        args.reserve(al, x.n_args);
        std::string sym_name = to_lower(func_name);
        for (size_t i=0; i<x.n_args; i++) {
            std::string arg_name = sym_name + "_arg_" + std::to_string(i);
            arg_name = to_lower(arg_name);
            ASR::expr_t *var_expr = c_args[i].m_value;
            ASR::symbol_t *v;
            if (ASR::is_a<ASR::Var_t>(*var_expr) &&
                    ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::Var_t>(var_expr)->m_v)) {
                v = ASR::down_cast<ASR::Var_t>(var_expr)->m_v;
            } else {
                ASR::ttype_t *var_type = ASRUtils::expr_type(var_expr);
                if (ASRUtils::is_array(var_type)) {
                    // For arrays like A(n, m) we use A(*) in BindC, so that
                    // the C ABI is just a pointer
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(
                        ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(var_type))
                    );
                    var_type = ASRUtils::duplicate_type_with_empty_dims(al, var_type,
                        ( array_type->m_physical_type == ASR::array_physical_typeType::UnboundedPointerToDataArray ) ?
                        array_type->m_physical_type : ASR::array_physical_typeType::PointerToDataArray, true);
                } else if (ASR::is_a<ASR::ArrayItem_t>(*var_expr) && compiler_options.legacy_array_sections) {
                    ASR::symbol_t* func_sym = parent_scope->resolve_symbol(func_name);
                    ASR::Function_t* func = nullptr;
                    if (func_sym) {
                        func = ASR::down_cast<ASR::Function_t>(func_sym);
                    }
                    if (func && func->n_args > 0 && func->n_args <= x.n_args && ASRUtils::is_array(ASRUtils::expr_type(func->m_args[i]))) {
                        ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(var_expr);
                        size_t n_dims = array_item->n_args;
                        Vec<ASR::dimension_t> empty_dims;
                        empty_dims.reserve(al, n_dims);
                        for( size_t i = 0; i < n_dims; i++ ) {
                            ASR::dimension_t empty_dim;
                            empty_dim.loc = var_type->base.loc;
                            empty_dim.m_start = nullptr;
                            empty_dim.m_length = nullptr;
                            empty_dims.push_back(al, empty_dim);
                        }
                        var_type = ASRUtils::duplicate_type(al, var_type, &empty_dims, ASR::array_physical_typeType::DescriptorArray, true);
                    }
                }
                SetChar variable_dependencies_vec;
                variable_dependencies_vec.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, var_type);
                v = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, x.base.base.loc,
                    current_scope, s2c(al, arg_name), variable_dependencies_vec.p,
                    variable_dependencies_vec.size(), ASRUtils::intent_unspecified,
                    nullptr, nullptr, ASR::storage_typeType::Default, var_type, nullptr,
                    ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required,
                    false));
                current_scope->add_or_overwrite_symbol(arg_name, v);
            }
            LCOMPILERS_ASSERT(v != nullptr)
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                v)));
        }
        ASR::ttype_t *type = old_type;
        ASR::expr_t *to_return = nullptr;
        if (add_return) {
            std::string return_var_name = sym_name + "_return_var_name";
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
            ASR::asr_t *return_var = ASRUtils::make_Variable_t_util(al, x.base.base.loc,
                current_scope, s2c(al, return_var_name), variable_dependencies_vec.p,
                variable_dependencies_vec.size(), ASRUtils::intent_return_var,
                nullptr, nullptr, ASR::storage_typeType::Default, type, nullptr,
                ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required,
                false);
            current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
            to_return = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                ASR::down_cast<ASR::symbol_t>(return_var)));
        }

        tmp = ASRUtils::make_Function_t_util(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, sym_name),
            nullptr, 0,
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ nullptr,
            /* n_body */ 0,
            /* a_return_var */ to_return,
            ASR::abiType::BindC, ASR::accessType::Public, ASR::deftypeType::Interface,
            nullptr, false, false, false, false, false, nullptr, 0,
            false, false, false);
        parent_scope->add_or_overwrite_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;

        is_implicit_interface = false;
        implicit_interface_parent_scope = nullptr;
    }

    void visit_DataImpliedDo(const AST::DataImpliedDo_t& x) {
        Vec<ASR::expr_t*> a_values_vec;
        ASR::expr_t *a_start, *a_end, *a_increment;
        a_start = a_end = a_increment = nullptr;
        a_values_vec.reserve(al, x.n_object_list);
        ASR::ttype_t* type = nullptr;
        Vec<ASR::ttype_t*> type_tuple;
        type_tuple.reserve(al, 1);
        bool unique_type = true;
        for( size_t i = 0; i < x.n_object_list; i++ ) {
            this->visit_expr(*(x.m_object_list[i]));
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            ASR::ttype_t* type_ = ASRUtils::expr_type(expr);
            if( type == nullptr ) {
                type = type_;
            } else {
                if (!unique_type || !ASRUtils::types_equal(type_, type)) {
                    unique_type = false;
                }
            }
            type_tuple.push_back(al, type_);

            a_values_vec.push_back(al, expr);
        }
        this->visit_expr(*(x.m_start));
        a_start = ASRUtils::EXPR(tmp);
        this->visit_expr(*(x.m_end));
        a_end = ASRUtils::EXPR(tmp);
        if( x.m_increment != nullptr ) {
            this->visit_expr(*(x.m_increment));
            a_increment = ASRUtils::EXPR(tmp);
        }
        ASR::expr_t** a_values = a_values_vec.p;
        size_t n_values = a_values_vec.size();

        ASR::symbol_t* a_sym = current_scope->resolve_symbol(to_lower(x.m_var));
        if (a_sym == nullptr) {
            diag.add(Diagnostic("The implied do loop variable '" +
                to_lower(x.m_var) + "' is not declared",
                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        ASR::expr_t* a_var = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, a_sym));
        if( !unique_type ) {
            type = ASRUtils::TYPE(ASR::make_Tuple_t(al, x.base.base.loc, type_tuple.p, type_tuple.size()));
        }
        tmp = ASR::make_ImpliedDoLoop_t(al, x.base.base.loc, a_values, n_values,
                                        a_var, a_start, a_end, a_increment,
                                        type, nullptr);
    }

    bool contains_loop_vars(ASR::expr_t* expr, std::vector<ASR::symbol_t*>& loop_vars) {

        class ImpliedDoLoopValuesVisitor : public ASR::BaseWalkVisitor<ImpliedDoLoopValuesVisitor> {
            public:
            std::vector<ASR::symbol_t*>& loop_vars;
            bool &contain_loop_vars;

            ImpliedDoLoopValuesVisitor(std::vector<ASR::symbol_t*>& loop_vars, bool &contain_loop_vars) :
                loop_vars(loop_vars), contain_loop_vars(contain_loop_vars) {}

            void visit_Var(const ASR::Var_t &x) {
                ASR::symbol_t* sym = x.m_v;
                if (std::find(loop_vars.begin(), loop_vars.end(), sym) != loop_vars.end()) {
                    contain_loop_vars &= true;
                    return;
                }
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(
                                            ASRUtils::symbol_get_past_external(sym));
                contain_loop_vars &= ASRUtils::is_value_constant(var->m_value);
                return;
            }

            void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
                this->visit_expr(*x.m_left);
                if (!contain_loop_vars) return;
                this->visit_expr(*x.m_right);
                if (!contain_loop_vars) return;
            }

            void visit_RealBinOp(const ASR::RealBinOp_t &x) {
                this->visit_expr(*x.m_left);
                if (!contain_loop_vars) return;
                this->visit_expr(*x.m_right);
                if (!contain_loop_vars) return;
            }

            void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t &x) {
                for (size_t i = 0; i < x.n_args; i++) {
                    this->visit_expr(*x.m_args[i]);
                    if (!contain_loop_vars) return;
                }
            }

            void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t &/*x*/) {
                // TODO: will have to handle this
                contain_loop_vars = false;
            }
        };

        bool contain_loop_vars = true;
        ImpliedDoLoopValuesVisitor visitor(loop_vars, contain_loop_vars);
        visitor.visit_expr(*expr);
        return contain_loop_vars;
    }

    bool is_compiletime_implied_do_loop(ASR::ImpliedDoLoop_t* idl, std::vector<ASR::symbol_t*>& loop_vars) {
        if ((!ASRUtils::is_value_constant(idl->m_start) && !contains_loop_vars(idl->m_start, loop_vars)) ||
            (!ASRUtils::is_value_constant(idl->m_end) && !contains_loop_vars(idl->m_end, loop_vars)) ||
            (idl->m_increment != nullptr && !ASRUtils::is_value_constant(idl->m_increment) && !contains_loop_vars(idl->m_increment, loop_vars))) {
            return false;
        }

        for (size_t i = 0; i < idl->n_values; i++) {
            ASR::expr_t* expr = idl->m_values[i];
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*expr)) {
                if (!is_compiletime_implied_do_loop(ASR::down_cast<ASR::ImpliedDoLoop_t>(expr), loop_vars)) {
                    return false;
                }
            }
            if (!ASRUtils::is_value_constant(expr)) {
                // may be possible that it contains a loop variable
                if (!contains_loop_vars(expr, loop_vars)) {
                    return false;
                }
            }
        }
        return true;
    }

    void fetch_implied_do_loop_variables(ASR::ImpliedDoLoop_t* idl, std::vector<ASR::symbol_t*>& loop_vars) {
        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(idl->m_var);
        loop_vars.push_back(var->m_v);
        for (size_t i = 0; i < idl->n_values; i++) {
            ASR::expr_t* expr = idl->m_values[i];
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*expr)) {
                fetch_implied_do_loop_variables(ASR::down_cast<ASR::ImpliedDoLoop_t>(expr), loop_vars);
            }
        }
        return;
    }

    template<typename T>
    T get_constant_value(ASR::expr_t* expr, ImpliedDoLoopValuesVisitor& visitor) {
        visitor.value = nullptr;
        visitor.visit_expr(*expr);
        T res;
        if constexpr (std::is_same_v<T,bool>) {
            res = ASR::down_cast<ASR::LogicalConstant_t>(visitor.value)->m_value;
        } else if constexpr (std::is_same_v<T,int>) {
            res = ASR::down_cast<ASR::IntegerConstant_t>(visitor.value)->m_n;
        } else if constexpr (std::is_same_v<T,float> || std::is_same_v<T,double>) {
            res = ASR::down_cast<ASR::RealConstant_t>(visitor.value)->m_r;
        }
        return res;
    }

    template<typename T>
    void populate_compiletime_array_for_idl(ASR::ImpliedDoLoop_t* idl, Vec<T> &array, std::vector<ASR::symbol_t*> &loop_vars, std::vector<int> &loop_indices, int &curr_nesting_level, int &itr) {
        /*
        (j, (i * j, i=1, 3), j=1, 2)
        gets translated via cpp code
        int *array = new int[4*2]

        int itr = 0;
        for (int j = 1; j <= 2; j++) {
            array[itr] = j;
            itr++;
            for (int i = 1; i <= 3; i++) {
                array[itr] = i * j;
                itr++;
            }
        }
        */
        ImpliedDoLoopValuesVisitor index_bound_visitor(al, loop_vars, loop_indices, nullptr, idl->m_type, diag);
        int end;
        if (ASRUtils::expr_value(idl->m_end)) {
            end = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(idl->m_end))->m_n;
        } else {
            end = get_constant_value<int>(idl->m_end, index_bound_visitor);
        }
        int start;
        if (ASRUtils::expr_value(idl->m_start)) {
            start = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(idl->m_start))->m_n;
        } else {
            start = get_constant_value<int>(idl->m_start, index_bound_visitor);
        }
        int increment;
        if (idl->m_increment) {
            if (ASRUtils::expr_value(idl->m_increment)) {
                increment = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(idl->m_increment))->m_n;
            } else {
                increment = get_constant_value<int>(idl->m_increment, index_bound_visitor);
            }
        } else {
            increment = 1;
        }

        for ( int j = start; j <= end; j += increment ) {
            loop_indices[curr_nesting_level] = j;
            for ( size_t i = 0; i < idl->n_values; i++ ) {
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*idl->m_values[i])) {
                    curr_nesting_level++;
                    populate_compiletime_array_for_idl(ASR::down_cast<ASR::ImpliedDoLoop_t>(idl->m_values[i]), array, loop_vars, loop_indices, curr_nesting_level, itr);
                } else {
                    ImpliedDoLoopValuesVisitor visitor(al, loop_vars, loop_indices, nullptr, idl->m_type, diag);
                    array.push_back(al, get_constant_value<T>(idl->m_values[i], visitor));
                    itr++;
                }
            }
        }
        curr_nesting_level--;
    }

    void visit_ImpliedDoLoop(const AST::ImpliedDoLoop_t& x) {
        idl_nesting_level++;
        Vec<ASR::expr_t*> a_values_vec;
        ASR::expr_t *a_start, *a_end, *a_increment;
        a_start = a_end = a_increment = nullptr;
        a_values_vec.reserve(al, x.n_values);
        ASR::ttype_t* type = nullptr;
        Vec<ASR::ttype_t*> type_tuple;
        type_tuple.reserve(al, 1);
        bool unique_type = true;
        for( size_t i = 0; i < x.n_values; i++ ) {
            this->visit_expr(*(x.m_values[i]));
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            ASR::ttype_t* type_ = ASRUtils::expr_type(expr);
            if( type == nullptr ) {
                type = type_;
            } else {
                if (!unique_type || !ASRUtils::types_equal(type_, type)) {
                    unique_type = false;
                }
            }
            type_tuple.push_back(al, type_);

            a_values_vec.push_back(al, expr);
        }
        this->visit_expr(*(x.m_start));
        a_start = ASRUtils::EXPR(tmp);
        this->visit_expr(*(x.m_end));
        a_end = ASRUtils::EXPR(tmp);
        if( x.m_increment != nullptr ) {
            this->visit_expr(*(x.m_increment));
            a_increment = ASRUtils::EXPR(tmp);
        }
        ASR::expr_t** a_values = a_values_vec.p;
        size_t n_values = a_values_vec.size();

        ASR::symbol_t* a_sym = current_scope->resolve_symbol(to_lower(x.m_var));
        if (a_sym == nullptr) {
            diag.add(Diagnostic("The implied do loop variable '" +
                to_lower(x.m_var) + "' is not declared",
                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        ASR::expr_t* a_var = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, a_sym));
        if( !unique_type ) {
            type = ASRUtils::TYPE(ASR::make_Tuple_t(al, x.base.base.loc, type_tuple.p, type_tuple.size()));
        }
        tmp = ASR::make_ImpliedDoLoop_t(al, x.base.base.loc, a_values, n_values,
                                        a_var, a_start, a_end, a_increment,
                                        type, nullptr);
        ASR::ImpliedDoLoop_t* idl = (ASR::ImpliedDoLoop_t*) tmp;

        // fetch loop variables
        std::vector<ASR::symbol_t*> loop_vars; fetch_implied_do_loop_variables(idl, loop_vars);

        // check compiletime evaluation possibility
        bool is_compiletime = is_compiletime_implied_do_loop(idl, loop_vars);

        if (is_compiletime && idl_nesting_level == 1) {
            std::vector<int> loop_indices; // fill it with all zero
            for (size_t i = 0; i < loop_vars.size(); i++) {
                loop_indices.push_back(0);
            }

            void *data = nullptr;
            int itr = 0, curr_nesting_level = 0;
            // TODO: handle multiple types
            // populate compiletime array
            if (ASRUtils::is_integer(*type)) {
                Vec<int> array; array.reserve(al, 1);
                populate_compiletime_array_for_idl(idl, array, loop_vars, loop_indices, curr_nesting_level, itr);
                data = &array.p[0];
            } else if (ASRUtils::is_logical(*type)) {
                Vec<bool> array; array.reserve(al, 1);
                populate_compiletime_array_for_idl(idl, array, loop_vars, loop_indices, curr_nesting_level, itr);
                data = &array.p[0];
            } else if (ASRUtils::is_real(*type)) {
                int kind = ASRUtils::extract_kind_from_ttype_t(type);

                if (kind == 4) {
                    Vec<float> array; array.reserve(al, 1);
                    populate_compiletime_array_for_idl(idl, array, loop_vars, loop_indices, curr_nesting_level, itr);
                    data = &array.p[0];
                } else if (kind == 8) {
                    Vec<double> array; array.reserve(al, 1);
                    populate_compiletime_array_for_idl(idl, array, loop_vars, loop_indices, curr_nesting_level, itr);
                    data = &array.p[0];
                } else {
                    diag.add(Diagnostic("Unsupported kind for real type in compiletime evaluation of implied do loop",
                                        Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            }
            if (data != nullptr) {
                Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
                ASR::dimension_t dim; dim.loc = x.base.base.loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4))));
                dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, itr, ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4))));
                dims.push_back(al, dim);

                ASR::ttype_t* array_type = ASRUtils::TYPE(ASR::make_Array_t(al, x.base.base.loc, type, dims.p, dims.n, ASR::array_physical_typeType::FixedSizeArray));

                ASR::expr_t* value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, x.base.base.loc, itr * ASRUtils::extract_kind_from_ttype_t(type), data,
                        array_type, ASR::arraystorageType::ColMajor));
                idl->m_value = value;
                tmp = (ASR::asr_t*) idl;
            }
        }
        idl_nesting_level--;
    }

    ASR::asr_t* create_Shifta(const Location &loc, Vec<ASR::call_arg_t> args) {
        /*
            shifta(n, w):
            This is arithmetic shift right by w bits.
            Represent using BinOp, with left = n, right = w, op = BitRShift
        */
        ASR::expr_t *n = args[0].m_value;
        ASR::expr_t *w = args[1].m_value;

        ASR::ttype_t* n_type = ASRUtils::expr_type(n);
        ASR::ttype_t* w_type = ASRUtils::expr_type(w);

        if (!ASRUtils::check_equal_type(n_type, w_type)) {
            if (ASRUtils::is_integer(*n_type) && ASRUtils::is_integer(*w_type)) {
                w = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, w, ASR::cast_kindType::IntegerToInteger, n_type, nullptr));
            }
        }

        return ASRUtils::make_Binop_util(al, loc, ASR::binopType::BitRShift,
                            n, w, n_type);
    }

    void visit_FuncCallOrArray(const AST::FuncCallOrArray_t &x) {
        std::string var_name = to_lower(x.m_func);
        if (x.n_temp_args > 0) {
            ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
            var_name = handle_templated(x.m_func, ASR::is_a<ASR::Template_t>(*ASRUtils::get_asr_owner(owner_sym)),
                x.m_temp_args, x.n_temp_args, x.base.base.loc);
        }
        SymbolTable *scope = current_scope;
        ASR::symbol_t *v = nullptr;
        ASR::expr_t *v_expr = nullptr;
        bool is_external_procedure = check_is_external(var_name);
        bool is_explicit_intrinsic = check_is_explicit_intrinsic(var_name);
        // If this is a type bound procedure (in a class) it won't be in the
        // main symbol table. Need to check n_member.
        if (x.n_member >= 1) {
            if (x.n_member ==  1) {
                if (x.m_member[0].n_args > 0) {
                    ASR::symbol_t *v1 = current_scope->resolve_symbol(to_lower(x.m_member[0].m_name));
                    ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v1);
                    tmp = create_ArrayRef(x.base.base.loc, x.m_member[0].m_args, x.m_member[0].n_args, nullptr, 0, nullptr, v1, f2);
                } else {
                    tmp = resolve_variable(x.base.base.loc, to_lower(x.m_member[0].m_name));
                }
                tmp = (ASR::asr_t*) replace_with_common_block_variables(ASRUtils::EXPR(tmp));
            } else {
                visit_NameUtil(x.m_member, x.n_member - 1,
                    x.m_member[x.n_member - 1].m_name, x.base.base.loc);
            }
            v_expr = ASRUtils::EXPR(tmp);
            v = resolve_deriv_type_proc(x.base.base.loc, var_name,
                    to_lower(x.m_member[x.n_member - 1].m_name),
                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_expr)), scope);
            v = ASRUtils::import_class_procedure(al, x.base.base.loc, v, current_scope);
        } else {
            v = current_scope->resolve_symbol(var_name);
        }
        if (!v || (v && (is_external_procedure || is_explicit_intrinsic))) {
            ASR::symbol_t* external_sym = is_external_procedure ? v : nullptr;
            bool is_function = true;
            if ( !is_external_procedure ) {
                v = intrinsic_as_node(x, is_function);
            }
            if( !is_function ) {
                return;
            }
            if (compiler_options.implicit_interface && is_function && ( !v || (v && is_external_procedure))) {
                // Function Call is not defined in this case.
                // We need to create an interface and add the Function into
                // the symbol table.
                // Currently using real*8 as the return type.
                ASR::ttype_t* type = external_sym ? ASRUtils::symbol_type(external_sym) :
                                    ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, 8));
                std::string var_name_first_letter = to_lower(std::string(1, var_name[0]));
                implicit_dictionary = implicit_mapping[get_hash(current_scope->asr_owner)];
                if ( !external_sym && compiler_options.implicit_typing &&
                     implicit_dictionary.find(var_name_first_letter) != implicit_dictionary.end() ) {
                    type = implicit_dictionary[var_name_first_letter];
                }
                create_implicit_interface_function(x, var_name, true, type);
                v = current_scope->resolve_symbol(var_name);
                LCOMPILERS_ASSERT(v!=nullptr);
                // check if external sym is updated, or: say if signature of external_sym and original_sym are different
                if (v && external_sym && is_external_procedure && ASRUtils::is_external_sym_changed(v, external_sym)) {
                    changed_external_function_symbol[ASRUtils::symbol_name(v)] = v;
                }
                // remove from external_procedures_mapping
                if (v && is_external_procedure) {
                    erase_from_external_mapping(var_name);
                }
                ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface, changed_external_function_symbol);
            }
        }
        // if v is a function which has null pointer return type, give error
        if (ASR::is_a<ASR::Function_t>(*v)){
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(v);
            if (func->m_return_var == nullptr){
                diag.add(Diagnostic("Subroutine `" + var_name + "` called as a function",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
        if (( ASR::is_a<ASR::Variable_t>(*v) || is_external_procedure )
            && (!ASRUtils::is_array(ASRUtils::symbol_type(v)))
            && (!ASRUtils::is_character(*ASRUtils::symbol_type(v)))) {
            if (intrinsic_procedures.is_intrinsic(var_name) || is_intrinsic_registry_function(var_name)) {
                if (compiler_options.implicit_interface) {
                    bool is_function = true;
                    if ( !is_external_procedure ) {
                        v = intrinsic_as_node(x, is_function);
                    }
                    if( !is_function ) {
                        return;
                    }
                } else {
                    diag.semantic_error_label(
                            var_name + " was declared as a variable, it can't be called as a function",
                            {x.base.base.loc},
                            "help: use the compiler option \"--implicit-interface\" to use intrinsic functions"
                        );
                    throw SemanticAbort();
                }
            } else if (compiler_options.implicit_interface && !ASRUtils::is_symbol_procedure_variable(v)) {
                bool is_function = true;
                // NOTE: ideally this shouldn't be needed, this is only to handle
                // 'dble', 'shifta', 'float', 'dfloat', which aren't currently
                // implemented as intrinsic elemental function
                intrinsic_as_node(x, is_function);
                if (!is_function) {
                    return;
                }

                // If implicit interface is allowed, we have to handle the
                // following case here:
                // real :: x
                // print *, x(5)
                // Which is a function call.
                // We remove "x" from the symbol table and instead recreate it.
                // We use the type of the old "x" as the return value type.
                std::map<std::string, ASR::symbol_t*> scope_ = current_scope->get_scope();
                bool in_current_scope = (scope_.find(var_name) != scope_.end());
                SymbolTable* sym_scope = current_scope;
                if (in_current_scope) {
                    current_scope->erase_symbol(var_name);
                } else {
                    ASR::symbol_t* sym_ = current_scope->get_symbol(var_name);
                    while(!sym_) {
                        sym_scope = sym_scope->parent;
                        sym_ = sym_scope->get_symbol(var_name);
                    }
                }
                ASR::ttype_t* old_type = ASRUtils::symbol_type(v);
                create_implicit_interface_function(x, var_name, true, old_type);
                v = current_scope->resolve_symbol(var_name);
                LCOMPILERS_ASSERT(v!=nullptr);
                if (!in_current_scope && is_external_procedure) {
                    SymbolTable* temp_scope = current_scope;
                    current_scope = sym_scope;
                    create_implicit_interface_function(x, var_name, true, old_type);
                    current_scope = temp_scope;
                    LCOMPILERS_ASSERT(sym_scope->resolve_symbol(var_name)!=nullptr);
                }

                // erase from external_procedures_mapping
                erase_from_external_mapping(var_name);
                ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface, changed_external_function_symbol);

                // Update arguments if the symbol belonged to a function
                if (current_scope->asr_owner) {
                    ASR::symbol_t* asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
                    if (ASR::is_a<ASR::Function_t>(*asr_owner_sym)) {
                        ASR::Function_t *current_function = ASR::down_cast<ASR::Function_t>(asr_owner_sym);
                        for (size_t i = 0; i < current_function->n_args; i++) {
                            if (ASR::is_a<ASR::Var_t>(*current_function->m_args[i])) {
                                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(current_function->m_args[i]);
                                if (std::string(ASRUtils::symbol_name(var->m_v)) == var_name) {
                                    var->m_v = v;
                                }
                            }
                        }
                    }
                }
            }
        }
        if (v && !compiler_options.implicit_interface && is_external_procedure) {
            /*
                Case: ./integration_tests/external_01.f90
                We have `enorm` declared outside current_scope. Check if it is a function
                and if it is, then we need to remove template function `enorm` from current scope and external procedures.
            */
            ASR::symbol_t* v2 = current_scope->parent->resolve_symbol(var_name);
            if (ASR::is_a<ASR::Function_t>(*v2)) {
                current_scope->erase_symbol(var_name);
                erase_from_external_mapping(var_name);
                ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface, changed_external_function_symbol);
                v = v2;
            }
        }
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                if (intrinsic_module_procedures_as_asr_nodes.find(var_name) != intrinsic_module_procedures_as_asr_nodes.end()) {
                    if (var_name == "c_loc") {
                        tmp = create_PointerToCptr(x);
                    } else if (var_name == "c_associated") {
                        tmp = create_Associated(x);
                    } else if (var_name == "c_funloc") {
                        tmp = create_PointerToCptr(x);
                    } else {
                        LCOMPILERS_ASSERT(false)
                    }
                    return;
                }
            }
        } else if (ASR::is_a<ASR::Struct_t>(*f2)) {
            // Check for any interface overriding a constructor for the struct
            ASR::symbol_t* interface_override_s = current_scope->resolve_symbol("~" + var_name);
            if (interface_override_s) {
                v = interface_override_s;
                f2 = ASRUtils::symbol_get_past_external(interface_override_s);
            }
        }
        if (ASR::is_a<ASR::Function_t>(*f2) ||
            ASR::is_a<ASR::GenericProcedure_t>(*f2) ||
            (ASR::is_a<ASR::Variable_t>(*f2) &&
            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::symbol_type(f2))) ) {
            if (ASRUtils::is_intrinsic_symbol(f2)) {
                // Here we handle all intrinsic functions that are implemented
                // in Fortran, but have different interface (API), e.g.,
                // the `kind` argument is handled differently, such as `not`.
                // In these cases we have to handle them here, since we need
                // to process the arguments ourselves, not via comparison
                // with the `not` implementation.
                if (var_name == "not") {
                    Vec<ASR::call_arg_t> args;
                    visit_expr_list(x.m_args, x.n_args, args);
                    LCOMPILERS_ASSERT(args.size() == 1);
                    ASR::expr_t* operand = args[0].m_value;
                    ASR::expr_t* value = nullptr;
                    ASR::ttype_t* operand_type = ASRUtils::expr_type(operand);
                    if (ASRUtils::is_integer(*operand_type)) {
                        if (ASRUtils::expr_value(operand) != nullptr) {
                            int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                                   ASRUtils::expr_value(operand))
                                                   ->m_n;
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(
                                al, x.base.base.loc, ~op_value, operand_type));
                        }
                        tmp = ASR::make_IntegerBitNot_t(
                            al, x.base.base.loc, operand, operand_type, value);
                        return;
                    } else {
                        diag.add(Diagnostic("Argument of `not` intrinsic must be INTEGER",
                                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                        throw SemanticAbort();
                    }
                }
            }
            Vec<ASR::call_arg_t> args;
            Vec<ASR::call_arg_t> args_with_mdt;
            visit_expr_list(x.m_args, x.n_args, args);
            if (x.n_member >= 1) {
                args_with_mdt.reserve(al, x.n_args + 1);
                ASR::call_arg_t v_expr_call_arg;
                v_expr_call_arg.loc = v_expr->base.loc;
                v_expr_call_arg.m_value = v_expr;
                args_with_mdt.push_back(al, v_expr_call_arg);
                for( size_t i = 0; i < args.size(); i++ ) {
                    args_with_mdt.push_back(al, args[i]);
                }
            }
            if (x.n_keywords > 0) {
                if (ASR::is_a<ASR::Function_t>(*f2)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
                    diag::Diagnostics diags;
                    visit_kwargs(args, x.m_keywords, x.n_keywords,
                        f->m_args, f->n_args, x.base.base.loc, f,
                        diags, x.n_member);
                    if( diags.has_error() ) {
                        diag.diagnostics.insert(diag.diagnostics.end(),
                            diags.diagnostics.begin(), diags.diagnostics.end());
                        throw SemanticAbort();
                    }
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
                    ASR::GenericProcedure_t* gp = ASR::down_cast<ASR::GenericProcedure_t>(f2);
                    bool function_found = false;
                    bool is_nopass = false;
                    bool is_class_procedure = false;
                    for( int i = 0; i < (int) gp->n_procs; i++ ) {
                        ASR::symbol_t* f4 = gp->m_procs[i];
                        if( !ASR::is_a<ASR::Function_t>(*f4) && !ASR::is_a<ASR::ClassProcedure_t>(*f4) ) {
                            diag.add(Diagnostic(std::string(ASRUtils::symbol_name(f4)) +
                            " is not a function.",
                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                            throw SemanticAbort();
                        }
                        if (ASR::is_a<ASR::ClassProcedure_t>(*f4)) {
                            ASR::ClassProcedure_t* f5 = ASR::down_cast<ASR::ClassProcedure_t>(f4);
                            f4 = f5->m_proc;
                            is_nopass = f5->m_is_nopass;
                            is_class_procedure = true;
                        }
                        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f4);
                        diag::Diagnostics diags;

                        Vec<ASR::call_arg_t> args_copy;
                        args_copy.reserve(al, args.size() + x.n_keywords + (is_class_procedure && !is_nopass ? 1 : 0));
                        for( size_t j = 0; j < args.size(); j++ ) {
                            args_copy.push_back(al, args[j]);
                        }
                        visit_kwargs(args_copy, x.m_keywords, x.n_keywords,
                            f->m_args, f->n_args, x.base.base.loc, f,
                            diags, x.n_member, is_nopass);
                        // Add 'this' if type-bound and not nopass
                        if (is_class_procedure && !is_nopass && x.n_member >= 1) {
                            ASR::call_arg_t this_arg;
                            this_arg.loc = v_expr->base.loc;
                            this_arg.m_value = v_expr;
                            args_copy.push_front(al, this_arg);
                        }
                        if( diags.has_error() ) {
                            continue ;
                        }
                        int idx = ASRUtils::select_generic_procedure(args_copy, *gp, x.base.base.loc,
                                        [&](const std::string &msg, const Location &loc) {
                                                diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                                                throw SemanticAbort();
                                            },
                                        false);
                        if( idx == i ) {
                            function_found = true;
                            args.n = 0;
                            args.from_pointer_n_copy(al, args_copy.p , args_copy.size() );
                            args_with_mdt.n = 0;
                            args_with_mdt.from_pointer_n_copy(al, args_copy.p , args_copy.size() );
                            break;
                        }
                    }
                    if( !function_found ) {
                        bool is_function = true;
                        v = intrinsic_as_node(x, is_function);
                        if( !is_function ) {
                            return ;
                        }
                    }
                    if( v == nullptr ) {
                        diag.add(Diagnostic("Unable to find a function to bind for generic procedure call, " + std::string(gp->m_name),
                                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                        throw SemanticAbort();
                    }
                }
            }
            if (ASR::is_a<ASR::Function_t>(*f2) && !ASRUtils::is_intrinsic_symbol(f2)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
                // check type mismatch (only function in templates)
                if (is_template) {
                    for (size_t i = 0; i < f->n_args; i++) {
                        ASR::expr_t *dest = f->m_args[i];
                        ASR::expr_t *source = args.p[i].m_value;

                        ASR::ttype_t *dest_type = ASRUtils::expr_type(dest);
                        ASR::ttype_t *source_type = ASRUtils::expr_type(source);

                        if (!ASRUtils::check_equal_type(dest_type, source_type)) {
                            std::string dtype = ASRUtils::type_to_str_fortran(dest_type);
                            std::string stype = ASRUtils::type_to_str_fortran(source_type);
                            diag.add(Diagnostic(
                                "Type mismatch in function call, the function expects '" + dtype + "' but '" + stype + "' was provided",
                                Level::Error, Stage::Semantic, {
                                    Label("type '" + dtype + "' expected, but '" + stype + "' provided",
                                            {source->base.loc}),
                                    Label("function definition has parameter type '" + dtype + "'",
                                            {dest->base.loc})
                                })
                            );
                            throw SemanticAbort();
                        }
                    }
                }
            }
            if (x.n_member >= 1) {
                tmp = create_FunctionCallWithASTNode(x, v, args_with_mdt, true);
            } else {
                tmp = create_FunctionCallWithASTNode(x, v, args);
            }
        } else {
            switch (f2->type) {
            case(ASR::symbolType::Variable): {
                // TODO: Make create_StringRef for character (non-array) variables.
                tmp = create_ArrayRef(x.base.base.loc, x.m_args, x.n_args,
                                      x.m_subargs, x.n_subargs, v_expr, v, f2);
                break;
            }
            case(ASR::symbolType::Struct): {
                tmp = create_DerivedTypeConstructor(x.base.base.loc, x.m_args, x.n_args,
                                                    x.m_keywords, x.n_keywords, v);
                break;
            }
            case(ASR::symbolType::ClassProcedure):
                tmp = create_ClassProcedure(x.base.base.loc, x.m_args, x.n_args, v, v_expr); break;
            default: {
                diag.add(Diagnostic("Symbol '" + var_name
                            + "' is not a function or an array",
                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
                }
            }
        }
    }

    void check_global_procedure_and_enable_separate_compilation(SymbolTable *parent_scope) {
        if ( parent_scope->parent != nullptr ) {
            return;
        }
        compiler_options.separate_compilation = true;
        compiler_options.po.intrinsic_symbols_mangling = true;
        return;
    }


    ASR::symbol_t* resolve_intrinsic_function(const Location &loc, const std::string &remote_sym) {
        if (!intrinsic_procedures.is_intrinsic(remote_sym)) {
            if (compiler_options.implicit_interface) {
                return nullptr;
            } else {
                is_coarray_or_atomic(remote_sym, loc);
                diag.add(Diagnostic("Function '" + remote_sym + "' not found"
                    " (not user defined nor intrinsic)",
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        std::string module_name = intrinsic_procedures.get_module(remote_sym, loc, diag);

        SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);

        ASR::Module_t *m = ASRUtils::load_module(al, tu_symtab, module_name,
                loc, true, compiler_options.po, true,
                [&](const std::string &msg, const Location &loc) {
                        diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }, lm, compiler_options.generate_object_code
                );

        ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
        if (!t) {
            diag.add(Diagnostic("The symbol '" + remote_sym
                + "' not found in the module '" + module_name + "'",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        } else if (! (ASR::is_a<ASR::GenericProcedure_t>(*t)
                    || ASR::is_a<ASR::Function_t>(*t)
                    )) {
            diag.add(Diagnostic("The symbol '" + remote_sym
                + "' found in the module '" + module_name + "', "
                + "but it is not a function, subroutine or a generic procedure.",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        char *fn_name = ASRUtils::symbol_name(t);
        ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
            al, t->base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ fn_name,
            t,
            m->m_name, nullptr, 0, fn_name,
            ASR::accessType::Private
            );
        std::string sym = fn_name;

        current_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(fn));
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(fn);
        if (current_module) {
            // We are in body visitor
            // Add the module `m` to current module dependencies
            SetChar vec;
            vec.from_pointer_n_copy(al, current_module->m_dependencies,
                        current_module->n_dependencies);
            vec.push_back(al, m->m_name);
            current_module->m_dependencies = vec.p;
            current_module->n_dependencies = vec.size();
        } else {
            // We are in the symtab visitor or body visitor (the
            // current_module_dependencies is not used in body visitor)
            current_module_dependencies.push_back(al, m->m_name);
        }
        return v;
    }

    bool is_integer(ASR::ttype_t &t) {
        return ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(&t));
    }

    bool is_real(ASR::ttype_t &t) {
        return ASR::is_a<ASR::Real_t>(*ASRUtils::type_get_past_pointer(&t));
    }

    bool is_complex(ASR::ttype_t &t) {
        return ASR::is_a<ASR::Complex_t>(*ASRUtils::type_get_past_pointer(&t));
    }

    bool is_logical(ASR::ttype_t &t) {
        return ASR::is_a<ASR::Logical_t>(*ASRUtils::type_get_past_pointer(&t));
    }

    bool assignment_types_agree(ASR::ttype_t *target, ASR::ttype_t *value) {
        // For now we will just check basic type mismatch
        if (target->type == value->type) {
            return true;
        }
        if (is_integer(*target) && is_integer(*value)) {
            return true;
        }
        if (is_real(*target) && is_real(*value)) {
            return true;
        }
        return false;
    }

    template<typename T>
    T perform_binop(T left_value, T right_value, ASR::binopType op) {
        T result;
        switch (op) {
            case ASR::Add:
                result = left_value + right_value;
                break;
            case ASR::Sub:
                result = left_value - right_value;
                break;
            case ASR::Mul:
                result = left_value * right_value;
                break;
            case ASR::Div:
                result = left_value / right_value;
                break;
            case ASR::Pow:
                result = std::pow(left_value, right_value);
                break;
            default:
                LCOMPILERS_ASSERT(false);
                result = 0;
        }
        return result;
    }
    // Creates a compile-time expression value for the Binop expression, if possible.
    ASR::expr_t* visit_BinOp_helper(ASR::expr_t* left, ASR::expr_t* right, ASR::binopType op, const Location& loc, ASR::ttype_t* dest_type) {
        LCOMPILERS_ASSERT((left != nullptr) && (right != nullptr));
        if (ASR::is_a<ASR::RealConstant_t>(*left) && ASR::is_a<ASR::RealConstant_t>(*right)) {
            double left_value = ASR::down_cast<ASR::RealConstant_t>(left)->m_r;
            double right_value = ASR::down_cast<ASR::RealConstant_t>(right)->m_r;
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, left->base.loc,
            perform_binop(left_value, right_value, op), dest_type));
        } else if (ASR::is_a<ASR::RealConstant_t>(*left) && ASR::is_a<ASR::IntegerConstant_t>(*right)){
            LCOMPILERS_ASSERT(op == ASR::binopType::Pow);
            double left_value = ASR::down_cast<ASR::RealConstant_t>(left)->m_r;
            int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n;
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, left->base.loc,
                    std::pow(left_value, right_value), dest_type));
        } else if (ASR::is_a<ASR::IntegerConstant_t>(*left) && ASR::is_a<ASR::IntegerConstant_t>(*right)) {
            int64_t left_value = ASR::down_cast<ASR::IntegerConstant_t>(left)->m_n;
            int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n;

            if (op == ASR::Div && right_value == 0) {
                diag.add(Diagnostic(
                    "Division by zero",
                    Level::Error, Stage::Semantic, {
                        Label("", {loc})
                    })
                );
                throw SemanticAbort();
            }

            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, left->base.loc,
                    perform_binop(left_value, right_value, op), dest_type));
        } else if (ASR::is_a<ASR::ComplexConstant_t>(*left) && ASR::is_a<ASR::ComplexConstant_t>(*right)) {
            ASR::ComplexConstant_t *left_value
                = ASR::down_cast<ASR::ComplexConstant_t>(
                        ASRUtils::expr_value(left));
            ASR::ComplexConstant_t *right_value
                = ASR::down_cast<ASR::ComplexConstant_t>(
                        ASRUtils::expr_value(right));
            std::complex<double> left_value_(left_value->m_re, left_value->m_im);
            std::complex<double> right_value_(right_value->m_re, right_value->m_im);
            std::complex<double> result = perform_binop(left_value_, right_value_, op);
            return ASRUtils::EXPR( ASR::make_ComplexConstant_t(al, loc,
                    std::real(result), std::imag(result), dest_type));
        }
        return nullptr;
    }

    ASR::expr_t* extract_value(ASR::expr_t* left_value, ASR::expr_t* right_value, ASR::binopType op, ASR::ttype_t* dest_type, const Location& loc) {
        if (left_value && right_value &&
            ASR::is_a<ASR::ArrayConstant_t>(*left_value) &&
            ASR::is_a<ASR::ArrayConstant_t>(*right_value)) {
            ASR::ArrayConstant_t* left_array = ASR::down_cast<ASR::ArrayConstant_t>(left_value);
            ASR::ArrayConstant_t* right_array = ASR::down_cast<ASR::ArrayConstant_t>(right_value);

            Vec<ASR::expr_t*> values; values.reserve(al, ASRUtils::get_fixed_size_of_array(left_array->m_type));

            for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(left_array->m_type); i++) {
                values.push_back(al, visit_BinOp_helper(ASRUtils::fetch_ArrayConstant_value(al, left_array, i),
                                ASRUtils::fetch_ArrayConstant_value(al, right_array, i), op, loc, ASRUtils::expr_type(ASRUtils::fetch_ArrayConstant_value(al, left_array, i))));
            }

            return ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc,
                                    values.p, values.size(), dest_type,
                                    ASR::arraystorageType::ColMajor));
        }
        return nullptr;
    }

    void visit_BinOp2(Allocator &al, const AST::BinOp_t &x,
                    ASR::expr_t *&left, ASR::expr_t *&right,
                    ASR::asr_t *&asr, std::string& intrinsic_op_name,
                    SymbolTable* curr_scope) {
        ASR::binopType op;
        switch (x.m_op) {
            case (AST::Add):
                op = ASR::Add;
                break;
            case (AST::Sub):
                op = ASR::Sub;
                break;
            case (AST::Mul):
                op = ASR::Mul;
                break;
            case (AST::Div):
                op = ASR::Div;
                break;
            case (AST::Pow):
                op = ASR::Pow;
                break;
            // Fix compiler warning:
            default: {
                LCOMPILERS_ASSERT(false);
                op = ASR::binopType::Pow;
            }
        }

        // Cast LHS or RHS if necessary
        ASR::ttype_t *left_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(left));
        ASR::ttype_t *right_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(right));
        ASR::expr_t *overloaded = nullptr;
        if ( ASRUtils::use_overloaded(left, right, op,
                    intrinsic_op_name, curr_scope, asr, al,
                    x.base.base.loc, current_function_dependencies,
                    current_module_dependencies,
                    [&](const std::string &msg, const Location &loc)
                    {
                        diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }) ) {
            overloaded = ASRUtils::EXPR(asr);
        }

        ASR::expr_t **conversion_cand = &left;
        ASR::ttype_t *source_type = left_type;
        ASR::ttype_t *dest_type = right_type;

        if( overloaded == nullptr ) {
          if(!ASRUtils::is_type_parameter(*left_type) && !ASRUtils::is_type_parameter(*right_type)){
              ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                      right_type, conversion_cand,
                                                      &source_type, &dest_type);
          }
            if((op == ASR::binopType::Pow) &&
                ASRUtils::is_real(*dest_type) &&
                ASRUtils::is_integer(*right_type)){ // Don't cast exponent to preserve precision.
                // Do nothing.
            } else {
                ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                                    source_type, dest_type, diag);
            }
        }

        if( (ASRUtils::is_array(right_type) || ASRUtils::is_array(left_type)) &&
             !ASRUtils::is_array(dest_type) ) {
            ASR::dimension_t* m_dims = nullptr;
            size_t n_dims = 0;
            if( ASRUtils::is_array(left_type) ) {
                n_dims = ASRUtils::extract_dimensions_from_ttype(left_type, m_dims);
            } else if( ASRUtils::is_array(right_type) ) {
                n_dims = ASRUtils::extract_dimensions_from_ttype(right_type, m_dims);
            }
            dest_type = ASRUtils::make_Array_t_util(al, dest_type->base.loc,
                ASRUtils::type_get_past_pointer(dest_type), m_dims, n_dims);
            if( ASR::is_a<ASR::Allocatable_t>(*left_type) || ASR::is_a<ASR::Allocatable_t>(*right_type) ) {
                dest_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, dest_type->base.loc, dest_type));
            }
        }
        if((op == ASR::binopType::Pow) &&
            ASRUtils::is_real(*dest_type) &&
            ASRUtils::is_integer(*right_type)) {
            // Don't Check.
        } else if (!ASRUtils::check_equal_type(ASRUtils::expr_type(left),
                                    ASRUtils::expr_type(right)) && overloaded == nullptr) {
            std::string ltype = ASRUtils::type_to_str_fortran(ASRUtils::expr_type(left));
            std::string rtype = ASRUtils::type_to_str_fortran(ASRUtils::expr_type(right));
            diag.add(Diagnostic(
                "Type mismatch in binary operator, the types must be compatible",
                Level::Error, Stage::Semantic, {
                    Label("type mismatch (" + ltype + " and " + rtype + ")",
                            {left->base.loc, right->base.loc})
                })
            );
            throw SemanticAbort();
        }
        ASR::expr_t *value = nullptr;

        if (ASRUtils::is_integer(*dest_type)) {

            if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr ) {
                value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
            }

            ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
            value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
            asr = ASR::make_IntegerBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

        } else if (ASRUtils::is_real(*dest_type)) {

            if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
                value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
            }

            ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
            value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
            asr = ASR::make_RealBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

        } else if (ASRUtils::is_complex(*dest_type)) {

            if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
                value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
            }

            ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
            value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
            asr = ASR::make_ComplexBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

        } else if (ASRUtils::is_character(*dest_type)) {
            diag.semantic_error_label(
                            "Binary numeric operators cannot be used on strings",
                            {x.base.base.loc},
                            "help: use '//' for string concatenation"
                        );
            throw SemanticAbort();
        } else if (ASRUtils::is_type_parameter(*left_type) || ASRUtils::is_type_parameter(*right_type)) {
            // if overloaded is not found, then reject
            if (overloaded == nullptr) {
                std::string op_str = "+";
                switch (op) {
                    case (ASR::Add):
                        break;
                    case (ASR::Sub):
                        op_str = "-";
                        break;
                    case (ASR::Mul):
                        op_str = "*";
                        break;
                    case (ASR::Div):
                        op_str = "/";
                        break;
                    case (ASR::Pow):
                        op_str = "**";
                        break;
                    default:
                        LCOMPILERS_ASSERT(false);
                }
            diag.add(Diagnostic("Operator `" + op_str + "` undefined for the types in the expression `" + ASRUtils::type_to_str_fortran(left_type)
                                + " " +  op_str + " " + ASRUtils::type_to_str_fortran(right_type) + "`", Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        } else if( overloaded == nullptr ) {
            LCOMPILERS_ASSERT(false);
        }

        if (overloaded != nullptr) {
            asr = ASR::make_OverloadedBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value, overloaded);
        }

    }

    // TODO: extract commonality with visit_Instantiate
    std::string handle_templated(std::string name, bool is_nested,
            AST::decl_attribute_t** args, size_t n_args, const Location &loc) {
        std::string func_name = name;

        ASR::symbol_t *sym0 = current_scope->resolve_symbol(func_name);
        if (!sym0) {
            diag.add(Diagnostic("Use of an unspecified templated function '" + func_name
                + "'", Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }

        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(sym0);
        if (!ASR::is_a<ASR::Template_t>(*sym)) {
            diag.add(Diagnostic("Cannot instantiate a non-templated function '" + func_name
                + "'", Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }

        ASR::Template_t* temp = ASR::down_cast<ASR::Template_t>(sym);

        if (temp->n_args != n_args) {
            diag.add(Diagnostic("Number of templated function arguments don't match",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }

        std::map<std::string, ASR::ttype_t*> type_subs;
        std::map<std::string, ASR::symbol_t*> symbol_subs;

        for (size_t i=0; i<n_args; i++) {
            std::string param = temp->m_args[i];
            ASR::symbol_t *param_sym = temp->m_symtab->get_symbol(param);
            if (AST::is_a<AST::AttrType_t>(*args[i])) {
                // Handling types as instantiate's arguments
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 0);
                ASR::symbol_t *type_declaration;
                ASR::ttype_t *arg_type = determine_type(args[i]->base.loc, param,
                    args[i], false, false, dims, type_declaration, current_procedure_abi_type);
                ASR::ttype_t *param_type = ASRUtils::symbol_type(param_sym);
                if (!ASRUtils::is_type_parameter(*param_type)) {
                    diag.add(Diagnostic("The type " + ASRUtils::type_to_str_fortran(arg_type) +
                        " cannot be applied to non-type parameter " + param, Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
                type_subs[param] = arg_type;
            } else if (AST::is_a<AST::AttrNamelist_t>(*args[i])) {
                AST::AttrNamelist_t *attr_name = AST::down_cast<AST::AttrNamelist_t>(args[i]);
                std::string arg = to_lower(attr_name->m_name);
                if (ASR::is_a<ASR::Function_t>(*param_sym)) {
                    // Handling functions passed as instantiate's arguments
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(param_sym);
                    ASR::symbol_t *f_arg0 = current_scope->resolve_symbol(arg);
                    if (!f_arg0) {
                        diag.add(Diagnostic("The function argument " + arg + " is not found",
                            Level::Error, Stage::Semantic, {Label("", {args[i]->base.loc})}));
                        throw SemanticAbort();
                    }
                    ASR::symbol_t *f_arg = ASRUtils::symbol_get_past_external(f_arg0);
                    if (!ASR::is_a<ASR::Function_t>(*f_arg)) {
                        diag.add(Diagnostic("The argument for " + param + " must be a function",
                            Level::Error, Stage::Semantic, {Label("", {args[i]->base.loc})}));
                        throw SemanticAbort();
                    }
                    check_restriction(type_subs,
                        symbol_subs, f, f_arg0, loc, diag, []() { throw SemanticAbort(); });
                } else {
                    ASR::ttype_t *param_type = ASRUtils::symbol_type(param_sym);
                    if (ASRUtils::is_type_parameter(*param_type)) {
                        // Handling type parameters passed as instantiate's arguments
                        ASR::symbol_t *arg_sym0 = current_scope->resolve_symbol(arg);
                        ASR::symbol_t *arg_sym = ASRUtils::symbol_get_past_external(arg_sym0);
                        ASR::ttype_t *arg_type = nullptr;
                        if (ASR::is_a<ASR::Struct_t>(*arg_sym)) {
                            arg_type = ASRUtils::TYPE(ASRUtils::make_StructType_t_util(al, args[i]->base.loc, arg_sym0));
                        } else {
                            arg_type = ASRUtils::symbol_type(arg_sym);
                        }
                        type_subs[param] = ASRUtils::duplicate_type(al, arg_type);
                    } else {
                        // Handling local variables passed as instantiate's arguments
                        ASR::symbol_t *arg_sym = current_scope->resolve_symbol(arg);
                        ASR::ttype_t *arg_type = ASRUtils::symbol_type(arg_sym);
                        if (!ASRUtils::check_equal_type(arg_type, param_type)) {
                            diag.add(Diagnostic("The type of " + arg + " does not match the type of " + param,
                                Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        }
                        symbol_subs[param] = arg_sym;
                    }
                }
            } else if (AST::is_a<AST::AttrIntrinsicOperator_t>(*args[i])) {
                AST::AttrIntrinsicOperator_t *intrinsic_op
                    = AST::down_cast<AST::AttrIntrinsicOperator_t>(args[i]);
                ASR::binopType binop = ASR::Add;
                ASR::cmpopType cmpop = ASR::Eq;
                bool is_binop = false, is_cmpop = false;
                std::string op_name;
                switch (intrinsic_op->m_op) {
                    case (AST::PLUS):
                        is_binop = true; binop = ASR::Add; op_name = "~add"; break;
                    case (AST::MINUS):
                        is_binop = true; binop = ASR::Sub; op_name = "~sub"; break;
                    case (AST::STAR):
                        is_binop = true; binop = ASR::Mul; op_name = "~mul"; break;
                    case (AST::DIV):
                        is_binop = true; binop = ASR::Div; op_name = "~div"; break;
                    case (AST::POW):
                        is_binop = true; binop = ASR::Pow; op_name = "~pow"; break;
                    case (AST::EQ):
                        is_cmpop = true; cmpop = ASR::Eq; op_name = "~eq"; break;
                    case (AST::NOTEQ):
                        is_cmpop = true; cmpop = ASR::NotEq; op_name = "~neq"; break;
                    case (AST::LT):
                        is_cmpop = true; cmpop = ASR::Lt; op_name = "~lt"; break;
                    case (AST::LTE):
                        is_cmpop = true; cmpop = ASR::LtE; op_name = "~lte"; break;
                    case (AST::GT):
                        is_cmpop = true; cmpop = ASR::Gt; op_name = "~gt"; break;
                    case (AST::GTE):
                        is_cmpop = true; cmpop = ASR::GtE; op_name = "~gte"; break;
                    default:
                        diag.add(Diagnostic("Unsupported binary operator",
                            Level::Error, Stage::Semantic, {Label("", {args[i]->base.loc})}));
                        throw SemanticAbort();
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
                    for (size_t i = 0; i < gen_proc->n_procs && !found; i++) {
                        ASR::symbol_t* proc = gen_proc->m_procs[i];
                        found = check_restriction(type_subs,
                                    symbol_subs, f, proc, loc, diag,
                                    []() { throw SemanticAbort(); }, false);
                    }
                }

                // if not found, then try to build a function for intrinsic operator
                if (!found) {
                    if (f->n_args != 2) {
                        diag.add(Diagnostic("The restriction " + f_name
                            + " does not have 2 parameters", Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }

                    ASR::ttype_t *left_type = ASRUtils::subs_expr_type(type_subs, f->m_args[0]);
                    ASR::ttype_t *right_type = ASRUtils::subs_expr_type(type_subs, f->m_args[1]);
                    ASR::ttype_t *ftype = ASRUtils::subs_expr_type(type_subs, f->m_return_var);

                    SymbolTable *parent_scope = current_scope;
                    current_scope = al.make_new<SymbolTable>(parent_scope);
                    Vec<ASR::expr_t*> args;
                    args.reserve(al, 2);
                    for (size_t i=0; i<2; i++) {
                        std::string var_name = "arg" + std::to_string(i);
                        ASR::asr_t *v = ASRUtils::make_Variable_t_util(al, loc, current_scope,
                            s2c(al, var_name), nullptr, 0, ASR::intentType::In, nullptr,
                            nullptr, ASR::storage_typeType::Default,
                            (i == 0 ? ASRUtils::duplicate_type(al, left_type)
                                : ASRUtils::duplicate_type(al, right_type)),
                            nullptr, ASR::abiType::Source, ASR::accessType::Private,
                            ASR::presenceType::Required, false);
                        current_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(v));
                        ASR::symbol_t *var = current_scope->get_symbol(var_name);
                        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, var)));
                    }

                    std::string func_name = parent_scope->get_unique_name(op_name + "_intrinsic");

                    ASR::ttype_t *return_type = nullptr;
                    ASR::expr_t *value = nullptr;
                    ASR::expr_t *left = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        current_scope->get_symbol("arg0")));
                    ASR::expr_t *right = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        current_scope->get_symbol("arg1")));

                    ASR::expr_t **conversion_cand = &left;
                    ASR::ttype_t *source_type = left_type;
                    ASR::ttype_t *dest_type = right_type;

                    if (is_binop) {
                        ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                                     right_type, conversion_cand,
                                                                     &source_type, &dest_type);
                        ImplicitCastRules::set_converted_value(al, loc, conversion_cand,
                                                               source_type, dest_type, diag);
                        return_type = ASRUtils::duplicate_type(al, ftype);
                        value = ASRUtils::EXPR(ASRUtils::make_Binop_util(al, loc, binop, left, right, dest_type));
                        if (!ASRUtils::check_equal_type(dest_type, return_type)) {
                            diag.add(Diagnostic("Unapplicable types for intrinsic operator " + op_name,
                                Level::Error, Stage::Semantic, {Label("", {loc})}));
                            throw SemanticAbort();
                        }
                    } else {
                        return_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, compiler_options.po.default_integer_kind));
                        value = ASRUtils::EXPR(ASRUtils::make_Cmpop_util(al, loc, cmpop, left, right, left_type));
                    }

                    ASR::asr_t *return_v = ASRUtils::make_Variable_t_util(al, loc,
                        current_scope, s2c(al, "ret"), nullptr, 0,
                        ASR::intentType::ReturnVar, nullptr, nullptr, ASR::storage_typeType::Default,
                        return_type, nullptr, ASR::abiType::Source,
                        ASR::accessType::Private, ASR::presenceType::Required, false);
                    current_scope->add_symbol("ret", ASR::down_cast<ASR::symbol_t>(return_v));
                    ASR::expr_t *return_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        current_scope->get_symbol("ret")));

                    Vec<ASR::stmt_t*> body;
                    body.reserve(al, 1);
                    ASR::symbol_t *return_sym = current_scope->get_symbol("ret");
                    ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_sym));
                    ASRUtils::make_ArrayBroadcast_t_util(al, loc, target, value);
                    ASR::stmt_t *assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc,
                        target, value, nullptr, compiler_options.po.realloc_lhs));
                    body.push_back(al, assignment);

                    ASR::asr_t *op_function = ASRUtils::make_Function_t_util(
                        al, loc, current_scope, s2c(al, func_name),
                        nullptr, 0, args.p, 2, body.p, 1, return_expr,
                        ASR::abiType::Source, ASR::accessType::Public,
                        ASR::deftypeType::Implementation, nullptr, false, true,
                        false, false, false, nullptr, 0, false, false, true);
                    ASR::symbol_t *op_sym = ASR::down_cast<ASR::symbol_t>(op_function);
                    parent_scope->add_symbol(func_name, op_sym);

                    Vec<ASR::symbol_t*> symbols;
                    if (parent_scope->get_symbol(op_name) != nullptr) {
                        ASR::CustomOperator_t *old_c = ASR::down_cast<ASR::CustomOperator_t>(
                            parent_scope->get_symbol(op_name));
                        symbols.reserve(al, old_c->n_procs + 1);
                        for (size_t i=0; i<old_c->n_procs; i++) {
                            symbols.push_back(al, old_c->m_procs[i]);
                        }
                    } else {
                        symbols.reserve(al, 1);
                    }
                    symbols.push_back(al, ASR::down_cast<ASR::symbol_t>(op_function));
                    ASR::asr_t *c = ASR::make_CustomOperator_t(al, loc,
                        parent_scope, s2c(al, op_name), symbols.p, symbols.size(), ASR::Public);
                    parent_scope->add_or_overwrite_symbol(op_name, ASR::down_cast<ASR::symbol_t>(c));

                    current_scope = parent_scope;
                    symbol_subs[f->m_name] = op_sym;
                }
            } else {
                throw LCompilersException("Unsupported argument to instantiate statement.");
            }
        }

        ASR::symbol_t *s = temp->m_symtab->resolve_symbol(func_name);

        SymbolTable *target_scope = current_scope;
        if (is_nested) {
            target_scope = current_scope->parent;
        }

        std::string new_func_name = target_scope->get_unique_name("__instantiated_" + func_name);

        ASR::symbol_t* new_s = instantiate_symbol(al, target_scope, type_subs, symbol_subs, new_func_name, s);
        instantiate_body(al, type_subs, symbol_subs, new_s, s);

        return new_func_name;
    }

    void visit_BinOp(const AST::BinOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        visit_BinOp2(al, x, left, right, tmp, binop2str[x.m_op], current_scope);

        if (ASR::is_a<ASR::IntegerBinOp_t>(*ASRUtils::EXPR(tmp))) {
            ASR::IntegerBinOp_t *bin_op = ASR::down_cast<ASR::IntegerBinOp_t>(ASRUtils::EXPR(tmp));
            ASR::expr_t *right_value = ASRUtils::expr_value(bin_op->m_right);
            if (bin_op->m_op == ASR::Div && right_value != nullptr && ASR::is_a<ASR::IntegerConstant_t>(*right_value)) {
                ASR::IntegerConstant_t *right_const = ASR::down_cast<ASR::IntegerConstant_t>(right_value);
                if (right_const->m_n == 0) {
                    diag.add(Diagnostic(
                        "Division by zero",
                        Level::Error, Stage::Semantic, {
                            Label("", {x.base.base.loc})
                        })
                    );
                    throw SemanticAbort();
                }
            }
        }
    }

    void visit_DefBinOp(const AST::DefBinOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);

        ASR::ttype_t *left_type = ASRUtils::expr_type(left);
        ASR::ttype_t *right_type = ASRUtils::expr_type(right);

        ASR::Struct_t *left_struct = nullptr;
        if ( ASR::is_a<ASR::StructType_t>(*left_type) ) {
            left_struct = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::StructType_t>(
                left_type)->m_derived_type));
        }

        ASR::symbol_t* sym = current_scope->resolve_symbol(x.m_op);
        ASR::symbol_t *op_sym = ASRUtils::symbol_get_past_external(sym);
        if ( left_struct != nullptr && op_sym == nullptr) {
            op_sym = left_struct->m_symtab->resolve_symbol(
                "~def_op~" + std::string(x.m_op));
            if (op_sym == nullptr) {
                diag.add(Diagnostic("`" + std::string(x.m_op)
                    + "` is not defined in the Struct: `" + left_struct->m_name
                    + "`", Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }
        if (op_sym == nullptr) {
            diag.add(Diagnostic("`" + std::string(x.m_op)
                + "` is not defined or imported", Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }

        ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(op_sym);
        LCOMPILERS_ASSERT(gen_proc->n_procs == 1)
        ASR::symbol_t* proc;
        if ( ASR::is_a<ASR::ClassProcedure_t>(*gen_proc->m_procs[0]) ) {
            proc =  ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::ClassProcedure_t>(
                gen_proc->m_procs[0])->m_proc);
        } else {
            proc = gen_proc->m_procs[0];
        }
        switch(proc->type) {
            case ASR::symbolType::Function: {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
                std::string matched_func_name = "";
                if( func->n_args == 2 ) {
                    ASR::ttype_t* left_arg_type = ASRUtils::expr_type(func->m_args[0]);
                    ASR::ttype_t* right_arg_type = ASRUtils::expr_type(func->m_args[1]);
                    if( ASRUtils::check_equal_type(left_arg_type, left_type) &&
                        ASRUtils::check_equal_type(right_arg_type, right_type) ) {
                        Vec<ASR::call_arg_t> a_args;
                        a_args.reserve(al, 2);
                        ASR::call_arg_t left_call_arg, right_call_arg;

                        left_call_arg.loc = left->base.loc;
                        left_call_arg.m_value = left;
                        a_args.push_back(al, left_call_arg);

                        right_call_arg.loc = right->base.loc;
                        right_call_arg.m_value = right;
                        a_args.push_back(al, right_call_arg);

                        std::string func_name = to_lower(func->m_name);
                        if( current_scope->resolve_symbol(func_name) ) {
                            matched_func_name = func_name;
                        } else {
                            std::string mangled_name = func_name + "@" + std::string(x.m_op);
                            matched_func_name = mangled_name;
                        }
                        ASR::symbol_t* a_name = current_scope->resolve_symbol(matched_func_name);
                        if( a_name == nullptr ) {
                            diag.add(Diagnostic("Unable to resolve matched function: `"
                                + matched_func_name + "` for defined binary operation",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                            throw SemanticAbort();
                        }
                        ASR::ttype_t *return_type = nullptr;
                        ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, a_args);
                        if (first_array_arg) {
                            ASR::dimension_t* array_dims;
                            size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(first_array_arg), array_dims
                            );
                            Vec<ASR::dimension_t> new_dims;
                            new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                            return_type = ASRUtils::duplicate_type(al,
                                            ASRUtils::get_FunctionType(func)->m_return_var_type,
                                            &new_dims);
                        } else {
                            return_type = ASRUtils::expr_type(func->m_return_var);
                        }
                        if (sym != nullptr && ASRUtils::symbol_parent_symtab(sym)->get_counter() != current_scope->get_counter()) {
                            ADD_ASR_DEPENDENCIES_WITH_NAME(current_scope, sym, current_function_dependencies, s2c(al, matched_func_name));
                        }
                        ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
                        ASRUtils::set_absent_optional_arguments_to_null(a_args, func, al);
                        tmp = ASRUtils::make_FunctionCall_t_util(al, x.base.base.loc,
                            a_name, sym, a_args.p, 2, return_type,
                            nullptr, nullptr, false);
                    } else {
                        diag.add(Diagnostic("Arguements type and Parameters type "
                            "does not match", Level::Error, Stage::Semantic, {Label("", {proc->base.loc})}));
                        throw SemanticAbort();
                    }
                }
                break;
            }
            default: {
                diag.add(Diagnostic("Only function can be used in the "
                    "defined binary operators", Level::Error, Stage::Semantic, {Label("", {proc->base.loc})}));
                throw SemanticAbort();
            }
        }
    }

    void visit_BoolOp(const AST::BoolOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_BoolOp(al, x, left, right, tmp, diag);
    }

    void visit_StrOp(const AST::StrOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        if(ASRUtils::is_descriptorString(ASRUtils::expr_type(left))){
            left = ASRUtils::cast_string_descriptor_to_pointer(al, left);
        }
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        if(ASRUtils::is_descriptorString(ASRUtils::expr_type(right))){
            right = ASRUtils::cast_string_descriptor_to_pointer(al, right);
        }
        std::string intrinsic_op_name = intrinsic2str[AST::intrinsicopType::CONCAT];
        LCOMPILERS_ASSERT(x.m_op == AST::Concat)
        ASR::ttype_t *left_type_ = ASRUtils::expr_type(left);
        ASR::ttype_t *right_type_ = ASRUtils::expr_type(right);
        ASR::ttype_t *left_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(left_type_));
        ASR::ttype_t *right_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(right_type_));

        if( ASR::is_a<ASR::String_t>(*left_type) &&
            ASR::is_a<ASR::String_t>(*right_type) ) {
            ASR::String_t *left_type2 = ASR::down_cast<ASR::String_t>(left_type);
            ASR::String_t *right_type2 = ASR::down_cast<ASR::String_t>(right_type);
            LCOMPILERS_ASSERT(ASRUtils::extract_n_dims_from_ttype(left_type) == 0);
            LCOMPILERS_ASSERT(ASRUtils::extract_n_dims_from_ttype(right_type) == 0);
            int a_len = -1;
            int64_t l_len, r_len;
            ASR::expr_t* expr_len;
            if (ASRUtils::extract_value(left_type2->m_len, l_len) &&
                ASRUtils::extract_value(right_type2->m_len, r_len)) {
                a_len = l_len + r_len;
                expr_len = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, a_len,
                    ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 8))));
            } else {
                expr_len = nullptr;
            }
            ASR::ttype_t *dest_type = ASR::down_cast<ASR::ttype_t>(ASR::make_String_t(
                al, x.base.base.loc, left_type2->m_kind, expr_len,
                ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::PointerString));

            if( (ASRUtils::is_array(right_type_) || ASRUtils::is_array(left_type_)) &&
                !ASRUtils::is_array(dest_type) ) {
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = 0;
                if( ASRUtils::is_array(left_type_) ) {
                    n_dims = ASRUtils::extract_dimensions_from_ttype(left_type_, m_dims);
                } else if( ASRUtils::is_array(right_type_) ) {
                    n_dims = ASRUtils::extract_dimensions_from_ttype(right_type_, m_dims);
                }
                dest_type = ASRUtils::make_Array_t_util(al, dest_type->base.loc,
                    ASRUtils::type_get_past_pointer(dest_type), m_dims, n_dims);
                if( ASR::is_a<ASR::Allocatable_t>(*left_type_) || ASR::is_a<ASR::Allocatable_t>(*right_type_) ) {
                    dest_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, dest_type->base.loc, dest_type));
                }
            }

            ASR::expr_t *value = nullptr;
            // Assign evaluation to `value` if possible, otherwise leave nullptr
            ASR::expr_t* left_value = ASRUtils::expr_value(left);
            ASR::expr_t* right_value = ASRUtils::expr_value(right);
            if (left_value != nullptr && right_value != nullptr) {
                ASR::ttype_t* left_value_type = ASRUtils::expr_type(left_value);
                ASR::String_t* left_value_type2 = ASR::down_cast<ASR::String_t>(left_value_type);
                char* left_value_ = ASR::down_cast<ASR::StringConstant_t>(left_value)->m_s;
                char* right_value_ = ASR::down_cast<ASR::StringConstant_t>(right_value)->m_s;
                ASR::ttype_t *dest_value_type = ASR::down_cast<ASR::ttype_t>(
                    ASR::make_String_t(al, x.base.base.loc, left_value_type2->m_kind, 
                        ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, strlen(left_value_) + strlen(right_value_),
                            ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4)))),
                        ASR::string_length_kindType::ExpressionLength,
                        ASR::string_physical_typeType::PointerString));
                char* result;
                std::string result_s = std::string(left_value_) + std::string(right_value_);
                Str s; s.from_str_view(result_s);
                result = s.c_str(al);
                int64_t len; ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(dest_value_type)->m_len, len);
                LCOMPILERS_ASSERT((int64_t)strlen(result) == len)
                value = ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(
                    al, x.base.base.loc, result, dest_value_type));
            }
            tmp = ASR::make_StringConcat_t(al, x.base.base.loc, left, right, dest_type,
                                    value);
        } else {
            ASR::symbol_t* sym = current_scope->resolve_symbol(intrinsic_op_name);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::CustomOperator_t>(*ASRUtils::symbol_get_past_external(sym)));
            ASR::CustomOperator_t* custom_op = ASR::down_cast<ASR::CustomOperator_t>(
                ASRUtils::symbol_get_past_external(sym));
            Vec<ASR::call_arg_t> args; args.reserve(al, 2);
            ASR::call_arg_t arg1; arg1.loc = x.base.base.loc; arg1.m_value = left;
            args.push_back(al, arg1);
            ASR::call_arg_t arg2; arg2.loc = x.base.base.loc; arg2.m_value = right;
            args.push_back(al, arg2);
            int i = ASRUtils::select_generic_procedure(args, *custom_op, x.base.base.loc,
                [&](const std::string &msg, const Location &loc) {
                        diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }, true);
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
                ASRUtils::symbol_get_past_external(custom_op->m_procs[i]));
            ASR::ttype_t* return_type = ASRUtils::get_FunctionType(func)->m_return_var_type;
            return_type = handle_return_type(return_type, x.base.base.loc, args, func);
            ASR::symbol_t* v = custom_op->m_procs[i];
            std::string func_name = ASRUtils::symbol_name(v);
            v = current_scope->resolve_symbol(func_name);
            if (v == nullptr) {
                std::string mangled_name = func_name + "@~concat";
                func_name = mangled_name;
            }
            v = current_scope->resolve_symbol(func_name);
            if( v == nullptr ) {
                diag.add(Diagnostic("'" + func_name +
                    "' not found in current scope", Level::Error, Stage::Semantic, {Label("", {v->base.loc})}));
                throw SemanticAbort();
            }
            ADD_ASR_DEPENDENCIES(current_scope, v, current_function_dependencies);
            ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
            tmp = ASRUtils::make_FunctionCall_t_util(al, x.base.base.loc, v,
                v, args.p, args.size(), return_type, nullptr, nullptr,
                false);
            tmp = ASR::make_OverloadedStringConcat_t(al, x.base.base.loc,
                left, right, return_type, nullptr, ASRUtils::EXPR(tmp));
        }
    }

    void visit_UnaryOp(const AST::UnaryOp_t &x) {
        this->visit_expr(*x.m_operand);
        ASR::expr_t *operand = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_UnaryOp(al, x, operand, tmp,
            current_scope, current_function_dependencies,
            current_module_dependencies, diag);
    }

    void visit_Compare(const AST::Compare_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_Compare(al, x, left, right, tmp,
                                            cmpop2str[x.m_op], current_scope,
                                            current_function_dependencies,
                                            current_module_dependencies,
                                            compiler_options, diag);
    }

    void visit_Parenthesis(const AST::Parenthesis_t &x) {
        this->visit_expr(*x.m_operand);
    }

    void visit_Logical(const AST::Logical_t &x) {
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
        tmp = ASR::make_LogicalConstant_t(al, x.base.base.loc, x.m_value, type);
    }

    void visit_String(const AST::String_t &x) {
        int s_len = strlen(x.m_s);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_String_t(al, x.base.base.loc, 1, 
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, s_len,
                ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4)))),
            ASR::string_length_kindType::ExpressionLength,
            ASR::string_physical_typeType::PointerString));
        tmp = ASR::make_StringConstant_t(al, x.base.base.loc, x.m_s, type);
    }

    void visit_BOZ(const AST::BOZ_t& x) {
        std::string s = std::string(x.m_s);
        int base = -1;
        ASR::integerbozType boz_type;
        if( s[0] == 'b' || s[0] == 'B' ) {
            boz_type = ASR::integerbozType::Binary;
            base = 2;
        } else if( s[0] == 'z' || s[0] == 'Z' ) {
            boz_type = ASR::integerbozType::Hex;
            base = 16;
        } else if( s[0] == 'o' || s[0] == 'O' ) {
            boz_type = ASR::integerbozType::Octal;
            base = 8;
        } else {
            diag.add(Diagnostic(R"""(Only 'b', 'o' and 'z'
                                are accepted as prefixes of
                                BOZ literal constants.)""",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        std::string boz_str = s.substr(2, s.size() - 2);
        uint64_t boz_unsigned_int = std::stoull(boz_str, nullptr, base);
        int64_t boz_int = static_cast<int64_t>(boz_unsigned_int);
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, compiler_options.po.default_integer_kind));
        tmp = ASR::make_IntegerConstant_t(al, x.base.base.loc, boz_int,
                int_type, boz_type);
    }

    void visit_Num(const AST::Num_t &x) {
        int ikind = compiler_options.po.default_integer_kind;
        if (x.m_kind) {
            ikind = std::atoi(x.m_kind);
            if (ikind == 0) {
                std::string var_name = x.m_kind;
                ASR::symbol_t *v = current_scope->resolve_symbol(to_lower(var_name));
                if (v) {
                    const ASR::symbol_t *v3 = ASRUtils::symbol_get_past_external(v);
                    if (ASR::is_a<ASR::Variable_t>(*v3)) {
                        ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v3);
                        if (v2->m_value) {
                            if (ASR::is_a<ASR::IntegerConstant_t>(*v2->m_value)) {
                                ikind = ASR::down_cast<ASR::IntegerConstant_t>(v2->m_value)->m_n;
                            } else {
                                diag.add(Diagnostic("Variable '" + var_name + "' is constant but not an integer",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                                throw SemanticAbort();
                            }
                        } else {
                            diag.add(Diagnostic("Variable '" + var_name + "' is not constant",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                            throw SemanticAbort();
                        }
                    } else {
                        diag.add(Diagnostic("Symbol '" + var_name + "' is not a variable",
                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                        throw SemanticAbort();
                    }
                } else {
                    diag.add(Diagnostic("Variable '" + var_name + "' not declared",
                        Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            }
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al,
                x.base.base.loc, ikind));
        if (BigInt::is_int_ptr(x.m_n)) {
            std::string str_repr = BigInt::largeint_to_string(x.m_n);
            if( !BigInt::is_int64(str_repr) ) {
                diag.add(Diagnostic("Integer constants larger than 2^64-1 are not implemented yet",
                                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
            int64_t m_n = std::stoll(str_repr);
            tmp = ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                                m_n, type);
        } else {
            tmp = ASR::make_IntegerConstant_t(al, x.base.base.loc, x.m_n, type);
        }
    }


    void visit_Real(const AST::Real_t &x) {
        // First determine the kind into r_kind (e.g., 4 or 8)
        char* s_kind;
        int r_kind = ASRUtils::extract_kind_str(x.m_n, s_kind);
        if (r_kind == 0) {
            std::string var_name = to_lower(s_kind);
            ASR::symbol_t *v = current_scope->resolve_symbol(var_name);
            if (v) {
                const ASR::symbol_t *v3 = ASRUtils::symbol_get_past_external(v);
                if (ASR::is_a<ASR::Variable_t>(*v3)) {
                    ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v3);
                    if (v2->m_value) {
                        if (ASR::is_a<ASR::IntegerConstant_t>(*v2->m_value)) {
                            r_kind = ASR::down_cast<ASR::IntegerConstant_t>(v2->m_value)->m_n;
                        } else {
                            diag.add(Diagnostic("Variable '" + var_name + "' is constant but not an integer",
                                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                            throw SemanticAbort();
                        }
                    } else {
                        diag.add(Diagnostic("Variable '" + var_name + "' is not constant",
                            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                        throw SemanticAbort();
                    }
                } else {
                    diag.add(Diagnostic("Symbol '" + var_name + "' is not a variable",
                        Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
                }
            } else {
                diag.add(Diagnostic("Variable '" + var_name + "' not declared",
                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
        }

        // Now extract the number into this kind correctly
        double r = -1;
        if ( r_kind == 4 ) {
            r = ASRUtils::extract_real_4(x.m_n);
        } else if ( r_kind == 8 ) {
            r = ASRUtils::extract_real_8(x.m_n);
        } else {
            diag.add(Diagnostic("Kind not supported",
                Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc, r_kind));
        tmp = ASR::make_RealConstant_t(al, x.base.base.loc, r, type);
    }

    void visit_Complex(const AST::Complex_t &x) {
        this->visit_expr(*x.m_re);
        ASR::expr_t *re = ASRUtils::EXPR(tmp);
        ASR::expr_t *re_value = ASRUtils::expr_value(re);
        int a_kind_r = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(re));
        this->visit_expr(*x.m_im);
        ASR::expr_t *im = ASRUtils::EXPR(tmp);
        ASR::expr_t *im_value = ASRUtils::expr_value(im);
        int a_kind_i = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(im));
        // TODO: Add semantic checks what type are allowed
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc,
                std::max(a_kind_r, a_kind_i)));
        ASR::expr_t *value = nullptr;
        if (re_value && im_value) {
            double re_double;
            if (ASR::is_a<ASR::RealConstant_t>(*re_value)) {
                re_double = ASR::down_cast<ASR::RealConstant_t>(re_value)->m_r;
            } else if (ASR::is_a<ASR::IntegerConstant_t>(*re_value)) {
                re_double = ASR::down_cast<ASR::IntegerConstant_t>(re_value)->m_n;
            } else {
                diag.add(Diagnostic("Argument `a` in a ComplexConstructor `(a,b)` must be either Real or Integer",
                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
            double im_double;
            if (ASR::is_a<ASR::RealConstant_t>(*im_value)) {
                im_double = ASR::down_cast<ASR::RealConstant_t>(im_value)->m_r;
            } else if (ASR::is_a<ASR::IntegerConstant_t>(*im_value)) {
                im_double = ASR::down_cast<ASR::IntegerConstant_t>(im_value)->m_n;
            } else {
                diag.add(Diagnostic("Argument `b` in a ComplexConstructor `(a,b)` must be either Real or Integer",
                    Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
                throw SemanticAbort();
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_ComplexConstant_t(al, x.base.base.loc, re_double, im_double, type));
        }
        tmp = ASR::make_ComplexConstructor_t(al, x.base.base.loc,
                re, im, type, value);
    }

    void visit_Procedure(const AST::Procedure_t&) {
        // To Be Implemented
    }

    Vec<ASR::expr_t*> visit_expr_list(AST::fnarg_t *ast_list, size_t n) {
        Vec<ASR::expr_t*> asr_list;
        asr_list.reserve(al, n);
        for (size_t i=0; i<n; i++) {
            LCOMPILERS_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            asr_list.push_back(al, expr);
        }
        return asr_list;
    }

    void visit_expr_list(AST::fnarg_t *ast_list, size_t n, Vec<ASR::call_arg_t>& call_args) {
        call_args.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            LCOMPILERS_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            ASR::call_arg_t call_arg;
            call_arg.loc = expr->base.loc;
            call_arg.m_value = expr;
            call_args.push_back(al, call_arg);
        }
    }

    std::vector<std::string> convert_fn_args_to_string(
            ASR::expr_t **expr_list, size_t n) {
        std::vector<std::string> result;
        for (size_t i=0; i < n; i++) {
            ASR::symbol_t* sym_i = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(expr_list[i])->m_v);
            if( ASR::is_a<ASR::Variable_t>(*sym_i) ) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_i);
                result.push_back(v->m_name);
            } else if( ASR::is_a<ASR::Function_t>(*sym_i) ) {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sym_i);
                result.push_back(f->m_name);
            } else {
                LCOMPILERS_ASSERT(false);
            }
        }
        return result;
    }

    template <typename T>
    void visit_kwargs(Vec<ASR::call_arg_t>& args, AST::keyword_t *kwargs, size_t n,
                ASR::expr_t **fn_args, size_t fn_n_args, const Location &loc, T* fn,
                diag::Diagnostics& diag, size_t type_bound=0, bool is_nopass = false) {
        int n_args = args.size();
        std::string fn_name = fn->m_name;
        if (is_nopass) {
            type_bound = 0;
        }
        bool is_method = (type_bound > 0);
        if (n_args + (int)n > (int)fn_n_args) {
            diag.semantic_error_label(
                "Procedure '" + fn_name + "' accepts " + std::to_string(fn_n_args)
                + " arguments, but " + std::to_string(n_args + n)
                + " were provided",
                {loc},
                "incorrect number of arguments to '" + fn_name + "'"
            );
            return;
        }

        std::vector<std::string> optional_args;
        std::vector<int> optional_args_idx;
        for( auto itr = fn->m_symtab->get_scope().begin();
             itr != fn->m_symtab->get_scope().end(); itr++ ) {
            ASR::symbol_t* fn_sym = itr->second;
            if( ASR::is_a<ASR::Variable_t>(*fn_sym) ) {
                ASR::Variable_t* fn_var = ASR::down_cast<ASR::Variable_t>(fn_sym);
                if( fn_var->m_presence == ASR::presenceType::Optional ) {
                    optional_args.push_back(itr->first);
                    for( int i = 0; i < (int)fn_n_args; i++ ) {
                        if( ASR::down_cast<ASR::Var_t>(fn_args[i])->m_v == fn_sym ) {
                            optional_args_idx.push_back(i - is_method);
                            break;
                        }
                    }
                }
            }
        }

        std::vector<std::string> fn_args2 = convert_fn_args_to_string(fn_args, fn_n_args);

        int offset = args.size();
        for (int i = 0; i < (int)fn_n_args - offset - is_method; i++) {
            ASR::call_arg_t call_arg;
            call_arg.loc = loc;
            call_arg.m_value = nullptr;
            args.push_back(al, call_arg);
        }

        for (int i = 0; i < (int)n; i++) {
            this->visit_expr(*kwargs[i].m_value);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            std::string name = to_lower(kwargs[i].m_arg);
            auto search = std::find(fn_args2.begin(), fn_args2.end(), name);
            if (search == fn_args2.end()) {
                diag.semantic_error_label(
                    "Keyword argument not found " + name,
                    {loc},
                    name + " is not a valid keyword argument.");
                return ;
            }

            int idx = std::distance(fn_args2.begin(), search) - (int)is_method;
            if (idx < n_args) {
                diag.semantic_error_label(
                    "Keyword argument '" + name + "' is already specified as a positional argument",
                    {loc},
                    "keyword argument '" + name + "' is already specified");
                return ;
            }
            if (args[idx].m_value != nullptr) {
                diag.semantic_error_label(
                    "Keyword argument " + name + " is already specified",
                    {loc},
                    name + " keyword argument is already specified.");
                return ;
            }
            args.p[idx].loc = expr->base.loc;
            args.p[idx].m_value = expr;
        }

        // Ensure required arguments are provided, but skip optional ones
        for (int i = 0; i < (int)args.size(); i++) {
            if (args[i].m_value == nullptr) {
                // Skip checking if the argument is optional
                if (std::find(optional_args_idx.begin(), optional_args_idx.end(), i) != optional_args_idx.end()) {
                    continue;
                }
                diag.semantic_error_label(
                    "Argument was not specified",
                    {loc},
                    std::to_string(i) +
                    "-th argument not specified for " + fn_name);
                return ;
            }
        }
    }

    void visit_kwargs(Vec<ASR::call_arg_t>& args, AST::keyword_t *kwargs, size_t n,
        const Location &loc, ASR::symbol_t* fn, diag::Diagnostics& diag) {
        fn = ASRUtils::symbol_get_past_external(fn);
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Struct_t>(*fn));
        std::deque<std::string> constructor_args;
        std::deque<ASR::symbol_t*> constructor_arg_syms;
        ASR::Struct_t* fn_struct_type = ASR::down_cast<ASR::Struct_t>(fn);
        while( fn_struct_type ) {
            for( int i = (int) fn_struct_type->n_members - 1; i >= 0; i-- ) {
                constructor_args.push_front(fn_struct_type->m_members[i]);
                constructor_arg_syms.push_front(
                    fn_struct_type->m_symtab->get_symbol(
                        fn_struct_type->m_members[i]));
            }
            if( fn_struct_type->m_parent != nullptr ) {
                ASR::symbol_t* fn_ = ASRUtils::symbol_get_past_external(
                                        fn_struct_type->m_parent);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Struct_t>(*fn_));
                fn_struct_type = ASR::down_cast<ASR::Struct_t>(fn_);
            } else {
                fn_struct_type = nullptr;
            }
        }

        int n_ = (int) constructor_args.size() - (int) args.size();
        for( int i = 0; i < n_; i++ ) {
            ASR::call_arg_t empty_arg;
            Location loc;
            loc.first = 1, loc.last = 1;
            empty_arg.loc = loc;
            empty_arg.m_value = nullptr;
            args.push_back(al, empty_arg);
        }

        LCOMPILERS_ASSERT(args.size() == constructor_args.size());

        for (size_t i = 0; i < n; i++) {
            this->visit_expr(*kwargs[i].m_value);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            std::string name = to_lower(kwargs[i].m_arg);
            auto search = std::find(constructor_args.begin(),
                                    constructor_args.end(), name);
            if (search == constructor_args.end()) {
                diag.semantic_error_label(
                    "Keyword argument not found",
                    {loc},
                    "'" + name + "'" + " keyword argument not found");
                throw SemanticAbort();
            }

            size_t idx = std::distance(constructor_args.begin(), search);
            if (args[idx].m_value != nullptr) {
                diag.semantic_error_label(
                    "Keyword argument is already specified",
                    {loc},
                    "'" + name + "'" + + " keyword argument is already specified");
                throw SemanticAbort();
            }
            args.p[idx].loc = expr->base.loc;
            args.p[idx].m_value = expr;
        }

        // If value is not specified in args nor in keyword argument, set to default initializer if it exists
        for( size_t i = 0; i < args.size(); i++ ) {
            if( args[i].m_value == nullptr ) {
                ASR::symbol_t* arg_sym = constructor_arg_syms[i];
                LCOMPILERS_ASSERT(arg_sym != nullptr);
                ASR::expr_t* default_init = nullptr;
                bool is_default_needed = true;
                if( ASR::is_a<ASR::Variable_t>(*arg_sym) ) {
                    ASR::Variable_t* arg_var = ASR::down_cast<ASR::Variable_t>(arg_sym);
                    default_init = arg_var->m_value;
                    if( ASRUtils::is_allocatable(arg_var->m_type) ) {
                        is_default_needed = false;
                    }
                }
                if( default_init == nullptr && is_default_needed ) {
                    diag.semantic_error_label(
                        "Argument was not specified",
                        {loc},
                        "Argument '" + constructor_args[i] + "' not specified for " + ASRUtils::symbol_name(fn));
                    throw SemanticAbort();
                }
                // Replace symbols in StructConstant to external symbols
                if (default_init && ASR::is_a<ASR::StructConstant_t>(*default_init)) {
                    ASR::StructConstant_t *st = ASR::down_cast<ASR::StructConstant_t>(default_init);
                    ASR::symbol_t *ext_sym = current_scope->resolve_symbol(ASRUtils::symbol_name(st->m_dt_sym));
                    if (ASR::is_a<ASR::ExternalSymbol_t>(*ext_sym)) {
                        ASR::ttype_t *type = ASRUtils::TYPE(ASRUtils::make_StructType_t_util(al, loc, ext_sym));
                        default_init = ASRUtils::EXPR(ASR::make_StructConstant_t(al, loc, ext_sym, st->m_args, st->n_args, type));
                    }
                }
                args.p[i].m_value = default_init;
                args.p[i].loc = arg_sym->base.loc;
            }
        }

        for (size_t i = 0; i < constructor_arg_syms.size(); i++) {
            if( args[i].m_value != nullptr ) {
                ASR::symbol_t* member_sym = constructor_arg_syms[i];
                ASR::ttype_t* member_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::symbol_type(member_sym));
                ASR::ttype_t* arg_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::expr_type(args[i].m_value));
                ImplicitCastRules::set_converted_value(al, loc,
                    &args.p[i].m_value, arg_type, member_type, diag);
            }
        }

    }

    void visit_NameUtil(AST::struct_member_t* x_m_member, size_t x_n_member,
                        char* x_m_id, const Location& loc) {
        if (x_n_member == 0) {
            ASR::expr_t* expr = ASRUtils::EXPR(resolve_variable(loc, to_lower(x_m_id)));
            if(x_m_member && x_m_member[0].m_args) {
                expr = ASRUtils::EXPR(
                    create_ArrayRef(loc, x_m_member[0].m_args, x_m_member[0].n_args,
                    nullptr, 0, nullptr,
                    ASR::down_cast<ASR::Var_t>(expr)->m_v,
                    ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expr)->m_v)));
            }
            tmp = (ASR::asr_t*) replace_with_common_block_variables(expr);
        } else if (x_n_member == 1) {
            if (x_m_member[0].n_args == 0) {
                SymbolTable* scope = current_scope;
                tmp = (ASR::asr_t*) replace_with_common_block_variables(
                    ASRUtils::EXPR(this->resolve_variable2(loc, to_lower(x_m_id),
                    to_lower(x_m_member[0].m_name), scope, nullptr, 0, x_m_member[1].m_args, x_m_member[1].n_args)));
            } else {
                // TODO: incorporate m_args
                SymbolTable* scope = current_scope;
                tmp = (ASR::asr_t*) replace_with_common_block_variables(
                    ASRUtils::EXPR(this->resolve_variable2(loc, to_lower(x_m_id),
                    to_lower(x_m_member[0].m_name), scope, x_m_member->m_args, x_m_member->n_args)));
            }
        } else {
            SymbolTable* scope = current_scope;
            tmp = (ASR::asr_t*) replace_with_common_block_variables(
                    ASRUtils::EXPR(this->resolve_variable2(loc,
                    to_lower(x_m_member[1].m_name), to_lower(x_m_member[0].m_name), scope,
                    x_m_member[0].m_args, x_m_member[0].n_args,
                    x_m_member[1].m_args, x_m_member[1].n_args)));
            bool is_tmp_array = ASRUtils::is_array(
                ASRUtils::expr_type(ASRUtils::EXPR(tmp)));
            ASR::StructInstanceMember_t* tmp2;
            std::uint32_t i;
            for( i = 2; i < x_n_member; i++ ) {
                tmp2 = (ASR::StructInstanceMember_t*) this->resolve_variable2(loc,
                        to_lower(x_m_member[i].m_name), to_lower(x_m_member[i - 1].m_name),
                        scope);
                ASR::ttype_t* tmp2_mem_type = tmp2->m_type;
                ASR::symbol_t* tmp2_m_m_ext = ASRUtils::import_struct_instance_member(al,
                                                    tmp2->m_m, current_scope, tmp2_mem_type);
                if( is_tmp_array ) {
                    ASR::dimension_t* m_dims = nullptr;
                    int n_dims = ASRUtils::extract_dimensions_from_ttype(
                        ASRUtils::expr_type(ASRUtils::EXPR(tmp)), m_dims);
                    Vec<ASR::dimension_t> m_dims_vec;
                    m_dims_vec.from_pointer_n(m_dims, n_dims);
                    tmp2_mem_type = ASRUtils::duplicate_type(al, tmp2_mem_type, &m_dims_vec);
                }
                tmp = ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(tmp),
                    tmp2_m_m_ext, ASRUtils::fix_scoped_type(al, tmp2_mem_type, current_scope), nullptr);
                make_ArrayItem_from_struct_m_args(x_m_member[i].m_args, x_m_member[i].n_args,
                    ASRUtils::EXPR(tmp), tmp, loc);
                if( ASR::is_a<ASR::ArraySection_t>(*ASRUtils::EXPR(tmp)) ) {
                    if( is_tmp_array ) {
                        diag.add(Diagnostic(
                            "The expression with derived types contains two or more arrays.", Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    is_tmp_array = true;
                }
            }
            i = x_n_member - 1;
            tmp2 = (ASR::StructInstanceMember_t*) this->resolve_variable2(loc, to_lower(x_m_id),
                        to_lower(x_m_member[i].m_name), scope);
            ASR::ttype_t* tmp2_mem_type = tmp2->m_type;
            ASR::symbol_t* tmp2_m_m_ext = ASRUtils::import_struct_instance_member(al, tmp2->m_m,
                                            current_scope, tmp2_mem_type);
            if( is_tmp_array ) {
                if( ASRUtils::is_array(tmp2_mem_type) ) {
                    diag.add(Diagnostic(
                        "The expression with derived types contains two or more arrays.",
                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
                ASR::dimension_t* m_dims = nullptr;
                int n_dims = ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::expr_type(ASRUtils::EXPR(tmp)), m_dims);
                Vec<ASR::dimension_t> m_dims_vec;
                m_dims_vec.from_pointer_n(m_dims, n_dims);
                tmp2_mem_type = ASRUtils::duplicate_type(al, tmp2_mem_type, &m_dims_vec);
            }
            tmp = ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(tmp), tmp2_m_m_ext,
                ASRUtils::fix_scoped_type(al, tmp2_mem_type, current_scope), nullptr);
        }
        // Find array in the returning tmp expression. If found set tmp type to that array type.
        bool array_found = false;
        ASR::ttype_t* array_type = nullptr; // will be set if only one single array is found. It'd be used to change the type of tmp.
        ASR::asr_t* tmp_copy = tmp;
        while(ASR::is_a<ASR::StructInstanceMember_t>(*ASRUtils::EXPR(tmp_copy)) ||
            (ASR::is_a<ASR::ArrayItem_t>(*ASRUtils::EXPR(tmp_copy)) &&
             ASR::is_a<ASR::StructInstanceMember_t>(*(ASR::down_cast<ASR::ArrayItem_t>(ASRUtils::EXPR(tmp_copy)))->m_v))){
            ASR::StructInstanceMember_t* tmp2 = nullptr;
            bool check_m_m = true;
            if(ASR::is_a<ASR::ArrayItem_t>(*ASRUtils::EXPR(tmp_copy))){
                tmp2 = ASR::down_cast<ASR::StructInstanceMember_t>(ASR::down_cast<ASR::ArrayItem_t>(ASRUtils::EXPR(tmp_copy))->m_v);
                check_m_m = false;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*ASRUtils::EXPR(tmp_copy))) {
                tmp2 = ASR::down_cast<ASR::StructInstanceMember_t>(ASRUtils::EXPR(tmp_copy));
            }

            if(check_m_m){
                ASR::ExternalSymbol_t* tmp2_m_m_ext = ASR::down_cast<ASR::ExternalSymbol_t>(tmp2->m_m);
                if(ASR::is_a<ASR::Variable_t>(*(tmp2_m_m_ext->m_external)) &&
                    ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_allocatable(ASRUtils::symbol_type(tmp2_m_m_ext->m_external)))){
                    if(array_found){
                        diag.add(Diagnostic(
                            "The expression with derived types contains two or more arrays.",
                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    array_found = true;
                    array_type = ASRUtils::duplicate_type(al,ASRUtils::symbol_type(tmp2->m_m));
                }
            }
            if(tmp2->m_v->type == ASR::exprType::Var){
                ASR::ttype_t* var_type = ASRUtils::expr_type(tmp2->m_v);
                if(ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_allocatable(var_type))){
                    if(array_found){
                        diag.add(Diagnostic(
                            "The expression with derived types contains two or more arrays.",
                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                    array_found = true;
                    array_type = ASRUtils::duplicate_type(al,var_type);
                }
            } else if ( tmp2->m_v->type == ASR::exprType::ArraySection ) {
                ASR::ArraySection_t* array_section = ASR::down_cast<ASR::ArraySection_t>(tmp2->m_v);

                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);

                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
                dim.m_length = ASRUtils::compute_length_from_start_end(al, array_section->m_args->m_left, array_section->m_args->m_right);
                dims.push_back(al, dim);

                array_found = true;
                array_type = ASRUtils::TYPE(ASR::make_Array_t(
                    al, array_section->base.base.loc, tmp2->m_type, dims.p, dims.size(), ASR::array_physical_typeType::FixedSizeArray));
            }
            tmp_copy = (ASR::asr_t*)(tmp2->m_v);
        }
        if(array_type){
            if(ASR::is_a<ASR::StructInstanceMember_t>(*ASRUtils::EXPR(tmp))){
                ASR::StructInstanceMember_t* tmp2 = ASR::down_cast<ASR::StructInstanceMember_t>(ASRUtils::EXPR(tmp));
                if(ASR::is_a<ASR::Array_t>(*array_type)){
                    (ASR::down_cast<ASR::Array_t>(array_type))->m_type = ASRUtils::type_get_past_array(tmp2->m_type);
                    tmp2->m_type = array_type;
                }
                if(ASR::is_a<ASR::Allocatable_t>(*array_type)){
                    ASR::down_cast<ASR::Array_t>((ASR::down_cast<ASR::Allocatable_t>(array_type))->m_type)->m_type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(tmp2->m_type));
                    if (ASRUtils::is_allocatable(ASRUtils::EXPR(tmp))) {
                        tmp2->m_type = array_type;
                    } else {
                        tmp2->m_type = ASRUtils::type_get_past_allocatable(array_type);
                    }
                }

            } else if (ASR::is_a<ASR::ArrayItem_t>(*ASRUtils::EXPR(tmp))) {
                ASR::ArrayItem_t* tmp2 = ASR::down_cast<ASR::ArrayItem_t>(ASRUtils::EXPR(tmp));
                if(ASR::is_a<ASR::Array_t>(*array_type)){
                    (ASR::down_cast<ASR::Array_t>(array_type))->m_type = ASRUtils::type_get_past_array(tmp2->m_type);
                    tmp2->m_type = array_type;
                }
                if(ASR::is_a<ASR::Allocatable_t>(*array_type)){
                    ASR::down_cast<ASR::Array_t>((ASR::down_cast<ASR::Allocatable_t>(array_type))->m_type)->m_type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(tmp2->m_type));
                    tmp2->m_type = array_type;
                }
            }
        }
    }

    void visit_Name(const AST::Name_t &x) {
        visit_NameUtil(x.m_member, x.n_member, x.m_id, x.base.base.loc);
    }

    void determine_char_len(const AST::kind_item_t& item, std::string& sym, ASR::String_t* str) {
        switch (item.m_type) {
            case (AST::kind_item_typeType::Value) : {
                LCOMPILERS_ASSERT(item.m_value != nullptr);
                if(is_funcCall_to_unresolved_genereicProcedure(item.m_value)){ // Postpone the evaluation
                    postponed_genericProcedure_calls_vec.emplace_back(&str->m_len, current_scope,
                    item.m_value, s2c(al, sym), [](ASR::expr_t* x){(void)x;});
                } else { // Evaluate normally
                    _processing_char_len = true;
                    this->visit_expr(*item.m_value);
                    ASR::expr_t* len_expr = ASRUtils::EXPR(tmp);
                    str->m_len = ASRUtils::is_const(len_expr) ? ASRUtils::expr_value(len_expr) : len_expr;
                    _processing_char_len = false;
                }
                str->m_len_kind = ASR::string_length_kindType::ExpressionLength;
                break;
            }
            case (AST::kind_item_typeType::Star) : {
                LCOMPILERS_ASSERT(item.m_value == nullptr);
                str->m_len = nullptr; // If it's parameter variable, len will be set later.
                str->m_len_kind = ASR::string_length_kindType::AssumedLength;
                break;
            }
            case (AST::kind_item_typeType::Colon) : {
                LCOMPILERS_ASSERT(item.m_value == nullptr);
                str->m_len = nullptr;
                str->m_len_kind = ASR::string_length_kindType::DeferredLength;
                break;
            }
            default :{
                throw LCompilersException("Character's len Not identified");
            }
        }
    }
};

} // namespace LCompilers::LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H */
