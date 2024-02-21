#ifndef LIBASR_BUILDER_H
#define LIBASR_BUILDER_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers::ASRUtils {

class ASRBuilder {
    private:

    Allocator& al;
    // TODO: use the location to point C++ code in `intrinsic_function_registry`
    const Location &loc;

    public:

    ASRBuilder(Allocator& al_, const Location& loc_): al(al_), loc(loc_) {}

    #define make_ConstantWithKind(Constructor, TypeConstructor, value, kind, loc) ASRUtils::EXPR( \
        ASR::Constructor( al, loc, value, \
            ASRUtils::TYPE(ASR::TypeConstructor(al, loc, kind)))) \

    #define make_ConstantWithType(Constructor, value, type, loc) ASRUtils::EXPR( \
        ASR::Constructor(al, loc, value, type)) \

    #define declare_basic_variables(name)                                       \
        std::string fn_name = scope->get_unique_name(name, false);              \
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);               \
        ASRBuilder b(al, loc);                                                  \
        Vec<ASR::expr_t*> args; args.reserve(al, 1);                            \
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);                            \
        SetChar dep; dep.reserve(al, 1);

    // Symbols -----------------------------------------------------------------
    ASR::expr_t *Variable(SymbolTable *symtab, std::string var_name,
            ASR::ttype_t *type, ASR::intentType intent,
            ASR::abiType abi=ASR::abiType::Source, bool a_value_attr=false) {
        ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(
            ASR::make_Variable_t(al, loc, symtab, s2c(al, var_name), nullptr, 0,
            intent, nullptr, nullptr, ASR::storage_typeType::Default, type, nullptr, abi,
            ASR::Public, ASR::presenceType::Required, a_value_attr));
        symtab->add_symbol(s2c(al, var_name), sym);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }

    #define declare(var_name, type, intent)                                     \
        b.Variable(fn_symtab, var_name, type, ASR::intentType::intent)

    #define fill_func_arg(arg_name, type) {                                     \
        auto arg = declare(arg_name, type, In);                                 \
        args.push_back(al, arg); }

    #define make_ASR_Function_t(name, symtab, dep, args, body, return_var, abi,     \
            deftype, bindc_name)                                                \
        ASR::down_cast<ASR::symbol_t>( ASRUtils::make_Function_t_util(al, loc,  \
        symtab, s2c(al, name), dep.p, dep.n, args.p, args.n, body.p, body.n,    \
        return_var, abi, ASR::accessType::Public,                 \
        deftype, bindc_name, false, false, false, false,      \
        false, nullptr, 0, false, false, false));

    #define make_Function_Without_ReturnVar_t(name, symtab, dep, args, body,    \
            abi, deftype, bindc_name)                                           \
        ASR::down_cast<ASR::symbol_t>( ASRUtils::make_Function_t_util(al, loc,  \
        symtab, s2c(al, name), dep.p, dep.n, args.p, args.n, body.p, body.n,    \
        nullptr, abi, ASR::accessType::Public,                    \
        deftype, bindc_name, false, false, false, false,      \
        false, nullptr, 0, false, false, false));

    // Types -------------------------------------------------------------------
    #define int8         TYPE(ASR::make_Integer_t(al, loc, 1))
    #define int16        TYPE(ASR::make_Integer_t(al, loc, 2))
    #define int32        TYPE(ASR::make_Integer_t(al, loc, 4))
    #define int64        TYPE(ASR::make_Integer_t(al, loc, 8))
    #define real32       TYPE(ASR::make_Real_t(al, loc, 4))
    #define real64       TYPE(ASR::make_Real_t(al, loc, 8))
    #define complex32    TYPE(ASR::make_Complex_t(al, loc, 4))
    #define logical      TYPE(ASR::make_Logical_t(al, loc, 4))
    #define character(x) TYPE(ASR::make_Character_t(al, loc, 1, x, nullptr))
    #define List(x)      TYPE(ASR::make_List_t(al, loc, x))

    ASR::ttype_t *Tuple(std::vector<ASR::ttype_t*> tuple_type) {
        Vec<ASR::ttype_t*> m_tuple_type; m_tuple_type.reserve(al, 3);
        for (auto &x: tuple_type) {
            m_tuple_type.push_back(al, x);
        }
        return TYPE(ASR::make_Tuple_t(al, loc, m_tuple_type.p, m_tuple_type.n));
    }
    ASR::ttype_t *Array(std::vector<int64_t> dims, ASR::ttype_t *type) {
        Vec<ASR::dimension_t> m_dims; m_dims.reserve(al, 1);
        for (auto &x: dims) {
            ASR::dimension_t dim;
            dim.loc = loc;
            if (x == -1) {
                dim.m_start = nullptr;
                dim.m_length = nullptr;
            } else {
                dim.m_start = EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int32));
                dim.m_length = EXPR(ASR::make_IntegerConstant_t(al, loc, x, int32));
            }
            m_dims.push_back(al, dim);
        }
        return make_Array_t_util(al, loc, type, m_dims.p, m_dims.n);
    }

    // Expressions -------------------------------------------------------------
    #define i(x, t)   ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, x, t))
    #define i32(x)   ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, x, int32))
    #define i32_n(x) ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc, i32(abs(x)),   \
        int32, i32(x)))
    #define i32_neg(x, t) ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc, x, t, nullptr))

    #define f(x, t)   ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, x, t))
    #define f32(x) ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, x, real32))
    #define f32_neg(x, t) ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc, x, t, nullptr))

    #define bool32(x)  ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, x, logical))
    #define c32(x, y) ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, loc, x, y, complex32))

    #define ListItem(x, pos, type) EXPR(ASR::make_ListItem_t(al, loc, x, pos,   \
        type, nullptr))
    #define ListAppend(x, val) STMT(ASR::make_ListAppend_t(al, loc, x, val))

    #define StringSection(s, start, end) EXPR(ASR::make_StringSection_t(al, loc,\
        s, start, end, nullptr, character(-2), nullptr))
    #define StringItem(x, idx) EXPR(ASR::make_StringItem_t(al, loc, x, idx,     \
        character(-2), nullptr))
    #define StringConstant(s, type) EXPR(ASR::make_StringConstant_t(al, loc,    \
        s2c(al, s), type))
    #define StringLen(s) EXPR(ASR::make_StringLen_t(al, loc, s, int32, nullptr))

    // Cast --------------------------------------------------------------------
    #define r2i8(x) EXPR(ASR::make_Cast_t(al, loc, x,                           \
        ASR::cast_kindType::RealToInteger, int8, nullptr))
    #define r2i16(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::RealToInteger, int16, nullptr))
    #define r2i32(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::RealToInteger, int32, nullptr))
    #define r2i64(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::RealToInteger, int64, nullptr))
    #define i2r32(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::IntegerToReal, real32, nullptr))
    #define i2r64(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::IntegerToReal, real64, nullptr))
    #define i2i(x, t) EXPR(ASR::make_Cast_t(al, loc, x,                         \
        ASR::cast_kindType::IntegerToInteger, t, nullptr))
    #define i2i64(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::IntegerToInteger, int64, nullptr))
    #define i2i32(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::IntegerToInteger, int32, nullptr))
    #define r2r32(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::RealToReal, real32, nullptr))
    #define r2r64(x) EXPR(ASR::make_Cast_t(al, loc, x,                          \
        ASR::cast_kindType::RealToReal, real64, nullptr))
    #define r2r(x, t) EXPR(ASR::make_Cast_t(al, loc, x,                         \
        ASR::cast_kindType::RealToReal, t, nullptr))
    #define i2r(x, t) EXPR(ASR::make_Cast_t(al, loc, x,                         \
        ASR::cast_kindType::IntegerToReal, t, nullptr))

    // Binop -------------------------------------------------------------------
    #define iAdd(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,      \
            ASR::binopType::Add, right, int32, nullptr))
    #define i64Add(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,    \
            ASR::binopType::Add, right, int64, nullptr))
    #define rAdd(left, right, t) EXPR(ASR::make_RealBinOp_t(al, loc, left,      \
            ASR::binopType::Add, right, t, nullptr))
    #define r32Add(left, right) EXPR(ASR::make_RealBinOp_t(al, loc, left,       \
            ASR::binopType::Add, right, real32, nullptr))
    #define r64Add(left, right) EXPR(ASR::make_RealBinOp_t(al, loc, left,       \
            ASR::binopType::Add, right, real64, nullptr))
    #define i_tAdd(left, right, t) EXPR(ASR::make_IntegerBinOp_t(al, loc, left, \
            ASR::binopType::Add, right, t, nullptr))

    #define iSub(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,      \
            ASR::binopType::Sub, right, ASRUtils::int32, nullptr))
    #define i_vSub(left, right, value) ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, left,      \
            ASR::binopType::Sub, right, ASRUtils::int32, value))
    #define i8Sub(left, right) ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, left,     \
        ASR::binopType::Sub, right, int8, nullptr))
    #define i16Sub(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,    \
        ASR::binopType::Sub, right, int16, nullptr))
    #define i64Sub(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,    \
        ASR::binopType::Sub, right, int64, nullptr))
    #define rSub(left, right, t) EXPR(ASR::make_RealBinOp_t(al, loc, left,      \
            ASR::binopType::Sub, right, t, nullptr))
    #define r32Sub(left, right) EXPR(ASR::make_RealBinOp_t(al, loc, left,       \
            ASR::binopType::Sub, right, real32, nullptr))
    #define r64Sub(left, right) EXPR(ASR::make_RealBinOp_t(al, loc, left,       \
            ASR::binopType::Sub, right, real64, nullptr))
    #define i_tSub(left, right, t) EXPR(ASR::make_IntegerBinOp_t(al, loc, left, \
            ASR::binopType::Sub, right, t, nullptr))
    #define r_tSub(left, right, t) EXPR(ASR::make_RealBinOp_t(al, loc, left, \
            ASR::binopType::Sub, right, t, nullptr))

    #define iDiv(left, right) r2i32(EXPR(ASR::make_RealBinOp_t(al, loc,         \
            i2r32(left), ASR::binopType::Div, i2r32(right), real32, nullptr)))
    #define i8Div(left, right) r2i8(EXPR(ASR::make_RealBinOp_t(al, loc,         \
            i2r32(left), ASR::binopType::Div, i2r32(right), real32, nullptr)))
    #define i16Div(left, right) r2i16(EXPR(ASR::make_RealBinOp_t(al, loc,       \
            i2r32(left), ASR::binopType::Div, i2r32(right), real32, nullptr)))
    #define i64Div(left, right) r2i64(EXPR(ASR::make_RealBinOp_t(al, loc,       \
            i2r64(left), ASR::binopType::Div, i2r64(right), real32, nullptr)))
    #define r32Div(left, right) EXPR(ASR::make_RealBinOp_t(al, loc,             \
            left, ASR::binopType::Div, right, real32, nullptr))
    #define r64Div(left, right) EXPR(ASR::make_RealBinOp_t(al, loc,             \
            left, ASR::binopType::Div, right, real64, nullptr))
    #define i_tDiv(left, right, t) EXPR(ASR::make_IntegerBinOp_t(al, loc,       \
            left, ASR::binopType::Div, right, t, nullptr))

    #define iMul(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,      \
            ASR::binopType::Mul, right, int32, nullptr))
    #define i8Mul(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,     \
            ASR::binopType::Mul, right, int8, nullptr))
    #define i16Mul(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,    \
            ASR::binopType::Mul, right, int16, nullptr))
    #define i64Mul(left, right) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,    \
            ASR::binopType::Mul, right, int64, nullptr))
    #define r32Mul(left, right) EXPR(ASR::make_RealBinOp_t(al, loc, left,       \
            ASR::binopType::Mul, right, real32, nullptr))
    #define r64Mul(left, right) EXPR(ASR::make_RealBinOp_t(al, loc, left,       \
            ASR::binopType::Mul, right, real64, nullptr))
    #define i_tMul(left, right, t) EXPR(ASR::make_IntegerBinOp_t(al, loc, left, \
            ASR::binopType::Mul, right, t, nullptr))
    #define r_tMul(left, right, t) EXPR(ASR::make_RealBinOp_t(al, loc, left, \
        ASR::binopType::Mul, right, t, nullptr))

    #define iPow(left, right, t) EXPR(ASR::make_IntegerBinOp_t(al, loc, left,   \
            ASR::binopType::Pow, right, t, nullptr))
    #define And(x, y) EXPR(ASR::make_LogicalBinOp_t(al, loc, x,                 \
            ASR::logicalbinopType::And, y, logical, nullptr))
    #define Or(x, y) EXPR(ASR::make_LogicalBinOp_t(al, loc, x,                 \
            ASR::logicalbinopType::Or, y, logical, nullptr))
    #define Not(x) EXPR(ASR::make_LogicalNot_t(al, loc, x, logical, nullptr))

    #define i_BitRshift(n, bits, t) EXPR(ASR::make_IntegerBinOp_t(al, loc,      \
            n, ASR::binopType::BitRShift, bits, t, nullptr))
    #define i_BitLshift(n, bits, t) EXPR(ASR::make_IntegerBinOp_t(al, loc,      \
            n, ASR::binopType::BitLShift, bits, t, nullptr))

    ASR::expr_t *Add(ASR::expr_t *left, ASR::expr_t *right) {
        LCOMPILERS_ASSERT(check_equal_type(expr_type(left), expr_type(right)));
        ASR::ttype_t *type = expr_type(left);
        ASRUtils::make_ArrayBroadcast_t_util(al, loc, left, right);
        switch (type->type) {
            case ASR::ttypeType::Integer : {
                return EXPR(ASR::make_IntegerBinOp_t(al, loc, left,
                    ASR::binopType::Add, right, type, nullptr));
                break;
            }
            case ASR::ttypeType::Real : {
                return EXPR(ASR::make_RealBinOp_t(al, loc, left,
                    ASR::binopType::Add, right, type, nullptr));
                break;
            }
            case ASR::ttypeType::Character : {
                return EXPR(ASR::make_StringConcat_t(al, loc, left,
                    right, type, nullptr));
                break;
            }
            case ASR::ttypeType::Complex : {
                return EXPR(ASR::make_ComplexBinOp_t(al, loc, left,
                    ASR::binopType::Add, right, type, nullptr));
            }
            default: {
                LCOMPILERS_ASSERT(false);
                return nullptr;
            }
        }
    }

    ASR::expr_t *Mul(ASR::expr_t *left, ASR::expr_t *right) {
        LCOMPILERS_ASSERT(check_equal_type(expr_type(left), expr_type(right)));
        ASR::ttype_t *type = expr_type(left);
        ASRUtils::make_ArrayBroadcast_t_util(al, loc, left, right);
        switch (type->type) {
            case ASR::ttypeType::Integer : {
                return EXPR(ASR::make_IntegerBinOp_t(al, loc, left,
                    ASR::binopType::Mul, right, type, nullptr));
                break;
            }
            case ASR::ttypeType::Real : {
                return EXPR(ASR::make_RealBinOp_t(al, loc, left,
                    ASR::binopType::Mul, right, type, nullptr));
                break;
            }
            case ASR::ttypeType::Complex : {
                return EXPR(ASR::make_ComplexBinOp_t(al, loc, left,
                    ASR::binopType::Mul, right, type, nullptr));
            }
            default: {
                LCOMPILERS_ASSERT(false);
                return nullptr;
            }
        }
    }

    // Compare -----------------------------------------------------------------
    #define iEq(x, y) EXPR(ASR::make_IntegerCompare_t(al, loc, x,               \
        ASR::cmpopType::Eq, y, logical, nullptr))
    #define iNotEq(x, y) EXPR(ASR::make_IntegerCompare_t(al, loc, x,            \
        ASR::cmpopType::NotEq, y, logical, nullptr))
    #define iLt(x, y) EXPR(ASR::make_IntegerCompare_t(al, loc, x,               \
        ASR::cmpopType::Lt, y, logical, nullptr))
    #define iLtE(x, y) EXPR(ASR::make_IntegerCompare_t(al, loc, x,              \
        ASR::cmpopType::LtE, y, logical, nullptr))
    #define iGtE(x, y) EXPR(ASR::make_IntegerCompare_t(al, loc, x,              \
        ASR::cmpopType::GtE, y, logical, nullptr))
    #define iGt(x, y) EXPR(ASR::make_IntegerCompare_t(al, loc, x,               \
        ASR::cmpopType::Gt, y, logical, nullptr))

    #define ArraySize_1(x, dim) EXPR(make_ArraySize_t_util(al, loc, x, dim,     \
        int32, nullptr))
    #define ArraySize_2(x, dim, t) EXPR(make_ArraySize_t_util(al, loc, x, dim,  \
        t, nullptr))

    #define fEq(x, y) EXPR(ASR::make_RealCompare_t(al, loc, x,               \
        ASR::cmpopType::Eq, y, logical, nullptr))
    #define fGtE(x, y) EXPR(ASR::make_RealCompare_t(al, loc, x,                 \
        ASR::cmpopType::GtE, y, logical, nullptr))
    #define fLtE(x, y) EXPR(ASR::make_RealCompare_t(al, loc, x,                  \
        ASR::cmpopType::LtE, y, logical, nullptr))
    #define fLt(x, y) EXPR(ASR::make_RealCompare_t(al, loc, x,                  \
        ASR::cmpopType::Lt, y, logical, nullptr))
    #define fGt(x, y) EXPR(ASR::make_RealCompare_t(al, loc, x,                  \
        ASR::cmpopType::Gt, y, logical, nullptr))
    #define fNotEq(x, y) EXPR(ASR::make_RealCompare_t(al, loc, x,                 \
        ASR::cmpopType::NotEq, y, logical, nullptr))

    #define sEq(x, y) EXPR(ASR::make_StringCompare_t(al, loc, x,                \
        ASR::cmpopType::Eq, y, logical, nullptr))
    #define sNotEq(x, y) EXPR(ASR::make_StringCompare_t(al, loc, x,             \
        ASR::cmpopType::NotEq, y, logical, nullptr))
    #define sLt(x, y) EXPR(ASR::make_StringCompare_t(al, loc, x,             \
        ASR::cmpopType::Lt, y, logical, nullptr))

    ASR::expr_t *Gt(ASR::expr_t *left, ASR::expr_t *right) {
        LCOMPILERS_ASSERT(check_equal_type(expr_type(left), expr_type(right)));
        if (is_real(*expr_type(left))) {
            return fGt(left, right);
        } else if (is_integer(*expr_type(left))) {
            return iGt(left, right);
        } else {
            LCOMPILERS_ASSERT(false);
            return nullptr;
        }
    }

    ASR::expr_t *Lt(ASR::expr_t *left, ASR::expr_t *right) {
        LCOMPILERS_ASSERT(check_equal_type(expr_type(left), expr_type(right)));
        if (is_real(*expr_type(left))) {
            return fLt(left, right);
        } else if (is_integer(*expr_type(left))) {
            return iLt(left, right);
        } else {
            LCOMPILERS_ASSERT(false);
            return nullptr;
        }
    }

    ASR::stmt_t *If(ASR::expr_t *a_test, std::vector<ASR::stmt_t*> if_body,
            std::vector<ASR::stmt_t*> else_body) {
        Vec<ASR::stmt_t*> m_if_body; m_if_body.reserve(al, 1);
        for (auto &x: if_body) m_if_body.push_back(al, x);

        Vec<ASR::stmt_t*> m_else_body; m_else_body.reserve(al, 1);
        for (auto &x: else_body) m_else_body.push_back(al, x);

        return STMT(ASR::make_If_t(al, loc, a_test, m_if_body.p, m_if_body.n,
            m_else_body.p, m_else_body.n));
    }

    ASR::stmt_t *While(ASR::expr_t *a_test, std::vector<ASR::stmt_t*> body) {
        Vec<ASR::stmt_t*> m_body; m_body.reserve(al, 1);
        for (auto &x: body) m_body.push_back(al, x);

        return STMT(ASR::make_WhileLoop_t(al, loc, nullptr, a_test,
            m_body.p, m_body.n));
    }

    ASR::expr_t *TupleConstant(std::vector<ASR::expr_t*> ele, ASR::ttype_t *type) {
        Vec<ASR::expr_t*> m_ele; m_ele.reserve(al, 3);
        for (auto &x: ele) m_ele.push_back(al, x);
        return EXPR(ASR::make_TupleConstant_t(al, loc, m_ele.p, m_ele.n, type));
    }

    #define make_Compare(Constructor, left, op, right) ASRUtils::EXPR(ASR::Constructor( \
        al, loc, left, ASR::cmpopType::op, right, \
        ASRUtils::TYPE(ASR::make_Logical_t( \
            al, loc, 4)), nullptr)); \

    #define create_ElementalBinOp(OpType, BinOpName, OpName, value) case ASR::ttypeType::OpType: { \
        return ASRUtils::EXPR(ASR::BinOpName(al, loc, \
                left, ASR::binopType::OpName, right, \
                ASRUtils::expr_type(left), value)); \
    } \

    ASR::expr_t* ElementalAdd(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        ASR::ttype_t *left_type = ASRUtils::expr_type(left);
        left_type = ASRUtils::type_get_past_pointer(left_type);
        switch (left_type->type) {
            create_ElementalBinOp(Real, make_RealBinOp_t, Add, value)
            create_ElementalBinOp(Integer, make_IntegerBinOp_t, Add, value)
            create_ElementalBinOp(Complex, make_ComplexBinOp_t, Add, value)
            default: {
                throw LCompilersException("Expression type, " +
                                          std::to_string(left_type->type) +
                                          " not yet supported");
            }
        }
    }

    ASR::expr_t* ElementalSub(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        switch (ASRUtils::expr_type(left)->type) {
            create_ElementalBinOp(Real, make_RealBinOp_t, Sub, value)
            create_ElementalBinOp(Integer, make_IntegerBinOp_t, Sub, value)
            create_ElementalBinOp(Complex, make_ComplexBinOp_t, Sub, value)
            default: {
                throw LCompilersException("Expression type, " +
                                          std::to_string(expr_type(left)->type) +
                                          " not yet supported");
            }
        }
    }

    ASR::expr_t* ElementalDiv(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        switch (ASRUtils::expr_type(left)->type) {
            create_ElementalBinOp(Real, make_RealBinOp_t, Div, value)
            create_ElementalBinOp(Integer, make_IntegerBinOp_t, Div, value)
            create_ElementalBinOp(Complex, make_ComplexBinOp_t, Div, value)
            default: {
                throw LCompilersException("Expression type, " +
                                          std::to_string(expr_type(left)->type) +
                                          " not yet supported");
            }
        }
    }

    ASR::expr_t* ElementalMul(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        switch (ASRUtils::expr_type(left)->type) {
            create_ElementalBinOp(Real, make_RealBinOp_t, Mul, value)
            create_ElementalBinOp(Integer, make_IntegerBinOp_t, Mul, value)
            create_ElementalBinOp(Complex, make_ComplexBinOp_t, Mul, value)
            default: {
                throw LCompilersException("Expression type, " +
                                          std::to_string(expr_type(left)->type) +
                                          " not yet supported");
            }
        }
    }

    ASR::expr_t* ElementalPow(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        switch (ASRUtils::expr_type(left)->type) {
            create_ElementalBinOp(Real, make_RealBinOp_t, Pow, value)
            create_ElementalBinOp(Integer, make_IntegerBinOp_t, Pow, value)
            create_ElementalBinOp(Complex, make_ComplexBinOp_t, Pow, value)
            default: {
                throw LCompilersException("Expression type, " +
                                          std::to_string(expr_type(left)->type) +
                                          " not yet supported");
            }
        }
    }

    ASR::expr_t* ElementalMax(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        ASR::expr_t* test_condition = nullptr;
        switch (ASRUtils::expr_type(left)->type) {
            case ASR::ttypeType::Integer: {
                test_condition = make_Compare(make_IntegerCompare_t, left, Gt, right);
                break;
            }
            case ASR::ttypeType::Real: {
                test_condition = make_Compare(make_RealCompare_t, left, Gt, right);
                break;
            }
            default: {
                throw LCompilersException("Expression type, " +
                    std::to_string(expr_type(left)->type) + " not yet supported");
            }
        }
        return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, test_condition, left, right, ASRUtils::expr_type(left), value));
    }

    ASR::expr_t* ElementalMin(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc, ASR::expr_t* value=nullptr) {
        ASR::expr_t* test_condition = nullptr;
        switch (ASRUtils::expr_type(left)->type) {
            case ASR::ttypeType::Integer: {
                test_condition = make_Compare(make_IntegerCompare_t, left, Lt, right);
                break;
            }
            case ASR::ttypeType::Real: {
                test_condition = make_Compare(make_RealCompare_t, left, Lt, right);
                break;
            }
            default: {
                throw LCompilersException("Expression type, " +
                    std::to_string(expr_type(left)->type) + " not yet supported");
            }
        }
        return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, test_condition, left, right, ASRUtils::expr_type(left), value));
    }

    ASR::expr_t* ElementalOr(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc) {
        return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
            left, ASR::Or, right,
            ASRUtils::TYPE(ASR::make_Logical_t( al, loc, 4)), nullptr));
    }

    ASR::expr_t* LogicalOr(ASR::expr_t* left, ASR::expr_t* right,
        const Location& loc) {
        return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
            left, ASR::Or, right, ASRUtils::expr_type(left),
            nullptr));
    }

    ASR::expr_t* Call(ASR::symbol_t* s, Vec<ASR::call_arg_t>& args,
                      ASR::ttype_t* return_type) {
        return ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, loc,
                s, s, args.p, args.size(), return_type, nullptr, nullptr));
    }

    ASR::expr_t* Call(ASR::symbol_t* s, Vec<ASR::expr_t *>& args,
                      ASR::ttype_t* return_type) {
        Vec<ASR::call_arg_t> args_; args_.reserve(al, 2);
        visit_expr_list(al, args, args_);
        return ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, loc,
                s, s, args_.p, args_.size(), return_type, nullptr, nullptr));
    }

    ASR::expr_t* Call(ASR::symbol_t* s, Vec<ASR::call_arg_t>& args,
                      ASR::ttype_t* return_type, ASR::expr_t* value) {
        return ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, loc,
                s, s, args.p, args.size(), return_type, value, nullptr));
    }

    ASR::expr_t *ArrayItem_01(ASR::expr_t *arr, std::vector<ASR::expr_t*> idx) {
        Vec<ASR::expr_t*> idx_vars; idx_vars.reserve(al, 1);
        for (auto &x: idx) idx_vars.push_back(al, x);
        return PassUtils::create_array_ref(arr, idx_vars, al);
    }

    #define ArrayItem_02(arr, idx_vars) PassUtils::create_array_ref(arr,        \
        idx_vars, al)

    ASR::expr_t *ArrayConstant(std::vector<ASR::expr_t *> elements,
            ASR::ttype_t *base_type, bool cast2descriptor=true) {
        // This function only creates array with rank one
        // TODO: Support other dimensions
        Vec<ASR::expr_t *> m_eles; m_eles.reserve(al, 1);
        for (auto &x: elements) m_eles.push_back(al, x);

        ASR::ttype_t *fixed_size_type = Array({(int64_t) elements.size()}, base_type);
        ASR::expr_t *arr_constant = EXPR(ASR::make_ArrayConstant_t(al, loc,
            m_eles.p, m_eles.n, fixed_size_type, ASR::arraystorageType::ColMajor));

        if (cast2descriptor) {
            return cast_to_descriptor(al, arr_constant);
        } else {
            return arr_constant;
        }
    }

    ASR::dimension_t set_dim(ASR::expr_t *start, ASR::expr_t *length) {
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = start;
        dim.m_length = length;
        return dim;
    }

    // Statements --------------------------------------------------------------
    #define Return() STMT(ASR::make_Return_t(al, loc))

    ASR::stmt_t *Assignment(ASR::expr_t *lhs, ASR::expr_t *rhs) {
        LCOMPILERS_ASSERT(check_equal_type(expr_type(lhs), expr_type(rhs)));
        return STMT(ASR::make_Assignment_t(al, loc, lhs, rhs, nullptr));
    }

    template <typename T>
    ASR::stmt_t *Assign_Constant(ASR::expr_t *lhs, T init_value) {
        ASR::ttype_t *type = expr_type(lhs);
        switch(type->type) {
            case ASR::ttypeType::Integer : {
                return Assignment(lhs, i(init_value, type));
            }
            case ASR::ttypeType::Real : {
                return Assignment(lhs, f(init_value, type));
            }
            default : {
                LCOMPILERS_ASSERT(false);
                return nullptr;
            }
        }
    }

    ASR::stmt_t *Allocate(ASR::expr_t *m_a, Vec<ASR::dimension_t> dims) {
        Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
        ASR::alloc_arg_t alloc_arg;
        alloc_arg.loc = loc;
        alloc_arg.m_a = m_a;
        alloc_arg.m_dims = dims.p;
        alloc_arg.n_dims = dims.n;
        alloc_arg.m_type = nullptr;
        alloc_arg.m_len_expr = nullptr;
        alloc_args.push_back(al, alloc_arg);
        return STMT(ASR::make_Allocate_t(al, loc, alloc_args.p, 1,
            nullptr, nullptr, nullptr));
    }

    #define UBound(arr, dim) PassUtils::get_bound(arr, dim, "ubound", al)
    #define LBound(arr, dim) PassUtils::get_bound(arr, dim, "lbound", al)

    ASR::stmt_t *DoLoop(ASR::expr_t *m_v, ASR::expr_t *start, ASR::expr_t *end,
            std::vector<ASR::stmt_t*> loop_body, ASR::expr_t *step=nullptr) {
        ASR::do_loop_head_t head;
        head.loc = m_v->base.loc;
        head.m_v = m_v;
        head.m_start = start;
        head.m_end = end;
        head.m_increment = step;
        Vec<ASR::stmt_t *> body;
        body.from_pointer_n_copy(al, &loop_body[0], loop_body.size());
        return STMT(ASR::make_DoLoop_t(al, loc, nullptr, head, body.p, body.n));
    }

    template <typename LOOP_BODY>
    ASR::stmt_t* create_do_loop(
        const Location& loc, int rank, ASR::expr_t* array,
        SymbolTable* scope, Vec<ASR::expr_t*>& idx_vars,
        Vec<ASR::stmt_t*>& doloop_body, LOOP_BODY loop_body) {
        PassUtils::create_idx_vars(idx_vars, rank, loc, al, scope, "_i");

        ASR::stmt_t* doloop = nullptr;
        for( int i = (int) idx_vars.size() - 1; i >= 0; i-- ) {
            ASR::do_loop_head_t head;
            head.m_v = idx_vars[i];
            head.m_start = PassUtils::get_bound(array, i + 1, "lbound", al);
            head.m_end = PassUtils::get_bound(array, i + 1, "ubound", al);
            head.m_increment = nullptr;

            head.loc = head.m_v->base.loc;
            doloop_body.reserve(al, 1);
            if( doloop == nullptr ) {
                loop_body();
            } else {
                doloop_body.push_back(al, doloop);
            }
            doloop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                        head, doloop_body.p, doloop_body.size()));
        }
        return doloop;
    }

    template <typename LOOP_BODY>
    ASR::stmt_t* create_do_loop(
        const Location& loc, ASR::expr_t* array,
        Vec<ASR::expr_t*>& loop_vars, std::vector<int>& loop_dims,
        Vec<ASR::stmt_t*>& doloop_body, LOOP_BODY loop_body) {

        ASR::stmt_t* doloop = nullptr;
        for( int i = (int) loop_vars.size() - 1; i >= 0; i-- ) {
            ASR::do_loop_head_t head;
            head.m_v = loop_vars[i];
            head.m_start = PassUtils::get_bound(array, loop_dims[i], "lbound", al);
            head.m_end = PassUtils::get_bound(array, loop_dims[i], "ubound", al);
            head.m_increment = nullptr;

            head.loc = head.m_v->base.loc;
            doloop_body.reserve(al, 1);
            if( doloop == nullptr ) {
                loop_body();
            } else {
                doloop_body.push_back(al, doloop);
            }
            doloop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                        head, doloop_body.p, doloop_body.size()));
        }
        return doloop;
    }

    template <typename INIT, typename LOOP_BODY>
    void generate_reduction_intrinsic_stmts_for_scalar_output(const Location& loc,
    ASR::expr_t* array, SymbolTable* fn_scope,
    Vec<ASR::stmt_t*>& fn_body, Vec<ASR::expr_t*>& idx_vars,
    Vec<ASR::stmt_t*>& doloop_body, INIT init_stmts, LOOP_BODY loop_body) {
        init_stmts();
        int rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(array));
        ASR::stmt_t* doloop = create_do_loop(loc,
            rank, array, fn_scope, idx_vars, doloop_body,
            loop_body);
        fn_body.push_back(al, doloop);
    }

    template <typename INIT, typename LOOP_BODY>
    void generate_reduction_intrinsic_stmts_for_array_output(const Location& loc,
        ASR::expr_t* array, ASR::expr_t* dim, SymbolTable* fn_scope,
        Vec<ASR::stmt_t*>& fn_body, Vec<ASR::expr_t*>& idx_vars,
        Vec<ASR::expr_t*>& target_idx_vars, Vec<ASR::stmt_t*>& doloop_body,
        INIT init_stmts, LOOP_BODY loop_body) {
        init_stmts();
        int n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(array));
        ASR::stmt_t** else_ = nullptr;
        size_t else_n = 0;
        idx_vars.reserve(al, n_dims);
        PassUtils::create_idx_vars(idx_vars, n_dims, loc, al, fn_scope, "_j");
        for( int i = 1; i <= n_dims; i++ ) {
            ASR::expr_t* current_dim = i32(i);
            ASR::expr_t* test_expr = make_Compare(make_IntegerCompare_t, dim,
                                        Eq, current_dim);

            Vec<ASR::expr_t*> loop_vars;
            std::vector<int> loop_dims;
            loop_dims.reserve(n_dims);
            loop_vars.reserve(al, n_dims);
            target_idx_vars.reserve(al, n_dims - 1);
            for( int j = 1; j <= n_dims; j++ ) {
                if( j == i ) {
                    continue ;
                }
                target_idx_vars.push_back(al, idx_vars[j - 1]);
                loop_dims.push_back(j);
                loop_vars.push_back(al, idx_vars[j - 1]);
            }
            loop_dims.push_back(i);
            loop_vars.push_back(al, idx_vars[i - 1]);

            ASR::stmt_t* doloop = create_do_loop(loc,
            array, loop_vars, loop_dims, doloop_body,
            loop_body);
            Vec<ASR::stmt_t*> if_body;
            if_body.reserve(al, 1);
            if_body.push_back(al, doloop);
            ASR::stmt_t* if_ = ASRUtils::STMT(ASR::make_If_t(al, loc, test_expr,
                                if_body.p, if_body.size(), else_, else_n));
            Vec<ASR::stmt_t*> if_else_if;
            if_else_if.reserve(al, 1);
            if_else_if.push_back(al, if_);
            else_ = if_else_if.p;
            else_n = if_else_if.size();
        }
        fn_body.push_back(al, else_[0]);
    }

    ASR::stmt_t *Print(std::vector<ASR::expr_t *> items) {
        // Used for debugging
        Vec<ASR::expr_t *> x_exprs;
        x_exprs.from_pointer_n_copy(al, &items[0], items.size());
        return STMT(ASR::make_Print_t(al, loc, x_exprs.p, x_exprs.n,
            nullptr, nullptr));
    }

};

} // namespace LCompilers::ASRUtils

#endif // LIBASR_BUILDER_H
