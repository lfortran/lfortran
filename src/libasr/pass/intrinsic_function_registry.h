#ifndef LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H
#define LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H

#include <libasr/pass/intrinsic_function_registry_util.h>

#include <cmath>
#include <string>
#include <tuple>

namespace LCompilers {

namespace ASRUtils {

#define INTRINSIC_NAME_CASE(X)                                                  \
    case (static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::X)) : {   \
        return #X;                                                              \
    }

inline std::string get_intrinsic_name(int x) {
    switch (x) {
        INTRINSIC_NAME_CASE(Kind)
        INTRINSIC_NAME_CASE(Rank)
        INTRINSIC_NAME_CASE(Sin)
        INTRINSIC_NAME_CASE(Cos)
        INTRINSIC_NAME_CASE(Tan)
        INTRINSIC_NAME_CASE(Asin)
        INTRINSIC_NAME_CASE(Acos)
        INTRINSIC_NAME_CASE(Atan)
        INTRINSIC_NAME_CASE(Sinh)
        INTRINSIC_NAME_CASE(Cosh)
        INTRINSIC_NAME_CASE(Tanh)
        INTRINSIC_NAME_CASE(Atan2)
        INTRINSIC_NAME_CASE(Asinh)
        INTRINSIC_NAME_CASE(Acosh)
        INTRINSIC_NAME_CASE(Atanh)
        INTRINSIC_NAME_CASE(Erf)
        INTRINSIC_NAME_CASE(Erfc)
        INTRINSIC_NAME_CASE(Gamma)
        INTRINSIC_NAME_CASE(Log)
        INTRINSIC_NAME_CASE(Log10)
        INTRINSIC_NAME_CASE(LogGamma)
        INTRINSIC_NAME_CASE(Trunc)
        INTRINSIC_NAME_CASE(Fix)
        INTRINSIC_NAME_CASE(Abs)
        INTRINSIC_NAME_CASE(Aimag)
        INTRINSIC_NAME_CASE(Exp)
        INTRINSIC_NAME_CASE(Exp2)
        INTRINSIC_NAME_CASE(Expm1)
        INTRINSIC_NAME_CASE(FMA)
        INTRINSIC_NAME_CASE(FlipSign)
        INTRINSIC_NAME_CASE(FloorDiv)
        INTRINSIC_NAME_CASE(Mod)
        INTRINSIC_NAME_CASE(Trailz)
        INTRINSIC_NAME_CASE(Shiftr)
        INTRINSIC_NAME_CASE(Rshift)
        INTRINSIC_NAME_CASE(Shiftl)
        INTRINSIC_NAME_CASE(Ishft)
        INTRINSIC_NAME_CASE(Leadz)
        INTRINSIC_NAME_CASE(Digits)
        INTRINSIC_NAME_CASE(Repeat)
        INTRINSIC_NAME_CASE(Range)
        INTRINSIC_NAME_CASE(Hypot)
        INTRINSIC_NAME_CASE(Selected_int_kind)
        INTRINSIC_NAME_CASE(MinExponent)
        INTRINSIC_NAME_CASE(MaxExponent)
        INTRINSIC_NAME_CASE(ListIndex)
        INTRINSIC_NAME_CASE(Partition)
        INTRINSIC_NAME_CASE(ListReverse)
        INTRINSIC_NAME_CASE(ListPop)
        INTRINSIC_NAME_CASE(ListReserve)
        INTRINSIC_NAME_CASE(DictKeys)
        INTRINSIC_NAME_CASE(DictValues)
        INTRINSIC_NAME_CASE(SetAdd)
        INTRINSIC_NAME_CASE(SetRemove)
        INTRINSIC_NAME_CASE(Max)
        INTRINSIC_NAME_CASE(Min)
        INTRINSIC_NAME_CASE(Sign)
        INTRINSIC_NAME_CASE(SignFromValue)
        INTRINSIC_NAME_CASE(Nint)
        INTRINSIC_NAME_CASE(Aint)
        INTRINSIC_NAME_CASE(Anint)
        INTRINSIC_NAME_CASE(Sqrt)
        INTRINSIC_NAME_CASE(Scale)
        INTRINSIC_NAME_CASE(Sngl)
        INTRINSIC_NAME_CASE(Ifix)
        INTRINSIC_NAME_CASE(Idint)
        INTRINSIC_NAME_CASE(Floor)
        INTRINSIC_NAME_CASE(Ceiling)
        INTRINSIC_NAME_CASE(Epsilon)
        INTRINSIC_NAME_CASE(Precision)
        INTRINSIC_NAME_CASE(Tiny)
        INTRINSIC_NAME_CASE(Conjg)
        INTRINSIC_NAME_CASE(Huge)
        INTRINSIC_NAME_CASE(SymbolicSymbol)
        INTRINSIC_NAME_CASE(SymbolicAdd)
        INTRINSIC_NAME_CASE(SymbolicSub)
        INTRINSIC_NAME_CASE(SymbolicMul)
        INTRINSIC_NAME_CASE(SymbolicDiv)
        INTRINSIC_NAME_CASE(SymbolicPow)
        INTRINSIC_NAME_CASE(SymbolicPi)
        INTRINSIC_NAME_CASE(SymbolicE)
        INTRINSIC_NAME_CASE(SymbolicInteger)
        INTRINSIC_NAME_CASE(SymbolicDiff)
        INTRINSIC_NAME_CASE(SymbolicExpand)
        INTRINSIC_NAME_CASE(SymbolicSin)
        INTRINSIC_NAME_CASE(SymbolicCos)
        INTRINSIC_NAME_CASE(SymbolicLog)
        INTRINSIC_NAME_CASE(SymbolicExp)
        INTRINSIC_NAME_CASE(SymbolicAbs)
        INTRINSIC_NAME_CASE(SymbolicHasSymbolQ)
        INTRINSIC_NAME_CASE(SymbolicAddQ)
        INTRINSIC_NAME_CASE(SymbolicMulQ)
        INTRINSIC_NAME_CASE(SymbolicPowQ)
        INTRINSIC_NAME_CASE(SymbolicLogQ)
        INTRINSIC_NAME_CASE(SymbolicSinQ)
        INTRINSIC_NAME_CASE(SymbolicGetArgument)
        default : {
            throw LCompilersException("pickle: intrinsic_id not implemented");
        }
    }
}

namespace IntrinsicElementalFunctionRegistry {

    static const std::map<int64_t,
        std::tuple<impl_function,
                   verify_function>>& intrinsic_function_by_id_db = {
        {static_cast<int64_t>(IntrinsicElementalFunctions::Gamma),
            {&Gamma::instantiate_Gamma, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Log10),
            {&Log10::instantiate_Log10, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Log),
            {&Log::instantiate_Log, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::LogGamma),
            {&LogGamma::instantiate_LogGamma, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Erf),
            {&Erf::instantiate_Erf, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Erfc),
            {&Erfc::instantiate_Erfc, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Trunc),
            {&Trunc::instantiate_Trunc, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Fix),
            {&Fix::instantiate_Fix, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sin),
            {&Sin::instantiate_Sin, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Cos),
            {&Cos::instantiate_Cos, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Tan),
            {&Tan::instantiate_Tan, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Asin),
            {&Asin::instantiate_Asin, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Acos),
            {&Acos::instantiate_Acos, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Atan),
            {&Atan::instantiate_Atan, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sinh),
            {&Sinh::instantiate_Sinh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Cosh),
            {&Cosh::instantiate_Cosh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Tanh),
            {&Tanh::instantiate_Tanh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Atan2),
            {&Atan2::instantiate_Atan2, &BinaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Asinh),
            {&Asinh::instantiate_Asinh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Acosh),
            {&Acosh::instantiate_Acosh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Atanh),
            {&Atanh::instantiate_Atanh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Exp),
            {&Exp::instantiate_Exp, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Exp2),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Expm1),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::FMA),
            {&FMA::instantiate_FMA, &FMA::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::FlipSign),
            {&FlipSign::instantiate_FlipSign, &FlipSign::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::FloorDiv),
            {&FloorDiv::instantiate_FloorDiv, &FloorDiv::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Mod),
            {&Mod::instantiate_Mod, &Mod::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Trailz),
            {&Trailz::instantiate_Trailz, &Trailz::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Shiftr),
            {&Shiftr::instantiate_Shiftr, &Shiftr::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Rshift),
            {&Rshift::instantiate_Rshift, &Rshift::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Shiftl),
            {&Shiftl::instantiate_Shiftl, &Shiftl::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Ishft),
            {&Ishft::instantiate_Ishft, &Ishft::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Leadz),
            {&Leadz::instantiate_Leadz, &Leadz::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Hypot),
            {&Hypot::instantiate_Hypot, &Hypot::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Kind),
            {&Kind::instantiate_Kind, &Kind::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Rank),
            {nullptr, &Rank::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Digits),
            {&Digits::instantiate_Digits, &Digits::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Repeat),
            {&Repeat::instantiate_Repeat, &Repeat::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::MinExponent),
            {&MinExponent::instantiate_MinExponent, &MinExponent::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::MaxExponent),
            {&MaxExponent::instantiate_MaxExponent, &MaxExponent::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Abs),
            {&Abs::instantiate_Abs, &Abs::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Aimag),
            {&Aimag::instantiate_Aimag, &Aimag::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Partition),
            {&Partition::instantiate_Partition, &Partition::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListIndex),
            {nullptr, &ListIndex::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListReverse),
            {nullptr, &ListReverse::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::DictKeys),
            {nullptr, &DictKeys::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::DictValues),
            {nullptr, &DictValues::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListPop),
            {nullptr, &ListPop::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListReserve),
            {nullptr, &ListReserve::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SetAdd),
            {nullptr, &SetAdd::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SetRemove),
            {nullptr, &SetRemove::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Max),
            {&Max::instantiate_Max, &Max::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Min),
            {&Min::instantiate_Min, &Min::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sign),
            {&Sign::instantiate_Sign, &Sign::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Radix),
            {nullptr, &Radix::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Scale),
            {&Scale::instantiate_Scale, &Scale::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Range),
            {nullptr, &Range::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Aint),
            {&Aint::instantiate_Aint, &Aint::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Nint),
            {&Nint::instantiate_Nint, &Nint::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Anint),
            {&Anint::instantiate_Anint, &Anint::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Floor),
            {&Floor::instantiate_Floor, &Floor::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Ceiling),
            {&Ceiling::instantiate_Ceiling, &Ceiling::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sqrt),
            {&Sqrt::instantiate_Sqrt, &Sqrt::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sngl),
            {&Sngl::instantiate_Sngl, &Sngl::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Ifix),
            {&Ifix::instantiate_Ifix, &Ifix::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Idint),
            {&Idint::instantiate_Idint, &Idint::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Conjg),
            {&Conjg::instantiate_Conjg, &Conjg::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SignFromValue),
            {&SignFromValue::instantiate_SignFromValue, &SignFromValue::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Epsilon),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Precision),
            {nullptr, &Precision::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Tiny),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Huge),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Selected_int_kind),
            {&Selected_int_kind::instantiate_Selected_int_kind, &Selected_int_kind::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSymbol),
            {nullptr, &SymbolicSymbol::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicAdd),
            {nullptr, &SymbolicAdd::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSub),
            {nullptr, &SymbolicSub::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicMul),
            {nullptr, &SymbolicMul::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicDiv),
            {nullptr, &SymbolicDiv::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicPow),
            {nullptr, &SymbolicPow::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicPi),
            {nullptr, &SymbolicPi::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicE),
            {nullptr, &SymbolicE::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicInteger),
            {nullptr, &SymbolicInteger::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicDiff),
            {nullptr, &SymbolicDiff::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicExpand),
            {nullptr, &SymbolicExpand::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSin),
            {nullptr, &SymbolicSin::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicCos),
            {nullptr, &SymbolicCos::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicLog),
            {nullptr, &SymbolicLog::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicExp),
            {nullptr, &SymbolicExp::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicAbs),
            {nullptr, &SymbolicAbs::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicHasSymbolQ),
            {nullptr, &SymbolicHasSymbolQ::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicAddQ),
            {nullptr, &SymbolicAddQ::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicMulQ),
            {nullptr, &SymbolicMulQ::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicPowQ),
            {nullptr, &SymbolicPowQ::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicLogQ),
            {nullptr, &SymbolicLogQ::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSinQ),
            {nullptr, &SymbolicSinQ::verify_args}},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicGetArgument),
            {nullptr, &SymbolicGetArgument::verify_args}},
    };

    static const std::map<int64_t, std::string>& intrinsic_function_id_to_name = {
        {static_cast<int64_t>(IntrinsicElementalFunctions::Gamma),
            "gamma"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Log),
            "log"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Log10),
            "log10"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::LogGamma),
            "log_gamma"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Erf),
            "erf"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Erfc),
            "erfc"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Trunc),
            "trunc"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Fix),
            "fix"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sin),
            "sin"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Cos),
            "cos"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Tan),
            "tan"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Asin),
            "asin"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Acos),
            "acos"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Atan),
            "atan"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sinh),
            "sinh"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Cosh),
            "cosh"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Tanh),
            "tanh"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Atan2),
            "atan2"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Asinh),
            "asinh"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Acosh),
            "acosh"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Atanh),
            "atanh"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Abs),
            "abs"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Aimag),
            "aimag"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Exp),
            "exp"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Exp2),
            "exp2"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::FMA),
            "fma"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::FlipSign),
            "flipsign"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::FloorDiv),
            "floordiv"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Mod),
            "mod"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Trailz),
            "trailz"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Shiftr),
            "shiftr"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Rshift),
            "rshift"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Shiftl),
            "shiftl"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Ishft),
            "ishft"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Leadz),
            "leadz"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Hypot),
            "hypot"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Selected_int_kind),
            "selected_int_kind"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Kind),
            "kind"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Rank),
            "rank"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Digits),
            "Digits"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Repeat),
            "Repeat"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::MinExponent),
            "minexponent"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::MaxExponent),
            "maxexponent"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Expm1),
            "expm1"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListIndex),
            "list.index"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListReverse),
            "list.reverse"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListPop),
            "list.pop"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::ListReserve),
            "list.reserve"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::DictKeys),
            "dict.keys"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::DictValues),
            "dict.values"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SetAdd),
            "set.add"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SetRemove),
            "set.remove"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Max),
            "max"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Min),
            "min"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Radix),
            "radix"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Scale),
            "scale"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Range),
            "range"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sign),
            "sign"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Aint),
            "aint"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Nint),
            "nint"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Anint),
            "anint"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Floor),
            "floor"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Ceiling),
            "ceiling"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sqrt),
            "sqrt"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Sngl),
            "sngl"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Idint),
            "idint"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Ifix),
            "ifix"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Conjg),
            "conjg"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SignFromValue),
            "signfromvalue"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Epsilon),
            "epsilon"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Precision),
            "precision"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Tiny),
            "tiny"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::Huge),
            "huge"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSymbol),
            "Symbol"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicAdd),
            "SymbolicAdd"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSub),
            "SymbolicSub"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicMul),
            "SymbolicMul"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicDiv),
            "SymbolicDiv"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicPow),
            "SymbolicPow"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicPi),
            "pi"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicE),
            "E"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicInteger),
            "SymbolicInteger"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicDiff),
            "SymbolicDiff"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicExpand),
            "SymbolicExpand"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSin),
            "SymbolicSin"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicCos),
            "SymbolicCos"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicLog),
            "SymbolicLog"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicExp),
            "SymbolicExp"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicAbs),
            "SymbolicAbs"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicHasSymbolQ),
            "SymbolicHasSymbolQ"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicAddQ),
            "SymbolicAddQ"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicMulQ),
            "SymbolicMulQ"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicPowQ),
            "SymbolicPowQ"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicLogQ),
            "SymbolicLogQ"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicSinQ),
            "SymbolicSinQ"},
        {static_cast<int64_t>(IntrinsicElementalFunctions::SymbolicGetArgument),
            "SymbolicGetArgument"},
    };


    static const std::map<std::string,
        std::tuple<create_intrinsic_function,
                    eval_intrinsic_function>>& intrinsic_function_by_name_db = {
                {"gamma", {&Gamma::create_Gamma, &Gamma::eval_Gamma}},
                {"log", {&Log::create_Log, &Log::eval_Log}},
                {"log10", {&Log10::create_Log10, &Log10::eval_Log10}},
                {"log_gamma", {&LogGamma::create_LogGamma, &LogGamma::eval_LogGamma}},
                {"erf", {&Erf::create_Erf, &Erf::eval_Erf}},
                {"erfc", {&Erfc::create_Erfc, &Erfc::eval_Erfc}},
                {"trunc", {&Trunc::create_Trunc, &Trunc::eval_Trunc}},
                {"fix", {&Fix::create_Fix, &Fix::eval_Fix}},
                {"sin", {&Sin::create_Sin, &Sin::eval_Sin}},
                {"cos", {&Cos::create_Cos, &Cos::eval_Cos}},
                {"tan", {&Tan::create_Tan, &Tan::eval_Tan}},
                {"asin", {&Asin::create_Asin, &Asin::eval_Asin}},
                {"acos", {&Acos::create_Acos, &Acos::eval_Acos}},
                {"atan", {&Atan::create_Atan, &Atan::eval_Atan}},
                {"sinh", {&Sinh::create_Sinh, &Sinh::eval_Sinh}},
                {"cosh", {&Cosh::create_Cosh, &Cosh::eval_Cosh}},
                {"tanh", {&Tanh::create_Tanh, &Tanh::eval_Tanh}},
                {"atan2", {&Atan2::create_Atan2, &Atan2::eval_Atan2}},
                {"asinh", {&Asinh::create_Asinh, &Asinh::eval_Asinh}},
                {"acosh", {&Acosh::create_Acosh, &Acosh::eval_Acosh}},
                {"atanh", {&Atanh::create_Atanh, &Atanh::eval_Atanh}},
                {"abs", {&Abs::create_Abs, &Abs::eval_Abs}},
                {"aimag", {&Aimag::create_Aimag, &Aimag::eval_Aimag}},
                {"exp", {&Exp::create_Exp, &Exp::eval_Exp}},
                {"exp2", {&Exp2::create_Exp2, &Exp2::eval_Exp2}},
                {"expm1", {&Expm1::create_Expm1, &Expm1::eval_Expm1}},
                {"fma", {&FMA::create_FMA, &FMA::eval_FMA}},
                {"floordiv", {&FloorDiv::create_FloorDiv, &FloorDiv::eval_FloorDiv}},
                {"mod", {&Mod::create_Mod, &Mod::eval_Mod}},
                {"trailz", {&Trailz::create_Trailz, &Trailz::eval_Trailz}},
                {"shiftr", {&Shiftr::create_Shiftr, &Shiftr::eval_Shiftr}},
                {"rshift", {&Rshift::create_Rshift, &Rshift::eval_Rshift}},
                {"shiftl", {&Shiftl::create_Shiftl, &Shiftl::eval_Shiftl}},
                {"lshift", {&Shiftl::create_Shiftl, &Shiftl::eval_Shiftl}},
                {"ishft", {&Ishft::create_Ishft, &Ishft::eval_Ishft}},
                {"leadz", {&Leadz::create_Leadz, &Leadz::eval_Leadz}},
                {"hypot", {&Hypot::create_Hypot, &Hypot::eval_Hypot}},
                {"selected_int_kind", {&Selected_int_kind::create_Selected_int_kind, &Selected_int_kind::eval_Selected_int_kind}},
                {"kind", {&Kind::create_Kind, &Kind::eval_Kind}},
                {"rank", {&Rank::create_Rank, &Rank::eval_Rank}},
                {"digits", {&Digits::create_Digits, &Digits::eval_Digits}},
                {"repeat", {&Repeat::create_Repeat, &Repeat::eval_Repeat}},
                {"minexponent", {&MinExponent::create_MinExponent, &MinExponent::eval_MinExponent}},
                {"maxexponent", {&MaxExponent::create_MaxExponent, &MaxExponent::eval_MaxExponent}},
                {"list.index", {&ListIndex::create_ListIndex, &ListIndex::eval_list_index}},
                {"list.reverse", {&ListReverse::create_ListReverse, &ListReverse::eval_ListReverse}},
                {"list.pop", {&ListPop::create_ListPop, &ListPop::eval_list_pop}},
                {"list.reserve", {&ListReserve::create_ListReserve, &ListReserve::eval_ListReserve}},
                {"dict.keys", {&DictKeys::create_DictKeys, &DictKeys::eval_dict_keys}},
                {"dict.values", {&DictValues::create_DictValues, &DictValues::eval_dict_values}},
                {"set.add", {&SetAdd::create_SetAdd, &SetAdd::eval_set_add}},
                {"set.remove", {&SetRemove::create_SetRemove, &SetRemove::eval_set_remove}},
                {"max0", {&Max::create_Max, &Max::eval_Max}},
                {"min0", {&Min::create_Min, &Min::eval_Min}},
                {"max", {&Max::create_Max, &Max::eval_Max}},
                {"min", {&Min::create_Min, &Min::eval_Min}},
                {"radix", {&Radix::create_Radix, &Radix::eval_Radix}},
                {"scale", {&Scale::create_Scale, &Scale::eval_Scale}},
                {"range", {&Range::create_Range, &Range::eval_Range}},
                {"sign", {&Sign::create_Sign, &Sign::eval_Sign}},
                {"aint", {&Aint::create_Aint, &Aint::eval_Aint}},
                {"nint", {&Nint::create_Nint, &Nint::eval_Nint}},
                {"anint", {&Anint::create_Anint, &Anint::eval_Anint}},
                {"floor", {&Floor::create_Floor, &Floor::eval_Floor}},
                {"ceiling", {&Ceiling::create_Ceiling, &Ceiling::eval_Ceiling}},
                {"sqrt", {&Sqrt::create_Sqrt, &Sqrt::eval_Sqrt}},
                {"sngl", {&Sngl::create_Sngl, &Sngl::eval_Sngl}},
                {"ifix", {&Ifix::create_Ifix, &Ifix::eval_Ifix}},
                {"idint", {&Idint::create_Idint, &Idint::eval_Idint}},
                {"epsilon", {&Epsilon::create_Epsilon, &Epsilon::eval_Epsilon}},
                {"precision", {&Precision::create_Precision, &Precision::eval_Precision}},
                {"tiny", {&Tiny::create_Tiny, &Tiny::eval_Tiny}},
                {"conjg", {&Conjg::create_Conjg, &Conjg::eval_Conjg}},
                {"huge", {&Huge::create_Huge, &Huge::eval_Huge}},
                {"Symbol", {&SymbolicSymbol::create_SymbolicSymbol, &SymbolicSymbol::eval_SymbolicSymbol}},
                {"SymbolicAdd", {&SymbolicAdd::create_SymbolicAdd, &SymbolicAdd::eval_SymbolicAdd}},
                {"SymbolicSub", {&SymbolicSub::create_SymbolicSub, &SymbolicSub::eval_SymbolicSub}},
                {"SymbolicMul", {&SymbolicMul::create_SymbolicMul, &SymbolicMul::eval_SymbolicMul}},
                {"SymbolicDiv", {&SymbolicDiv::create_SymbolicDiv, &SymbolicDiv::eval_SymbolicDiv}},
                {"SymbolicPow", {&SymbolicPow::create_SymbolicPow, &SymbolicPow::eval_SymbolicPow}},
                {"pi", {&SymbolicPi::create_SymbolicPi, &SymbolicPi::eval_SymbolicPi}},
                {"E", {&SymbolicE::create_SymbolicE, &SymbolicE::eval_SymbolicE}},
                {"SymbolicInteger", {&SymbolicInteger::create_SymbolicInteger, &SymbolicInteger::eval_SymbolicInteger}},
                {"diff", {&SymbolicDiff::create_SymbolicDiff, &SymbolicDiff::eval_SymbolicDiff}},
                {"expand", {&SymbolicExpand::create_SymbolicExpand, &SymbolicExpand::eval_SymbolicExpand}},
                {"SymbolicSin", {&SymbolicSin::create_SymbolicSin, &SymbolicSin::eval_SymbolicSin}},
                {"SymbolicCos", {&SymbolicCos::create_SymbolicCos, &SymbolicCos::eval_SymbolicCos}},
                {"SymbolicLog", {&SymbolicLog::create_SymbolicLog, &SymbolicLog::eval_SymbolicLog}},
                {"SymbolicExp", {&SymbolicExp::create_SymbolicExp, &SymbolicExp::eval_SymbolicExp}},
                {"SymbolicAbs", {&SymbolicAbs::create_SymbolicAbs, &SymbolicAbs::eval_SymbolicAbs}},
                {"has", {&SymbolicHasSymbolQ::create_SymbolicHasSymbolQ, &SymbolicHasSymbolQ::eval_SymbolicHasSymbolQ}},
                {"AddQ", {&SymbolicAddQ::create_SymbolicAddQ, &SymbolicAddQ::eval_SymbolicAddQ}},
                {"MulQ", {&SymbolicMulQ::create_SymbolicMulQ, &SymbolicMulQ::eval_SymbolicMulQ}},
                {"PowQ", {&SymbolicPowQ::create_SymbolicPowQ, &SymbolicPowQ::eval_SymbolicPowQ}},
                {"LogQ", {&SymbolicLogQ::create_SymbolicLogQ, &SymbolicLogQ::eval_SymbolicLogQ}},
                {"SinQ", {&SymbolicSinQ::create_SymbolicSinQ, &SymbolicSinQ::eval_SymbolicSinQ}},
                {"GetArgument", {&SymbolicGetArgument::create_SymbolicGetArgument, &SymbolicGetArgument::eval_SymbolicGetArgument}},
    };

    static inline bool is_intrinsic_function(const std::string& name) {
        return intrinsic_function_by_name_db.find(name) != intrinsic_function_by_name_db.end();
    }

    static inline bool is_intrinsic_function(int64_t id) {
        return intrinsic_function_by_id_db.find(id) != intrinsic_function_by_id_db.end();
    }

    static inline create_intrinsic_function get_create_function(const std::string& name) {
        return  std::get<0>(intrinsic_function_by_name_db.at(name));
    }

    static inline verify_function get_verify_function(int64_t id) {
        return std::get<1>(intrinsic_function_by_id_db.at(id));
    }

    static inline impl_function get_instantiate_function(int64_t id) {
        if( intrinsic_function_by_id_db.find(id) == intrinsic_function_by_id_db.end() ) {
            return nullptr;
        }
        return std::get<0>(intrinsic_function_by_id_db.at(id));
    }

    static inline std::string get_intrinsic_function_name(int64_t id) {
        if( intrinsic_function_id_to_name.find(id) == intrinsic_function_id_to_name.end() ) {
            throw LCompilersException("IntrinsicFunction with ID " + std::to_string(id) +
                                      " has no name registered for it");
        }
        return intrinsic_function_id_to_name.at(id);
    }

} // namespace IntrinsicElementalFunctionRegistry

/************************* Intrinsic Impure Function **************************/
enum class IntrinsicImpureFunctions : int64_t {
    IsIostatEnd,
    IsIostatEor,
    Allocated,
    // ...
};

namespace IsIostatEnd {

    static inline ASR::asr_t* create_IsIostatEnd(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& /*diag*/) {
        // Compile time value cannot be computed
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::IsIostatEnd),
                args.p, args.n, 0, logical, nullptr);
    }

} // namespace IsIostatEnd

namespace Allocated {

    static inline ASR::asr_t* create_Allocated(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args, diag::Diagnostics& diag) {
        // Compile time value cannot be computed
        if( args.n != 1 ) {
            append_error(diag, "Intrinsic `allocated` accepts exactly one argument", \
                loc);                                                           \
            return nullptr;
        }
        if( !ASRUtils::is_allocatable(args.p[0]) ) {
            append_error(diag, "Intrinsic `allocated` can be called only on" \
                " allocatable argument", loc);
            return nullptr;
        }
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated),
                args.p, args.n, 0, logical, nullptr);
    }

} // namespace Allocated

namespace IsIostatEor {

    static inline ASR::asr_t* create_IsIostatEor(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& /*diag*/) {
        // Compile time value cannot be computed
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::IsIostatEor),
                args.p, args.n, 0, logical, nullptr);
    }

} // namespace IsIostatEor

namespace IntrinsicImpureFunctionRegistry {

    static const std::map<std::string, std::tuple<create_intrinsic_function,
            eval_intrinsic_function>>& function_by_name_db = {
        {"is_iostat_end", {&IsIostatEnd::create_IsIostatEnd, nullptr}},
        {"is_iostat_eor", {&IsIostatEor::create_IsIostatEor, nullptr}},
        {"allocated", {&Allocated::create_Allocated, nullptr}},
    };

    static inline bool is_intrinsic_function(const std::string& name) {
        return function_by_name_db.find(name) != function_by_name_db.end();
    }

    static inline create_intrinsic_function get_create_function(const std::string& name) {
        return  std::get<0>(function_by_name_db.at(name));
    }

} // namespace IntrinsicImpureFunctionRegistry


#define IMPURE_INTRINSIC_NAME_CASE(X)                                           \
    case (static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::X)) : {      \
        return #X;                                                              \
    }

inline std::string get_impure_intrinsic_name(int x) {
    switch (x) {
        IMPURE_INTRINSIC_NAME_CASE(IsIostatEnd)
        IMPURE_INTRINSIC_NAME_CASE(IsIostatEor)
        IMPURE_INTRINSIC_NAME_CASE(Allocated)
        default : {
            throw LCompilersException("pickle: intrinsic_id not implemented");
        }
    }
}

} // namespace ASRUtils

} // namespace LCompilers

#endif // LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H
