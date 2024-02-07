#ifndef LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H
#define LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H

#include <libasr/pass/intrinsic_function_registry_util.h>

#include <cmath>
#include <string>
#include <tuple>

namespace LCompilers {

namespace ASRUtils {

#define INTRINSIC_NAME_CASE(X)                                                  \
    case (static_cast<int64_t>(ASRUtils::IntrinsicScalarFunctions::X)) : {      \
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
        INTRINSIC_NAME_CASE(Shiftl)
        INTRINSIC_NAME_CASE(Ishft)
        INTRINSIC_NAME_CASE(Leadz)
        INTRINSIC_NAME_CASE(Digits)
        INTRINSIC_NAME_CASE(Repeat)
        INTRINSIC_NAME_CASE(Hypot)
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
        INTRINSIC_NAME_CASE(Sngl)
        INTRINSIC_NAME_CASE(Ifix)
        INTRINSIC_NAME_CASE(Idint)
        INTRINSIC_NAME_CASE(Floor)
        INTRINSIC_NAME_CASE(Ceiling)
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

namespace IntrinsicScalarFunctionRegistry {

    static const std::map<int64_t,
        std::tuple<impl_function,
                   verify_function>>& intrinsic_function_by_id_db = {
        {static_cast<int64_t>(IntrinsicScalarFunctions::Gamma),
            {&Gamma::instantiate_Gamma, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Log10),
            {&Log10::instantiate_Log10, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Log),
            {&Log::instantiate_Log, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::LogGamma),
            {&LogGamma::instantiate_LogGamma, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Erf),
            {&Erf::instantiate_Erf, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Erfc),
            {&Erfc::instantiate_Erfc, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Trunc),
            {&Trunc::instantiate_Trunc, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Fix),
            {&Fix::instantiate_Fix, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sin),
            {&Sin::instantiate_Sin, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Cos),
            {&Cos::instantiate_Cos, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Tan),
            {&Tan::instantiate_Tan, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Asin),
            {&Asin::instantiate_Asin, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Acos),
            {&Acos::instantiate_Acos, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Atan),
            {&Atan::instantiate_Atan, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sinh),
            {&Sinh::instantiate_Sinh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Cosh),
            {&Cosh::instantiate_Cosh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Tanh),
            {&Tanh::instantiate_Tanh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Atan2),
            {&Atan2::instantiate_Atan2, &BinaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Asinh),
            {&Asinh::instantiate_Asinh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Acosh),
            {&Acosh::instantiate_Acosh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Atanh),
            {&Atanh::instantiate_Atanh, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Exp),
            {&Exp::instantiate_Exp, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Exp2),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Expm1),
            {nullptr, &UnaryIntrinsicFunction::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::FMA),
            {&FMA::instantiate_FMA, &FMA::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::FlipSign),
            {&FlipSign::instantiate_FlipSign, &FlipSign::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::FloorDiv),
            {&FloorDiv::instantiate_FloorDiv, &FloorDiv::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Mod),
            {&Mod::instantiate_Mod, &Mod::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Trailz),
            {&Trailz::instantiate_Trailz, &Trailz::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Shiftr),
            {&Shiftr::instantiate_Shiftr, &Shiftr::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Shiftl),
            {&Shiftl::instantiate_Shiftl, &Shiftl::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Ishft),
            {&Ishft::instantiate_Ishft, &Ishft::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Leadz),
            {&Leadz::instantiate_Leadz, &Leadz::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Hypot),
            {&Hypot::instantiate_Hypot, &Hypot::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Kind),
            {&Kind::instantiate_Kind, &Kind::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Rank),
            {&Rank::instantiate_Rank, &Rank::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Digits),
            {&Digits::instantiate_Digits, &Digits::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Repeat),
            {&Repeat::instantiate_Repeat, &Repeat::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::MinExponent),
            {&MinExponent::instantiate_MinExponent, &MinExponent::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::MaxExponent),
            {&MaxExponent::instantiate_MaxExponent, &MaxExponent::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Abs),
            {&Abs::instantiate_Abs, &Abs::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Aimag),
            {&Aimag::instantiate_Aimag, &Aimag::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Partition),
            {&Partition::instantiate_Partition, &Partition::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListIndex),
            {nullptr, &ListIndex::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListReverse),
            {nullptr, &ListReverse::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::DictKeys),
            {nullptr, &DictKeys::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::DictValues),
            {nullptr, &DictValues::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListPop),
            {nullptr, &ListPop::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListReserve),
            {nullptr, &ListReserve::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SetAdd),
            {nullptr, &SetAdd::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SetRemove),
            {nullptr, &SetRemove::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Max),
            {&Max::instantiate_Max, &Max::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Min),
            {&Min::instantiate_Min, &Min::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sign),
            {&Sign::instantiate_Sign, &Sign::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Radix),
            {&Radix::instantiate_Radix, &Radix::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Aint),
            {&Aint::instantiate_Aint, &Aint::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Nint),
            {&Nint::instantiate_Nint, &Nint::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Anint),
            {&Anint::instantiate_Anint, &Anint::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Floor),
            {&Floor::instantiate_Floor, &Floor::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Ceiling),
            {&Ceiling::instantiate_Ceiling, &Ceiling::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sqrt),
            {&Sqrt::instantiate_Sqrt, &Sqrt::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sngl),
            {&Sngl::instantiate_Sngl, &Sngl::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Ifix),
            {&Ifix::instantiate_Ifix, &Ifix::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Idint),
            {&Idint::instantiate_Idint, &Idint::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SignFromValue),
            {&SignFromValue::instantiate_SignFromValue, &SignFromValue::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSymbol),
            {nullptr, &SymbolicSymbol::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicAdd),
            {nullptr, &SymbolicAdd::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSub),
            {nullptr, &SymbolicSub::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicMul),
            {nullptr, &SymbolicMul::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicDiv),
            {nullptr, &SymbolicDiv::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicPow),
            {nullptr, &SymbolicPow::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicPi),
            {nullptr, &SymbolicPi::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicE),
            {nullptr, &SymbolicE::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicInteger),
            {nullptr, &SymbolicInteger::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicDiff),
            {nullptr, &SymbolicDiff::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicExpand),
            {nullptr, &SymbolicExpand::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSin),
            {nullptr, &SymbolicSin::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicCos),
            {nullptr, &SymbolicCos::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicLog),
            {nullptr, &SymbolicLog::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicExp),
            {nullptr, &SymbolicExp::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicAbs),
            {nullptr, &SymbolicAbs::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicHasSymbolQ),
            {nullptr, &SymbolicHasSymbolQ::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicAddQ),
            {nullptr, &SymbolicAddQ::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicMulQ),
            {nullptr, &SymbolicMulQ::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicPowQ),
            {nullptr, &SymbolicPowQ::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicLogQ),
            {nullptr, &SymbolicLogQ::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSinQ),
            {nullptr, &SymbolicSinQ::verify_args}},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicGetArgument),
            {nullptr, &SymbolicGetArgument::verify_args}},
    };

    static const std::map<int64_t, std::string>& intrinsic_function_id_to_name = {
        {static_cast<int64_t>(IntrinsicScalarFunctions::Gamma),
            "gamma"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Log),
            "log"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Log10),
            "log10"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::LogGamma),
            "log_gamma"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Erf),
            "erf"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Erfc),
            "erfc"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Trunc),
            "trunc"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Fix),
            "fix"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sin),
            "sin"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Cos),
            "cos"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Tan),
            "tan"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Asin),
            "asin"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Acos),
            "acos"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Atan),
            "atan"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sinh),
            "sinh"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Cosh),
            "cosh"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Tanh),
            "tanh"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Atan2),
            "atan2"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Asinh),
            "asinh"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Acosh),
            "acosh"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Atanh),
            "atanh"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Abs),
            "abs"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Aimag),
            "aimag"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Exp),
            "exp"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Exp2),
            "exp2"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::FMA),
            "fma"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::FlipSign),
            "flipsign"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::FloorDiv),
            "floordiv"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Mod),
            "mod"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Trailz),
            "trailz"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Shiftr),
            "shiftr"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Shiftl),
            "shiftl"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Ishft),
            "ishft"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Leadz),
            "leadz"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Hypot),
            "hypot"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Kind),
            "kind"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Rank),
            "rank"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Digits),
            "Digits"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Repeat),
            "Repeat"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::MinExponent),
            "minexponent"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::MaxExponent),
            "maxexponent"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Expm1),
            "expm1"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListIndex),
            "list.index"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListReverse),
            "list.reverse"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListPop),
            "list.pop"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::ListReserve),
            "list.reserve"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::DictKeys),
            "dict.keys"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::DictValues),
            "dict.values"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SetAdd),
            "set.add"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SetRemove),
            "set.remove"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Max),
            "max"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Min),
            "min"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Radix),
            "radix"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sign),
            "sign"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Aint),
            "aint"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Nint),
            "nint"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Anint),
            "anint"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Floor),
            "floor"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Ceiling),
            "ceiling"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sqrt),
            "sqrt"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Sngl),
            "sngl"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Idint),
            "idint"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::Ifix),
            "ifix"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SignFromValue),
            "signfromvalue"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSymbol),
            "Symbol"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicAdd),
            "SymbolicAdd"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSub),
            "SymbolicSub"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicMul),
            "SymbolicMul"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicDiv),
            "SymbolicDiv"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicPow),
            "SymbolicPow"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicPi),
            "pi"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicE),
            "E"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicInteger),
            "SymbolicInteger"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicDiff),
            "SymbolicDiff"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicExpand),
            "SymbolicExpand"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSin),
            "SymbolicSin"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicCos),
            "SymbolicCos"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicLog),
            "SymbolicLog"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicExp),
            "SymbolicExp"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicAbs),
            "SymbolicAbs"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicHasSymbolQ),
            "SymbolicHasSymbolQ"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicAddQ),
            "SymbolicAddQ"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicMulQ),
            "SymbolicMulQ"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicPowQ),
            "SymbolicPowQ"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicLogQ),
            "SymbolicLogQ"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSinQ),
            "SymbolicSinQ"},
        {static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicGetArgument),
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
                {"shiftl", {&Shiftl::create_Shiftl, &Shiftl::eval_Shiftl}},
                {"lshift", {&Shiftl::create_Shiftl, &Shiftl::eval_Shiftl}},
                {"ishft", {&Ishft::create_Ishft, &Ishft::eval_Ishft}},
                {"leadz", {&Leadz::create_Leadz, &Leadz::eval_Leadz}},
                {"hypot", {&Hypot::create_Hypot, &Hypot::eval_Hypot}},
                {"kind", {&Kind::create_Kind, &Kind::eval_Kind}},
                {"rank", {&Rank::create_Rank, &Rank::eval_Rank}},
                {"digits", {&Digits::create_Digits, &Digits::eval_Digits}},
                {"repeat", {&Repeat::create_Repeat, &Repeat::eval_Repeat}},
                {"minexponent", {&MinExponent::create_MinExponent, &MinExponent::eval_MinExponent}},
                {"maxexponent", {&MaxExponent::create_MaxExponent, &MaxExponent::eval_MaxExponent}},
                {"list.index", {&ListIndex::create_ListIndex, &ListIndex::eval_list_index}},
                {"list.reverse", {&ListReverse::create_ListReverse, &ListReverse::eval_list_reverse}},
                {"list.pop", {&ListPop::create_ListPop, &ListPop::eval_list_pop}},
                {"list.reserve", {&ListReserve::create_ListReserve, &ListReserve::eval_list_reserve}},
                {"dict.keys", {&DictKeys::create_DictKeys, &DictKeys::eval_dict_keys}},
                {"dict.values", {&DictValues::create_DictValues, &DictValues::eval_dict_values}},
                {"set.add", {&SetAdd::create_SetAdd, &SetAdd::eval_set_add}},
                {"set.remove", {&SetRemove::create_SetRemove, &SetRemove::eval_set_remove}},
                {"max0", {&Max::create_Max, &Max::eval_Max}},
                {"min0", {&Min::create_Min, &Min::eval_Min}},
                {"max", {&Max::create_Max, &Max::eval_Max}},
                {"min", {&Min::create_Min, &Min::eval_Min}},
                {"radix", {&Radix::create_Radix, Radix::eval_Radix}},
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

    static inline bool is_elemental(int64_t id) {
        IntrinsicScalarFunctions id_ = static_cast<IntrinsicScalarFunctions>(id);
        return ( id_ == IntrinsicScalarFunctions::Abs ||
                 id_ == IntrinsicScalarFunctions::Cos ||
                 id_ == IntrinsicScalarFunctions::Gamma ||
                 id_ == IntrinsicScalarFunctions::Log ||
                 id_ == IntrinsicScalarFunctions::LogGamma ||
                 id_ == IntrinsicScalarFunctions::Erf ||
                 id_ == IntrinsicScalarFunctions::Erfc ||
                 id_ == IntrinsicScalarFunctions::Trunc ||
                 id_ == IntrinsicScalarFunctions::Fix ||
                 id_ == IntrinsicScalarFunctions::Sin ||
                 id_ == IntrinsicScalarFunctions::Exp ||
                 id_ == IntrinsicScalarFunctions::Exp2 ||
                 id_ == IntrinsicScalarFunctions::Expm1 ||
                 id_ == IntrinsicScalarFunctions::Min ||
                 id_ == IntrinsicScalarFunctions::Max ||
                 id_ == IntrinsicScalarFunctions::Sqrt ||
                 id_ == IntrinsicScalarFunctions::SymbolicSymbol ||
                 id_ == IntrinsicScalarFunctions::Tan ||
                 id_ == IntrinsicScalarFunctions::Acosh ||
                 id_ == IntrinsicScalarFunctions::Asinh ||
                 id_ == IntrinsicScalarFunctions::Atanh ||
                 id_ == IntrinsicScalarFunctions::Cosh ||
                 id_ == IntrinsicScalarFunctions::Sinh ||
                 id_ == IntrinsicScalarFunctions::Tanh);
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

} // namespace IntrinsicScalarFunctionRegistry

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
