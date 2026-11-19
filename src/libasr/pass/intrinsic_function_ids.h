#ifndef LIBASR_PASS_INTRINSIC_FUNCTION_IDS_H
#define LIBASR_PASS_INTRINSIC_FUNCTION_IDS_H

#include <libasr/asr.h>

namespace LCompilers::ASRUtils {

enum class IntrinsicElementalFunctions : int64_t {
    ObjectType,
    Kind, // if kind is reordered, update `extract_kind` in `asr_utils.h`
    Mod, // if mod is reordered, update `pass/openmp.cpp`
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Atan2,
    Atan2d,
    Asinh,
    Sind,
    Cosd,
    Tand,
    Asind,
    Acosd,
    Atand,
    Acosh,
    Atanh,
    Sinpi,
    Cospi,
    Tanpi,
    Asinpi,
    Acospi,
    Atanpi,
    Atan2pi,
    Erf,
    Erfc,
    ErfcScaled,
    Gamma,
    Log,
    Log10,
    LogGamma,
    Trunc,
    Fix,
    Abs,
    Aimag,
    Dreal,
    Exp,
    Exp2,
    Expm1,
    FMA,
    FlipSign,
    Trailz,
    Isnan,
    Nearest,
    Spacing,
    Modulo,
    BesselJ0,
    BesselJ1,
    BesselJN,
    BesselY0,
    BesselY1,
    BesselYN,
    SameTypeAs,
    ExtendsTypeOf,
    Merge,
    Mergebits,
    Shiftr,
    Rshift,
    Shiftl,
    Dshiftl,
    Dshiftr,
    Ishft,
    OutOfRange,
    Bgt,
    Blt,
    Bge,
    Ble,
    Lgt,
    Llt,
    Lge,
    Lle,
    Exponent,
    Fraction,
    SetExponent,
    Not,
    Iand,
    Ior,
    Ieor,
    Ibclr,
    Ibset,
    Btest,
    Ibits,
    Leadz,
    ToLowerCase,
    Digits,
    Rrspacing,
    Repeat,
    StringContainsSet,
    StringFindSet,
    SubstrIndex,
    Hypot,
    SelectedIntKind,
    SelectedRealKind,
    SelectedCharKind,
    SelectedLogicalKind,
    Adjustl,
    Adjustr,
    StringConcat,
    StringLenTrim,
    StringTrim,
    Ichar,
    Char,
    Achar,
    Iachar,
    MinExponent,
    MaxExponent,
    FloorDiv,
    ListIndex,
    Partition,
    ListReverse,
    ListPop,
    ListReserve,
    DictKeys,
    DictValues,
    SetAdd,
    SetRemove,
    Max,
    Min,
    Radix,
    IsContiguous,
    StorageSize,
    Scale,
    Dprod,
    Range,
    Sign,
    CompilerVersion,
    CommandArgumentCount,
    ThisImage,
    NumImages,
    LCoBound,
    UCoBound,
    SignFromValue,
    Logical,
    Nint,
    Idnint,
    Aint,
    Anint,
    Dim,
    Sqrt,
    Sngl,
    Ifix,
    Idint,
    Floor,
    Ceiling,
    Ishftc,
    Maskr,
    Maskl,
    Epsilon,
    Precision,
    Tiny,
    BitSize,
    NewLine,
    Conjg,
    Huge,
    Loc,
    Popcnt,
    Poppar,
    Real,
    Cmplx,
    SymbolicSymbol,
    SymbolicAdd,
    SymbolicSub,
    SymbolicMul,
    SymbolicDiv,
    SymbolicPow,
    SymbolicPi,
    SymbolicE,
    SymbolicInteger,
    SymbolicDiff,
    SymbolicExpand,
    SymbolicSin,
    SymbolicCos,
    SymbolicLog,
    SymbolicExp,
    SymbolicAbs,
    SymbolicHasSymbolQ,
    SymbolicAddQ,
    SymbolicMulQ,
    SymbolicPowQ,
    SymbolicLogQ,
    SymbolicSinQ,
    SymbolicGetArgument,
    Int,
    Present,
    And,
    Or,
    Xor,
    Rand
    // ...
};

typedef ASR::expr_t* (*impl_function)(
    Allocator&, const Location &,
    SymbolTable*, Vec<ASR::ttype_t*>&, ASR::ttype_t *,
    Vec<ASR::call_arg_t>&, int64_t, int);

typedef ASR::expr_t* (*eval_intrinsic_function)(
    Allocator&, const Location &, ASR::ttype_t *,
    Vec<ASR::expr_t*>&, diag::Diagnostics&);

typedef ASR::asr_t* (*create_intrinsic_function)(
    Allocator&, const Location&,
    Vec<ASR::expr_t*>&,
    diag::Diagnostics&);

typedef void (*verify_function)(
    const ASR::IntrinsicElementalFunction_t&,
    diag::Diagnostics&);

typedef ASR::expr_t* (*get_initial_value_func)(Allocator&, ASR::ttype_t*);

/************************* Intrinsic Impure Function **************************/
enum class IntrinsicImpureFunctions : int64_t {
    IsIostatEnd,
    IsIostatEor,
    Allocated,
    GetCwd,
    // ...
};

typedef void (*verify_impure_function)(
    const ASR::IntrinsicImpureFunction_t&, diag::Diagnostics&);

typedef ASR::expr_t* (*instantiate_impure_function)(
    Allocator&, const Location&, SymbolTable*, Vec<ASR::call_arg_t>&);

/************************* Intrinsic Array Functions **************************/
enum class IntrinsicArrayFunctions : int64_t {
    Any,
    All,
    Iany,
    Iall,
    Norm2,
    MatMul,
    MaxLoc,
    MaxVal,
    MinLoc,
    MinVal,
    FindLoc,
    Product,
    Shape,
    Coshape,
    Sum,
    Iparity,
    Transpose,
    Pack,
    Unpack,
    Count,
    Parity,
    DotProduct,
    Cshift,
    Eoshift,
    Spread,
    Reduce,
    // ...
};

typedef void (*verify_array_func)(ASR::expr_t*, ASR::ttype_t*,
    const Location&, diag::Diagnostics&,
    ASRUtils::IntrinsicArrayFunctions);

typedef void (*verify_array_function)(
    const ASR::IntrinsicArrayFunction_t&,
    diag::Diagnostics&);

/************************ Intrinsic Impure Subroutines ************************/
enum class IntrinsicImpureSubroutines : int64_t {
    RandomNumber,
    RandomInit,
    RandomSeed,
    GetCommand,
    GetEnvironmentVariable,
    ExecuteCommandLine,
    GetCommandArgument,
    CpuTime,
    Srand,
    SystemClock,
    DateAndTime,
    MoveAlloc,
    Mvbits,
    Abort,
    System,
    Sleep,
    CoSum,
    CoMax,
    CoMin,
    CoBroadcast,
    Exit,
    GetCwd,
    Pause,
    // ...
};

typedef ASR::stmt_t* (*impl_subroutine)(
    Allocator&, const Location &,
    SymbolTable*, Vec<ASR::ttype_t*>&,
    Vec<ASR::call_arg_t>&, int64_t);

typedef ASR::asr_t* (*create_intrinsic_subroutine)(
    Allocator&, const Location&,
    Vec<ASR::expr_t*>&,
    diag::Diagnostics&);

typedef void (*verify_subroutine)(
    const ASR::IntrinsicImpureSubroutine_t&,
    diag::Diagnostics&);

} // namespace LCompilers::ASRUtils

#endif // LIBASR_PASS_INTRINSIC_FUNCTION_IDS_H
