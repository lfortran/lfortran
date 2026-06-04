/*
 * lfortran_float128_llvm.h
 *
 * Declarations for the compiler-rt / soft-float ABI symbols that LLVM
 * automatically emits calls to when lowering fp128 operations.
 *
 * LFortran never calls these directly. LLVM generates calls like:
 *   %r = fadd fp128 %a, %b   →   call fp128 @__addtf3(fp128 %a, fp128 %b)
 *
 * The implementations live in lfortran_float128.c.
 * On Linux, libgcc/compiler-rt already provides these; our implementations
 * are marked weak on ELF so libgcc takes priority.
 * On macOS ARM64, naked-function shims bridge the Apple SIMD ABI to our
 * struct-based implementations.
 * On WASM, sret-style wrappers match the ABI LLVM uses for wasm32.
 */

#ifndef LFORTRAN_FLOAT128_LLVM_H
#define LFORTRAN_FLOAT128_LLVM_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Core type — 16-byte struct, little-endian IEEE 754 binary128 */
typedef struct { uint8_t bytes[16]; } lf_float128;

/* ── Internal _lf_impl variants (struct ABI, all platforms) ────────────── */
lf_float128 __addtf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __subtf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __multf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __divtf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __negtf2_lf_impl(lf_float128 a);

lf_float128 __extenddftf2_lf_impl(double a);
lf_float128 __extendsftf2_lf_impl(float  a);
double       __trunctfdf2_lf_impl(lf_float128 a);
float        __trunctfsf2_lf_impl(lf_float128 a);

lf_float128 __floatsitf_lf_impl(int32_t  a);
lf_float128 __floatditf_lf_impl(int64_t  a);
lf_float128 __floatunditf_lf_impl(uint64_t a);
int32_t      __fixtfsi_lf_impl(lf_float128 a);
int64_t      __fixtfdi_lf_impl(lf_float128 a);
int          __eqtf2_lf_impl   (lf_float128 a, lf_float128 b);
int          __netf2_lf_impl   (lf_float128 a, lf_float128 b);
int          __lttf2_lf_impl   (lf_float128 a, lf_float128 b);
int          __letf2_lf_impl   (lf_float128 a, lf_float128 b);
int          __gttf2_lf_impl   (lf_float128 a, lf_float128 b);
int          __getf2_lf_impl   (lf_float128 a, lf_float128 b);
int          __unordtf2_lf_impl(lf_float128 a, lf_float128 b);

/* ── Public ABI symbols (non-ARM64, non-WASM: thin wrappers) ───────────── */
#if !defined(__aarch64__) && !defined(__wasm32__)
lf_float128 __addtf3(lf_float128 a, lf_float128 b);
lf_float128 __subtf3(lf_float128 a, lf_float128 b);
lf_float128 __multf3(lf_float128 a, lf_float128 b);
lf_float128 __divtf3(lf_float128 a, lf_float128 b);
lf_float128 __negtf2(lf_float128 a);
lf_float128 __extenddftf2(double a);
lf_float128 __extendsftf2(float  a);
double       __trunctfdf2(lf_float128 a);
float        __trunctfsf2(lf_float128 a);
lf_float128 __floatsitf(int32_t  a);
lf_float128 __floatditf(int64_t  a);
lf_float128 __floatunditf(uint64_t a);
int32_t      __fixtfsi(lf_float128 a);
int64_t      __fixtfdi(lf_float128 a);
#endif

#ifdef __cplusplus
}
#endif

#endif /* LFORTRAN_FLOAT128_LLVM_H */