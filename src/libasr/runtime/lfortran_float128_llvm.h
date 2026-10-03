/*
 * lfortran_float128_llvm.h
 *
 * Declarations for the compiler-rt / soft-float ABI symbols that LLVM
 * automatically emits calls to when lowering fp128 operations.
 *
 * LFortran never calls these directly. LLVM generates calls like:
 *   %r = fadd fp128 %a, %b   →   call fp128 @__addtf3(fp128 %a, fp128 %b)
 *
 * The implementations live in lfortran_float128.c. They are written against
 * the 16-byte `lf_float128` struct, and each platform bridges that struct to
 * the calling convention LLVM actually uses for fp128:
 * On x86-64, a binary128 value travels in a single SSE register, so the
 * public entry points take a 16-byte vector rather than the struct.
 * On ARM64, naked-function shims bridge the SIMD ABI to the struct ABI.
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

#if !defined(__wasm32__)
int __eqtf2_lf_impl   (lf_float128 a, lf_float128 b);
int __netf2_lf_impl   (lf_float128 a, lf_float128 b);
int __lttf2_lf_impl   (lf_float128 a, lf_float128 b);
int __letf2_lf_impl   (lf_float128 a, lf_float128 b);
int __gttf2_lf_impl   (lf_float128 a, lf_float128 b);
int __getf2_lf_impl   (lf_float128 a, lf_float128 b);
int __unordtf2_lf_impl(lf_float128 a, lf_float128 b);
#endif

/* ── Public ABI symbols ─────────────────────────────────────────────────
 *
 * The compiler-rt entry points LLVM emits calls to (__addtf3, __eqtf2, ...)
 * are deliberately not declared here. LLVM calls them with the target's
 * native fp128 calling convention, which does not match the 16-byte struct
 * `lf_float128`, so their signatures differ per platform and are spelled out
 * where they are defined in lfortran_float128.c. Nothing outside that file
 * calls them from C. */

#ifdef __cplusplus
}
#endif

#endif /* LFORTRAN_FLOAT128_LLVM_H */