/*
 * lfortran_trig.h
 *
 * Trigonometric functions of the runtime (lfortran_intrinsics.c) with an
 * exact argument reduction. The compiler folds constant expressions with
 * these same functions, so that a folded value agrees bit for bit with the
 * value computed at run time with the same libm (with a different target
 * libm, values away from the exact points can differ by an ulp).
 * The declarations carry the dllexport of their LFORTRAN_API definitions,
 * as MSVC rejects a definition whose linkage differs from its declaration.
 */

#ifndef LFORTRAN_TRIG_H
#define LFORTRAN_TRIG_H

#ifdef __cplusplus
extern "C" {
#endif

/* Fortran 2023 sind, cosd and tand of real(4) and real(8) */
#ifdef _WIN32
__declspec(dllexport)
#endif
float _lfortran_ssind(float x);
#ifdef _WIN32
__declspec(dllexport)
#endif
double _lfortran_dsind(double x);
#ifdef _WIN32
__declspec(dllexport)
#endif
float _lfortran_scosd(float x);
#ifdef _WIN32
__declspec(dllexport)
#endif
double _lfortran_dcosd(double x);
#ifdef _WIN32
__declspec(dllexport)
#endif
float _lfortran_stand(float x);
#ifdef _WIN32
__declspec(dllexport)
#endif
double _lfortran_dtand(double x);

/* Fortran 2023 sinpi, cospi and tanpi of real(4) and real(8) */
#ifdef _WIN32
__declspec(dllexport)
#endif
float _lfortran_ssinpi(float x);
#ifdef _WIN32
__declspec(dllexport)
#endif
double _lfortran_dsinpi(double x);
#ifdef _WIN32
__declspec(dllexport)
#endif
float _lfortran_scospi(float x);
#ifdef _WIN32
__declspec(dllexport)
#endif
double _lfortran_dcospi(double x);
#ifdef _WIN32
__declspec(dllexport)
#endif
float _lfortran_stanpi(float x);
#ifdef _WIN32
__declspec(dllexport)
#endif
double _lfortran_dtanpi(double x);

#ifdef __cplusplus
}
#endif

#endif /* LFORTRAN_TRIG_H */
