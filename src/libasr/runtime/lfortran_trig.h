/*
 * lfortran_trig.h
 *
 * Trigonometric functions of the runtime (lfortran_intrinsics.c) with an
 * exact argument reduction. The compiler folds constant expressions with
 * these same functions, so that a folded value agrees bit for bit with the
 * value computed at run time.
 */

#ifndef LFORTRAN_TRIG_H
#define LFORTRAN_TRIG_H

#ifdef __cplusplus
extern "C" {
#endif

/* Fortran 2023 sind, cosd and tand of real(4) and real(8) */
float  _lfortran_ssind(float x);
double _lfortran_dsind(double x);
float  _lfortran_scosd(float x);
double _lfortran_dcosd(double x);
float  _lfortran_stand(float x);
double _lfortran_dtand(double x);

/* Fortran 2023 sinpi, cospi and tanpi of real(4) and real(8) */
float  _lfortran_ssinpi(float x);
double _lfortran_dsinpi(double x);
float  _lfortran_scospi(float x);
double _lfortran_dcospi(double x);
float  _lfortran_stanpi(float x);
double _lfortran_dtanpi(double x);

#ifdef __cplusplus
}
#endif

#endif /* LFORTRAN_TRIG_H */
