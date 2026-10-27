C     `lfortran fmt --fixed-form-infer --cpp-infer` infers fixed form and
C     preprocessing from the .F extension of the file being formatted
#define N 2
      program fmt_fixed_form_infer1
      integer i
      i = 1 +
     +    N
      print *, i
      end
