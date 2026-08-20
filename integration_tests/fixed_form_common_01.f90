      PROGRAM FIXED_FORM_COMMON_01
      COMMON /C/ A(20/4)
      A(1) = 2.5
      A(5) = 3.5
      IF (SIZE(A) /= 5) ERROR STOP
      IF (ABS(A(1) - 2.5) > 1.0E-6) ERROR STOP
      IF (ABS(A(5) - 3.5) > 1.0E-6) ERROR STOP
      PRINT *, A(1), A(5)
      END
