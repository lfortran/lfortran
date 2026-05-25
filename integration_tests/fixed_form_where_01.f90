      PROGRAM FIXED_FORM_WHERE_01
      INTEGER I, N
      PARAMETER (N=3)
      REAL YP(N), YPOLD(N), CURTOL

      CURTOL = 1.0
      YPOLD = (/ 1.0, 2.0, 3.0 /)
      YP = 0.0
      DO 10 I = 1, 5
      WHERE (ABS(YP-YPOLD) .LE. CURTOL)
         YP = YPOLD
      ELSEWHERE
         YP = 0.0
      END WHERE
   10 CONTINUE

      PRINT *, YP
      IF (ANY(YP .NE. [1.0, 0.0, 0.0])) STOP 2
      END
