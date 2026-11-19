      PROGRAM FIXED_FORM_LABEL_01
C     A labeled END IF and a labeled END are branch targets
      INTEGER I, N, R
      N = 0
      DO I = 1, 4
         IF (I .GT. 0) THEN
            IF (I .EQ. 2) GO TO 10
            N = N + I
   10    END IF
      END DO
      IF (N .NE. 8) ERROR STOP 1
      CALL S(3, R)
      IF (R .NE. 6) ERROR STOP 2
      CALL S(-1, R)
      IF (R .NE. 0) ERROR STOP 3
      PRINT *, N
      END

      SUBROUTINE S(K, R)
      INTEGER K, R
      R = 0
      IF (K .LT. 0) GO TO 30
      R = 2*K
   30 END
