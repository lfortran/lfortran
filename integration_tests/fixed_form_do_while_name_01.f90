      PROGRAM FIXED_FORM_DO_WHILE_NAME_01
      INTEGER K
      K = 1
      L: DO WHILE (K .LE. 2)
        K = K + 1
      END DO L
      IF (K .NE. 3) ERROR STOP 1
      END
