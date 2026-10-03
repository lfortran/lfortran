      MODULE FIXED_FORM_TYPE_SAVE_01_MOD
        TYPE GT ! trailing comment must not defeat opener detection
          INTEGER N
        END TYPE GT
        TYPE(GT), SAVE:: DAT(10)
      END MODULE FIXED_FORM_TYPE_SAVE_01_MOD

      PROGRAM FIXED_FORM_TYPE_SAVE_01
      USE FIXED_FORM_TYPE_SAVE_01_MOD
      INTEGER I, TYPEX
      TYPEX = 5
      DO I = 1, 10
        DAT(I)%N = I*I
      END DO
      IF (DAT(10)%N .NE. 100) ERROR STOP 1
      IF (TYPEX .NE. 5) ERROR STOP 2
      END PROGRAM FIXED_FORM_TYPE_SAVE_01
