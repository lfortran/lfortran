      PROGRAM FIXED_FORM_NAMELIST_01
      INTEGER A, B, C
      CALL S(A, B, C)
      IF (A /= 1) ERROR STOP
      IF (B /= 2) ERROR STOP
      IF (C /= 3) ERROR STOP
      CALL T(A)
      IF (A /= 12) ERROR STOP
      PRINT *, A, B, C
      END

      SUBROUTINE S(A, B, C)
      INTEGER A, B, C
      CHARACTER(LEN=40) REC
      NAMELIST /N/ A
      NAMELIST /M/ B,
     &             C
      A = 0
      B = 0
      C = 0
      REC = '&N A=1 /'
      READ(REC, NML=N)
      REC = '&M B=2, C=3 /'
      READ(REC, NML=M)
      END

      SUBROUTINE T(K)
      INTEGER K
      INTEGER NAMELIST(2), NAMELISTX
      NAMELIST(1) = 5
      NAMELIST(2) = 3
      NAMELISTX = 4
      K = NAMELIST(1) + NAMELIST(2) + NAMELISTX
      END
