      SUBROUTINE CALLEE(SEED)
      INTEGER SEED(*)
      SEED(1) = SEED(1) + 1
      RETURN
      END

      SUBROUTINE CALLER(SEED)
      INTEGER SEED(4)
      CALL CALLEE(SEED)
      RETURN
      END

      PROGRAM LEGACY_ASSUME
      INTEGER SEED(4)
      SEED(1) = 1
      SEED(2) = 2
      SEED(3) = 3
      SEED(4) = 4
      CALL CALLER(SEED)
      IF (SEED(1) .NE. 2) THEN
         STOP 1
      ENDIF
      PRINT *, 'legacy assumed-size passthrough ok'
      END
