      SUBROUTINE S(X, K)
      REAL X(..)
      INTEGER K
      R: SELECT RANK (X)
      RANK (1) R
         K = 1
      RANK DEFAULT R
         K = 2
      END SELECT R
      END SUBROUTINE

      PROGRAM FIXED_FORM_NAMED_CONSTRUCT_02
      INTERFACE
      SUBROUTINE S(X, K)
      REAL X(..)
      INTEGER K
      END SUBROUTINE
      END INTERFACE
      INTEGER K
      REAL A(3), B(2,2)
      CALL S(A, K)
      IF (K .NE. 1) ERROR STOP 1
      CALL S(B, K)
      IF (K .NE. 2) ERROR STOP 2
      PRINT *, 'ok'
      END
