C     Minimal reproducer for GEP pointer error (Issue #1232)
C     Tests Fortran77 ABI: passing explicit-size array to external subroutine
C     Before fix: Invalid GEP indices (getelementptr ptr, ptr %arr, i32 0, i32 0)
C     After fix: Direct pointer pass (no GEP needed in Fortran77 ABI)
      PROGRAM test_gep_fix
      INTEGER SEED(4)
      SEED(1) = 1
      SEED(2) = 2
      SEED(3) = 3
      SEED(4) = 4
      CALL CALLER(SEED)
      PRINT *, 'Done'
      END

      SUBROUTINE CALLER(ARR)
      INTEGER ARR(4)
      CALL EXTERNAL_SUB(ARR)
      END

      SUBROUTINE EXTERNAL_SUB(X)
      INTEGER X(4)
      IF (X(1) .NE. 1) ERROR STOP
      IF (X(2) .NE. 2) ERROR STOP
      IF (X(3) .NE. 3) ERROR STOP
      IF (X(4) .NE. 4) ERROR STOP
      END
