C MRE from LAPACK slals0.f: integer array index into real array
C causes type check assertion: check_equal_type(i32[:], r32[:])
C Pattern: arr(idx(i)) passed to implicit-interface subroutine
      SUBROUTINE TEST
      INTEGER IDX(2)
      REAL ARR(4)

      IDX(1) = 1
      ARR = 1.0

      CALL SUB(ARR(IDX(1)))
      END

      SUBROUTINE SUB(X)
      REAL X(*)
      IF (X(1) .NE. 1.0) STOP 1
      END

      PROGRAM LAPACK_04
      CALL TEST
      PRINT *, 'PASS'
      END
