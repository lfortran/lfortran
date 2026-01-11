C Segfault in pass_array_by_data: dimension pointer corruption
C MRE from LAPACK sgbtrf.f compilation failure
      SUBROUTINE TEST
      REAL A( 2, 2 )
      REAL W( 2, 2 )
      EXTERNAL SUB
      CALL SUB( W, A( 1, 1 ) )
      CALL SUB( A( 1, 1 ), W )
      END

      SUBROUTINE SUB( X, Y )
      REAL X( 2, 2 ), Y( 2, 2 )
      X(1,1) = Y(1,1)
      END

      PROGRAM LAPACK_02
      CALL TEST
      PRINT *, 'PASS'
      END
