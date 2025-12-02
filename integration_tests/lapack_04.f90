C MRE: implicit-interface array physical type mismatch from LAPACK SLALS0
      SUBROUTINE SLALS0( NRHS, B, LDB, BX, LDBX, PERM, K, WORK )
      INTEGER K, LDB, LDBX, NRHS, I, PERM(*)
      REAL B(LDB,*), BX(LDBX,*), WORK(*)
      EXTERNAL SCOPY, SGEMV

      CALL SCOPY( NRHS, B, LDB, BX, LDBX )
      CALL SGEMV( 'T', K, NRHS, 1.0, B, LDB, WORK, 1, 0.0,
     $            BX( 1, 1 ), LDBX )

      DO 90 I = 1, K
         CALL SCOPY( NRHS, BX( I, 1 ), LDBX, B( PERM( I ), 1 ), LDB )
   90 CONTINUE

      END

      SUBROUTINE SCOPY( N, X, INCX, Y, INCY )
      INTEGER N, INCX, INCY
      REAL X(*), Y(*)
C Stub implementation - just to make test compile and run
      END

      SUBROUTINE SGEMV( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                  BETA, Y, LDY )
      CHARACTER TRANS
      INTEGER M, N, LDA, INCX, LDY
      REAL ALPHA, BETA
      REAL A(LDA,*), X(*), Y(*)
C Stub implementation - just to make test compile and run
      END

      PROGRAM LAPACK_04
      INTEGER PERM(2)
      REAL B(3,2), BX(3,2), WORK(2)
      PERM(1) = 1
      PERM(2) = 2
      CALL SLALS0( 2, B, 3, BX, 3, PERM, 2, WORK )
      PRINT *, 'PASS'
      END
