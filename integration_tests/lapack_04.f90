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
