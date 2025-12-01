      SUBROUTINE STFSM( TRANSR, SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A,
     $                  B, LDB )
      CHARACTER          TRANSR, DIAG, SIDE, TRANS, UPLO
      INTEGER            LDB, M, N
      REAL               ALPHA
      REAL               A( 0: * ), B( 0: LDB-1, 0: * )
      REAL               ONE, ZERO
      LOGICAL            LOWER, LSIDE, MISODD, NISODD, NORMALTRANSR,
     $                   NOTRANS
      INTEGER            M1, M2, N1, N2, K, INFO, I, J
      IF( ALPHA.EQ.ZERO ) THEN
         IF( MOD( M, 2 ).EQ.0 ) THEN
            IF( NORMALTRANSR ) THEN
               IF( LOWER ) THEN
                  IF( NOTRANS ) THEN
                     IF( M.EQ.1 ) THEN
                        CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA,
     $                              A, M, B, LDB )
                     END IF
                  END IF
                  IF( NOTRANS ) THEN
                     CALL SGEMM( 'N', 'N', M, N2, N1, -ONE, B( 0, 0 ),
     $                           LDB, B( 0, 0 ), LDB, ZERO, B( 0, 0 ), LDB )
                     CALL STRSM( 'R', 'U', 'T', DIAG, M, N1, ONE,
     $                           A( N2*N2 ), N2, B( 0, 0 ), LDB )
                  END IF
               END IF
            END IF
         END IF
      END IF
      END

      SUBROUTINE STRSM( SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, LDA,
     $                  B, LDB )
      CHARACTER          SIDE, UPLO, TRANS, DIAG
      INTEGER            M, N, LDA, LDB
      REAL               ALPHA
      REAL               A( LDA, * ), B( LDB, * )
      RETURN
      END

      SUBROUTINE SGEMM( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                  BETA, C, LDC )
      CHARACTER          TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      REAL               ALPHA, BETA
      REAL               A( LDA, * ), B( LDB, * ), C( LDC, * )
      RETURN
      END

      PROGRAM LAPACK05_MAIN
      CHARACTER          TRANSR, SIDE, UPLO, TRANS, DIAG
      INTEGER            M, N, LDB
      REAL               ALPHA
      INTEGER            LDA
      PARAMETER         ( LDA = 1 )
      REAL               A( 0: LDA-1 ), B( 0: 0, 0: LDA-1 )
      TRANSR = 'N'
      SIDE   = 'L'
      UPLO   = 'L'
      TRANS  = 'N'
      DIAG   = 'N'
      M      = 0
      N      = 0
      LDB    = 1
      ALPHA  = 0.0
      CALL STFSM( TRANSR, SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, B,
     $            LDB )
      END
