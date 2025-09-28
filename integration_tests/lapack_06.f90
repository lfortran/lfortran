C     ACTUAL reproducer for STFSM LLVM 11-16 GEP assertion failure
C     This must actually trigger the assertion to be a valid reproducer
      SUBROUTINE STFSM( TRANSR, SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A,
     $                  B, LDB )
      IMPLICIT NONE
      CHARACTER          TRANSR, DIAG, SIDE, TRANS, UPLO
      INTEGER            LDB, M, N
      REAL               ALPHA
      REAL               A( 0: * ), B( 0: LDB-1, 0: * )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      LOGICAL            LOWER, LSIDE, MISODD, NISODD, NORMALTRANSR,
     $                   NOTRANS
      INTEGER            M1, M2, N1, N2, K, INFO, I, J
      LOGICAL            LSAME
      EXTERNAL           LSAME, SGEMM, STRSM, XERBLA

C     Simplified version of actual stfsm.f logic that triggers the bug
      NORMALTRANSR = LSAME( TRANSR, 'N' )
      LSIDE = LSAME( SIDE, 'L' )
      LOWER = LSAME( UPLO, 'L' )
      NOTRANS = LSAME( TRANS, 'N' )

      IF( LSIDE ) THEN
         NISODD = ( MOD( N, 2 ).NE.0 )
         IF( .NOT.NISODD ) THEN
            K = N / 2
         ELSE
            IF( LOWER ) THEN
               N2 = N / 2
               N1 = N - N2
            ELSE
               N1 = N / 2
               N2 = N - N1
            END IF
         END IF
      ELSE
         MISODD = ( MOD( M, 2 ).NE.0 )
         IF( .NOT.MISODD ) THEN
            K = M / 2
         ELSE
            IF( LOWER ) THEN
               M2 = M / 2
               M1 = M - M2
            ELSE
               M1 = M / 2
               M2 = M - M1
            END IF
         END IF
      END IF

C     The specific pattern from stfsm.f that should trigger LLVM 11 assertion
      IF( LSIDE ) THEN
         IF( NISODD ) THEN
            IF( NORMALTRANSR ) THEN
               IF( LOWER ) THEN
                  IF( NOTRANS ) THEN
                     IF( M.EQ.1 ) THEN
                        CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA,
     $                              A, M, B, LDB )
                     ELSE
                        CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA,
     $                              A( 0 ), M, B, LDB )
                        CALL SGEMM( 'N', 'N', M2, N, M1, -ONE, A( M1 ),
     $                              M, B, LDB, ALPHA, B( M1, 0 ), LDB )
                        CALL STRSM( 'L', 'U', 'T', DIAG, M2, N, ONE,
     $                              A( M ), M, B( M1, 0 ), LDB )
                     END IF
                  END IF
               END IF
            END IF
         ELSE
C           N is even case with the problematic K+1 patterns
            IF( NORMALTRANSR ) THEN
               IF( LOWER ) THEN
                  IF( NOTRANS ) THEN
                     CALL STRSM( 'L', 'L', 'N', DIAG, K, N, ALPHA,
     $                           A( 1 ), M+1, B, LDB )
                     CALL SGEMM( 'N', 'N', K, N, K, -ONE, A( K+1 ),
     $                           M+1, B, LDB, ALPHA, B( K, 0 ), LDB )
                     CALL STRSM( 'L', 'U', 'T', DIAG, K, N, ONE,
     $                           A( 0 ), M+1, B( K, 0 ), LDB )
                  END IF
               END IF
            END IF
         END IF
      END IF

      RETURN
      END

      PROGRAM TEST
      IMPLICIT NONE
      INTEGER M, N, LDB
      PARAMETER (M = 6, N = 4, LDB = 8)
      REAL A(100), B(0:LDB-1, 0:N-1), ALPHA
      ALPHA = 1.0

      CALL STFSM('N', 'L', 'L', 'N', 'N', M, N, ALPHA, A, B, LDB)
      PRINT *, 'Done'
      END PROGRAM

C     Required function
      LOGICAL FUNCTION LSAME( CA, CB )
      CHARACTER          CA, CB
      LSAME = ( CA.EQ.CB ) .OR.
     $        ( CA.EQ.CHAR( ICHAR( CB )+32 ) ) .OR.
     $        ( CB.EQ.CHAR( ICHAR( CA )+32 ) )
      RETURN
      END

C     Dummy BLAS routines
      SUBROUTINE STRSM(SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, LDA,
     $                 B, LDB)
      CHARACTER SIDE, UPLO, TRANS, DIAG
      INTEGER M, N, LDA, LDB
      REAL ALPHA, A(LDA, *), B(LDB, *)
      RETURN
      END

      SUBROUTINE SGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     $                 B, LDB, BETA, C, LDC)
      CHARACTER TRANSA, TRANSB
      INTEGER M, N, K, LDA, LDB, LDC
      REAL ALPHA, BETA, A(LDA, *), B(LDB, *), C(LDC, *)
      RETURN
      END