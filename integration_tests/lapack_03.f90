C     Minimal reproducer for STRMM sequence association issues
C     Tests multiple cases from LAPACK that fail without proper handling
      PROGRAM STRMM_TEST
      IMPLICIT NONE
      INTEGER LDT, LDV, L, K, N
      PARAMETER (LDT = 10, LDV = 10)
      REAL T(LDT, LDT), V(LDV, LDV), TAU(10)
      REAL NEG_ONE, ONE
      PARAMETER (NEG_ONE = -1.0E+0, ONE = 1.0E+0)
      INTEGER I, J
      EXTERNAL STRMM, SGEMM

C     Initialize
      N = 8
      K = 4
      L = K / 2

C     Initialize arrays
      DO I = 1, LDT
         DO J = 1, LDT
            T(I,J) = 0.0
            V(I,J) = REAL(I + J)
         END DO
         TAU(I) = REAL(I) * 0.1
      END DO

C     DO loop like in LAPACK slarft.f
      DO J = 1, L
         DO I = 1, K-L
            T(J, L+I) = V(L+I, J)
         END DO
      END DO

C     Test 1: Array element passed as array start (works)
      CALL STRMM('Right', 'Lower', 'No transpose', 'Unit', L,
     +           K-L, ONE, V(L+1, L+1), LDV, T(1, L+1), LDT)

C     SGEMM call
      CALL SGEMM('Transpose', 'No transpose', L, K-L, N-K, ONE,
     +           V(K+1, 1), LDV, V(K+1, L+1), LDV, ONE,
     +           T(1, L+1), LDT)

C     Test 2: From slarft.f line 335
C     Whole array T passed where scalar expected
C     Error without fix: expected f32, passed f32[:,:]
      CALL STRMM('Left', 'Upper', 'No transpose', 'Non-unit', L,
     +           K-L, NEG_ONE, T, LDT, T(1, L+1), LDT)

C     Test 3: Another array element case
      CALL STRMM('Right', 'Upper', 'No transpose', 'Non-unit', L,
     +           K-L, ONE, T(L+1, L+1), LDT, T(1, L+1), LDT)

      PRINT *, 'All tests completed successfully'
      END PROGRAM

C     Dummy STRMM implementation - just marks that it was called
      SUBROUTINE STRMM(SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, LDA,
     +                  B, LDB)
      CHARACTER SIDE, UPLO, TRANS, DIAG
      INTEGER M, N, LDA, LDB
      REAL ALPHA, A(LDA, *), B(LDB, *)
C     In real BLAS, this performs triangular matrix multiply
C     For testing, we just verify it can be called
      RETURN
      END

C     Dummy SGEMM implementation - just marks that it was called
      SUBROUTINE SGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                  B, LDB, BETA, C, LDC)
      CHARACTER TRANSA, TRANSB
      INTEGER M, N, K, LDA, LDB, LDC
      REAL ALPHA, BETA, A(LDA, *), B(LDB, *), C(LDC, *)
C     In real BLAS, this performs general matrix multiply
C     For testing, we just verify it can be called
      RETURN
      END