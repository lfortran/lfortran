C     Minimal reproducer for SORM22 sequence association issue
      SUBROUTINE SORM22_MINIMAL(N1, LEN, N2, Q, LDQ, C, LDC,
     $                          WORK, LDWORK)
      IMPLICIT NONE
      INTEGER N1, LEN, N2, LDQ, LDC, LDWORK
      REAL Q(LDQ, *), C(LDC, *), WORK(*)
      REAL ONE
      PARAMETER (ONE = 1.0E+0)
      INTEGER I
      EXTERNAL SLACPY, STRMM

C     First SLACPY call - WORK is 1D, expects 2D
      CALL SLACPY('All', N1, LEN, C(N2+1, 1), LDC, WORK, LDWORK)

C     STRMM call - WORK is 1D, expects 2D
      CALL STRMM('Left', 'Lower', 'No Transpose', 'Non-Unit',
     $           N1, LEN, ONE, Q(1, N2+1), LDQ, WORK, LDWORK)

C     Second SLACPY call - WORK is 1D, expects 2D
      CALL SLACPY('All', N1, LEN, WORK, LDWORK, C(1, 1), LDC)

      RETURN
      END

      PROGRAM TEST
      IMPLICIT NONE
      INTEGER N1, LEN, N2, LDQ, LDC, LDWORK
      PARAMETER (N1 = 5, LEN = 4, N2 = 3, LDQ = 10, LDC = 10)
      PARAMETER (LDWORK = 10)
      REAL Q(LDQ, LDQ), C(LDC, LDC), WORK(100)

      CALL SORM22_MINIMAL(N1, LEN, N2, Q, LDQ, C, LDC, WORK, LDWORK)
      PRINT *, 'Done'
      END PROGRAM

C     Dummy routines
      SUBROUTINE SLACPY(UPLO, M, N, A, LDA, B, LDB)
      CHARACTER UPLO
      INTEGER M, N, LDA, LDB
      REAL A(LDA, *), B(LDB, *)
      RETURN
      END

      SUBROUTINE STRMM(SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, A, LDA,
     +                  B, LDB)
      CHARACTER SIDE, UPLO, TRANS, DIAG
      INTEGER M, N, LDA, LDB
      REAL ALPHA, A(LDA, *), B(LDB, *)
      RETURN
      END