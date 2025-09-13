C     Minimal reproducer for LAPACK slarft.f sequence association issue
C     V(2,2) should be treated as a 2D array starting at that element
C     per Fortran sequence association rules with implicit interfaces
      RECURSIVE SUBROUTINE SUB(N, V)
      IMPLICIT NONE
      INTEGER            N
      REAL               V(N, *)

      IF(N.LE.1) RETURN
      CALL SUB(N/2, V)
      CALL SUB(N/2, V(2, 2))
      END SUBROUTINE

C     Test case for STRMM call issue from line 335 of slarft.f
C     Error: expected f32, passed f32[:,:]
      RECURSIVE SUBROUTINE SLARFT2(DIRECT, STOREV, N, K, V, LDV,
     +                              TAU, T, LDT)
      IMPLICIT NONE
      CHARACTER DIRECT, STOREV
      INTEGER N, K, LDV, LDT
      REAL V(LDV, *), TAU(*), T(LDT, *)
      REAL NEG_ONE, ONE
      PARAMETER (NEG_ONE = -1.0E+0, ONE = 1.0E+0)
      INTEGER L, I, J
      LOGICAL LSAME
      EXTERNAL STRMM, SGEMM, LSAME

      IF(K.LE.1) THEN
         T(1,1) = TAU(1)
         RETURN
      END IF

      L = K / 2

      IF(LSAME(DIRECT,'F').AND.LSAME(STOREV,'C')) THEN
C        QR case like in LAPACK
         CALL SLARFT2(DIRECT, STOREV, N, L, V, LDV, TAU, T, LDT)
         CALL SLARFT2(DIRECT, STOREV, N-L, K-L, V(L+1,L+1), LDV,
     +                TAU(L+1), T(L+1,L+1), LDT)

C        DO loop like in LAPACK
         DO J = 1, L
            DO I = 1, K-L
               T(J, L+I) = V(L+I, J)
            END DO
         END DO

C        First STRMM call
         CALL STRMM('Right', 'Lower', 'No transpose', 'Unit', L,
     +              K-L, ONE, V(L+1, L+1), LDV, T(1, L+1), LDT)

C        SGEMM call
         CALL SGEMM('Transpose', 'No transpose', L, K-L, N-K, ONE,
     +              V(K+1, 1), LDV, V(K+1, L+1), LDV, ONE,
     +              T(1, L+1), LDT)

C        Second STRMM call - this is line 335 where error occurs!
         CALL STRMM('Left', 'Upper', 'No transpose', 'Non-unit', L,
     +              K-L, NEG_ONE, T, LDT, T(1, L+1), LDT)

C        Third STRMM call
         CALL STRMM('Right', 'Upper', 'No transpose', 'Non-unit', L,
     +              K-L, ONE, T(L+1, L+1), LDT, T(1, L+1), LDT)
      END IF
      END SUBROUTINE