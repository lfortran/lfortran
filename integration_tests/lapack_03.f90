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
      RECURSIVE SUBROUTINE SLARFT2(N, K, T, LDT)
      IMPLICIT NONE
      INTEGER N, K, LDT
      REAL T(LDT, *)
      REAL NEG_ONE
      PARAMETER (NEG_ONE = -1.0E+0)
      INTEGER L
      EXTERNAL STRMM

      IF(K.LE.1) RETURN
      L = K / 2

      CALL SLARFT2(N, L, T, LDT)
      CALL SLARFT2(N-L, K-L, T(L+1, L+1), LDT)

      CALL STRMM('Left', 'Upper', 'No transpose', 'Non-unit', L,
     +           K-L, NEG_ONE, T, LDT, T(1, L+1), LDT)
      END SUBROUTINE