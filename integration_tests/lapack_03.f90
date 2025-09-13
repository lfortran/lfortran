C     Minimal reproducer for LAPACK slarft.f sequence association issue
C     V(2,2) should be treated as a 2D array starting at that element
C     per Fortran sequence association rules with implicit interfaces
      RECURSIVE SUBROUTINE SUB(N, V)
      IMPLICIT NONE
      INTEGER            N
      REAL               V(N, *)

      CALL SUB(N, V)
      CALL SUB(N, V(2, 2))
      END SUBROUTINE