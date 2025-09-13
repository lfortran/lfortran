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