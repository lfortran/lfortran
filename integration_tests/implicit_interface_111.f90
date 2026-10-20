      PROGRAM IMPLICIT_INTERFACE_111
C     Lawson & Hanson style: the user function DIFF (also the name of an
C     LFortran internal intrinsic) is called with an implicit interface,
C     without an EXTERNAL statement, in an arithmetic IF.
      DOUBLE PRECISION DIFF, ONE
      PARAMETER (ONE = 1.0D0)
      IF (DIFF(ONE+1.0D-3,ONE)) 130,130,70
   70 CALL SUB(4.0D0, 1.0D0)
      PRINT *, 'ok'
      STOP
  130 ERROR STOP 1
      END

      SUBROUTINE SUB(X, Y)
C     DIFF and EXP2 are typed only by the IMPLICIT statement.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (DIFF(X,Y) .NE. 3.0D0) ERROR STOP 2
      IF (EXP2(X) .NE. 9.0D0) ERROR STOP 3
      END
