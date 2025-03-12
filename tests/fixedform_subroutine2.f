      PROGRAM SUBR2
      CALL MY_SUBR()
      END

      SUBROUTINE MY_SUBR()
      PRINT *, SXVALS()
      IF (SXVALS() /= 4) ERROR STOP
      RETURN

      CONTAINS
      INTEGER FUNCTION SXVALS()
      SXVALS = 4
      RETURN
      END

      END
