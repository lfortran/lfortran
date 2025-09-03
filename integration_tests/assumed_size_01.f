      PROGRAM SBLAT1
      END

      SUBROUTINE CHECK0()
      REAL              DATRUE(1)
      EXTERNAL          STEST1
      DATRUE(1) = 0.5E0
      CALL STEST1(DATRUE(1))
      RETURN
      END

      SUBROUTINE STEST(LEN,SSIZE)
      INTEGER          LEN
      REAL             SSIZE(LEN)
      RETURN
      END


      SUBROUTINE STEST1(SSIZE)
      REAL              SSIZE(*)
      EXTERNAL          STEST
      CALL STEST(1,SSIZE)
      RETURN
      END