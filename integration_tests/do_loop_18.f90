C     The fixed-form program of #15964: a GO TO out of a loop leaves the
C     DO variable, seen through COMMON and EQUIVALENCE, at its value.
         SUBROUTINE SUB(N)
         COMMON /BAR/ I
         EQUIVALENCE(I,J)
         DO 10 J = 1, N
            IF (J == 5) GOTO 11
10       CONTINUE
11       CONTINUE
         END SUBROUTINE

         PROGRAM FOO
         COMMON /BAR/ I
         I = 4
         N = 4 * I
         CALL SUB(N)
         PRINT *, I
         IF (I /= 5) ERROR STOP
         CALL SUB(3)
         PRINT *, I
         IF (I /= 4) ERROR STOP
         END
