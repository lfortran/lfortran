      PROGRAM DOLOOP_SHARED_TERM_BRANCH_02
C     Fixed form: a branch to the shared terminal statement of nested DO
C     loops from the outer loop gets a warning, from the innermost loop
C     it does not.
      IMPLICIT NONE
      INTEGER I, J, K, N
      N = 0
      DO 10 I = 1, 3
      IF (I .EQ. 2) GO TO 10
      DO 10 J = 1, 2
      N = N + 1
   10 CONTINUE
      DO 20 I = 1, 2
      DO 20 J = 1, 2
      IF (J .EQ. 2) GOTO 20
      DO 20 K = 1, 2
   20 N = N + 1
      DO 30 I = 1, 2
      DO 30 J = 1, 2
      IF (J .EQ. 2) GO TO 30
      N = N + 1
   30 CONTINUE
      PRINT *, N
      END
