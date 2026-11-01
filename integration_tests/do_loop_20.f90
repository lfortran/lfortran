C     A GO TO from the range of a DO loop to the terminal statement that
C     it shares with the DO loops nested in it ends the iteration of that
C     loop (a legacy idiom, as GFortran has it).
      PROGRAM DO_LOOP_20
      INTEGER I, J, L, K, N
C     From the outer loop, past the inner one.
      K = 0
      DO 10 I = 1, 3
      IF (I .EQ. 2) GO TO 10
      DO 10 J = 1, 4
      K = K + 1
   10 CONTINUE
      IF (K .NE. 8) ERROR STOP
      IF (I .NE. 4) ERROR STOP
C     From the middle of three loops.
      K = 0
      DO 20 I = 1, 2
      DO 20 J = 1, 3
      IF (J .EQ. 2) GO TO 20
      DO 20 L = 1, 2
      K = K + 1
   20 CONTINUE
      IF (K .NE. 8) ERROR STOP
      IF (I .NE. 3) ERROR STOP
      IF (J .NE. 4) ERROR STOP
C     From a DO WHILE loop inside the outer loop, which the GO TO leaves.
      K = 0
      DO 30 I = 1, 3
      N = 0
      DO WHILE (N .LT. 2)
         N = N + 1
         IF (I .EQ. 2) GO TO 30
      END DO
      DO 30 J = 1, 2
      DO 30 L = 1, 2
      K = K + 1
   30 CONTINUE
      IF (K .NE. 8) ERROR STOP
      IF (I .NE. 4) ERROR STOP
      IF (N .NE. 2) ERROR STOP
      PRINT *, 'ok'
      END
