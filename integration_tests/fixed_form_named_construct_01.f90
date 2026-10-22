      PROGRAM FIXED_FORM_NAMED_CONSTRUCT_01
      INTEGER MODE, I, K, BLOCK
      REAL A(3)

      K = 0
      DO 10 MODE = 0, 2
      MODE0: IF (MODE .EQ. 0) THEN
         K = K + 1
      ELSE IF (MODE .EQ. 1) THEN MODE0
         K = K + 10
      ELSE MODE0
         K = K + 100
      ENDIF MODE0
   10 CONTINUE
      IF (K .NE. 111) ERROR STOP 1

      K = 0
      DO 20 I = 1, 3
      SEL: SELECT CASE (I)
      CASE (1) SEL
         K = K + 1
      CASE (2)
         K = K + 10
      CASE DEFAULT SEL
         K = K + 100
      END SELECT SEL
   20 CONTINUE
      IF (K .NE. 111) ERROR STOP 2

      A = [1.0, -1.0, 2.0]
      WH: WHERE (A .GT. 0.0)
         A = 2.0
      ELSEWHERE
         A = 0.0
      END WHERE WH
      IF (ANY(A .NE. [2.0, 0.0, 2.0])) ERROR STOP 3

      K = 0
      BLOCK
         INTEGER J
         J = 5
         K = K + J
      END BLOCK
      IF (K .NE. 5) ERROR STOP 4

      CLEANUP: BLOCK
         K = K + 1
         IF (K .GT. 0) EXIT CLEANUP
         K = -1
      END BLOCK CLEANUP
      IF (K .NE. 6) ERROR STOP 5

      OUTER: DO CONCURRENT (I = 1:3)
         A(I) = REAL(I)
      END DO OUTER
      IF (ANY(A .NE. [1.0, 2.0, 3.0])) ERROR STOP 6

      BLOCK = 1
      IFX: IF (BLOCK .EQ. 2) THEN
         K = -1
      ELSE IFX
         BLOCK
            K = BLOCK
         END BLOCK
      END IF IFX
      IF (K .NE. 1) ERROR STOP 7

      PRINT *, 'ok'
      END
