       REAL U, S 
       DIMENSION U(100) 
       S = 0.0 
       INTEGER J
       DO 1 J = 1, 100 
              S = S + U(J) 
              IF ( S .GE. 1000000 ) GO TO 2 
1      CONTINUE 
       STOP 
2      CONTINUE 
       END
