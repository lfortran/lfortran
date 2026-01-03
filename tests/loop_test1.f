      program main
      integer KP
      integer i,j,do330i
      kp = 10
      DO 330 I=1,KP
         DO 320, J=I+1,KP
            IF (0.GT.1) THEN
               print *, i, j
            ELSE
               print *, j, i
            END IF
  320    CONTINUE
  330 CONTINUE
      DO330I = 15
      end program
