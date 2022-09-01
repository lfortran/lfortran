      program main 
      integer KP
      kp =10
      integer i,j
      DO 330 I=1,KP
         DO 320 J=I+1,KP
            IF (0.GT.1) THEN
               print *, i, j
            ELSE
               print *, j, i
            END IF
  320    CONTINUE
  330 CONTINUE
      end program
