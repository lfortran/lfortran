      program main 
      integer KP
      kp =10
<<<<<<< HEAD
=======
      integer i,j
>>>>>>> gh-main/main
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
