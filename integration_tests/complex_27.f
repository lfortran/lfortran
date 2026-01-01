      program complex_27
      implicit none

      complex*16         zero, one, z
      parameter          ( zero = ( 0.0E+0, 0.0E+0 ),
     $                     one  = ( 1.0E+0, 0.0E+0 ) )

      z = one + zero

      if (dabs(dble(z)-1.0d0) .gt. 1.0d-12) stop 1
      if (dabs(dimag(z)) .gt. 1.0d-12) stop 2

      print *, 'PASS'
      end
