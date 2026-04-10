      program include_04
      implicit none
      INCLUDE 'vals.inc'
      integer :: z
      z = x + y
      print *, z
      if (z /= 52) error stop
      end program include_04
