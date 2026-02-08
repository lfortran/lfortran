C Fixed-form Fortran test for FORMAT statement location tracking
C This test ensures that format validation errors point to the correct line
      program format_56
      implicit none
      integer :: x
      x = 42
      print 100, x
100   format (I5)
      if (x /= 42) error stop
      end program format_56
