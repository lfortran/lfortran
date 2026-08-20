      program main
      implicit none
      integer, parameter :: n = 2
      real :: x(n, n)
      call a1(n, x)
      if (abs(x(1,1) - 0.0) > 1e-5) error stop 1
      if (abs(x(2,2) - 0.0) > 1e-5) error stop 2
      end program main

      subroutine a1(n, x)
      integer n, i, j
      real x(n,n)
      do i = 1, n
         do j = 1, n
            x(i,j) = 0.0
         end do; end do
      end subroutine a1
