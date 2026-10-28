      program doloop_real_03
      implicit none
      real x
      integer n
      n = 0
      do x = 1, 3
          n = n + 1
      end do
      if (n /= 3 .or. x /= 4.) error stop
      end program
