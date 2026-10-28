      program doloop_real_02
      implicit none
      real, target :: value
      real, pointer :: p
      real(8), allocatable :: a
      integer n

      p => value
      n = 0
      do p = 1., 3.
          n = n + 1
      end do
      if (n /= 3 .or. value /= 4.) error stop
      if (p /= 4.) error stop

      n = 0
      do p = 3., 1.
          n = n + 1
      end do
      if (n /= 0 .or. value /= 3.) error stop

      allocate(a)
      n = 0
      do a = 1.d0, 3.d0
          n = n + 1
      end do
      if (n /= 3 .or. a /= 4.d0) error stop

      n = 0
      do a = 3.d0, 1.d0
          n = n + 1
      end do
      if (n /= 0 .or. a /= 3.d0) error stop
      deallocate(a)
      end program
