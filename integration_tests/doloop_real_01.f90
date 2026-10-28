      program doloop_real_01
      implicit none
      real x, first, last, step, total
      real(8) y, total8
      integer n, calls(3), j

      n = 0
      total = 0.
      do x = 0., 1., 0.1
          n = n + 1
          total = total + x
      end do
      if (n /= 11) error stop
      if (abs(total - 5.5) > 1.e-5) error stop
      if (abs(x - 1.1) > 1.e-5) error stop

      first = 1.25
      last = 2.75
      step = 0.5
      calls = 0
      n = 0
      total = 0.
      do x = control(first,1), control(last,2), control(step,3)
          n = n + 1
          total = total + x
          first = 9.
          last = 9.
          step = 9.
      end do
      if (any(calls /= 1)) error stop
      if (n /= 4 .or. abs(total - 8.) > 1.e-6) error stop
      if (abs(x - 3.25) > 1.e-6) error stop

      n = 0
      total8 = 0.d0
      do y = 2.25d0, -0.25d0, -0.5d0
          n = n + 1
          total8 = total8 + y
      end do
      if (n /= 6 .or. abs(total8 - 6.d0) > 1.d-12) error stop
      if (abs(y + 0.75d0) > 1.d-12) error stop

      n = 0
      do x = 2.5, 1., 0.5
          n = n + 1
      end do
      if (n /= 0 .or. abs(x - 2.5) > 1.e-6) error stop
      do y = -2.5d0, 1.d0, -0.5d0
          n = n + 1
      end do
      if (n /= 0 .or. abs(y + 2.5d0) > 1.d-12) error stop

      do x = 1.25, 3.25
          n = n + 1
      end do
      if (n /= 3 .or. abs(x - 4.25) > 1.e-6) error stop

      n = 0
      total = 0.
      outer: do x = 1.25, 3.25, 0.5
          n = n + 1
          do j = 1, 2
              if (n == 2) cycle outer
              if (n == 4) exit outer
          end do
          total = total + x
      end do outer
      if (n /= 4 .or. abs(total - 3.5) > 1.e-6) error stop
      if (abs(x - 2.75) > 1.e-6) error stop

      n = 0
      do y = 1.25d0, 2.25d0, 0.5d0
          n = n + 1
          cycle
      end do
      if (n /= 3 .or. abs(y - 2.75d0) > 1.d-12) error stop

      contains
      real function control(value, which)
      real, intent(in) :: value
      integer, intent(in) :: which
      calls(which) = calls(which) + 1
      control = value
      end function
      end program
