      program fixed_form_attr_stmt_01
      implicit none
      integer x
      allocatable x
      integer y, z
      allocatable :: y, z
      real a(3)
      target a
      real, pointer :: pa(:)
      integer v
      volatile v
      integer w
      asynchronous w
      integer targetx, allocatablex, optional, intent(2)
      integer r(4)

      allocate(x)
      x = 5
      if (x /= 5) error stop
      allocate(y, z)
      y = 6
      z = 7
      if (y + z /= 13) error stop
      deallocate(x, y, z)

      a = [1.0, 2.0, 3.0]
      pa => a
      pa(2) = 20.0
      if (abs(a(2) - 20.0) > 1e-6) error stop

      v = 3
      w = 4
      if (v + w /= 7) error stop

      targetx = 1
      allocatablex = 2
      optional = 3
      intent(1) = 4
      intent(2) = 5
      if (targetx + allocatablex + optional /= 6) error stop
      if (intent(1) + intent(2) /= 9) error stop

      r = [1, 2, 3, 4]
      call s(2, r)
      if (sum(r) /= 20) error stop
      call s(3, r, 10)
      if (sum(r) /= 60) error stop
      print *, "ok"

      contains

      subroutine s(n, c, m)
      integer n, c(:), m
      intent(in) n, m
      intent(inout) c
      optional m
      contiguous c
      if (present(m)) then
          c = c * n + m - 10
      else
          c = c * n
      end if
      end subroutine
      end program
