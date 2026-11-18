      module fixed_form_contains_02_mod
      implicit none
      contains
      integer function f(x)
      integer, intent(in) :: x
      f = g(x)
      contains
      integer function g(y)
      integer, intent(in) :: y
      g = 2*y
      end function g
      end function f

      pure double precision function h(x)
      double precision, intent(in) :: x
      h = twice(x)
      contains
      pure double precision function twice(y)
      double precision, intent(in) :: y
      twice = 2*y
      end function
      end function

      function k(x)
      integer :: k
      integer, intent(in) :: x
      call add_one(x, k)
      return
      contains
      subroutine add_one(a, b)
      integer, intent(in) :: a
      integer, intent(out) :: b
      b = a + 1
      end subroutine
      end
      end module

      program fixed_form_contains_02
      use fixed_form_contains_02_mod
      implicit none
      print *, f(21), h(21.0d0), k(41)
      if (f(21) /= 42) error stop
      if (abs(h(21.0d0) - 42.0d0) > 1.0d-12) error stop
      if (k(41) /= 42) error stop
      end program
