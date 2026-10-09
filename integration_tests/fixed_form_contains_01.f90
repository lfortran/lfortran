      subroutine outer(r)
      implicit none
      integer, intent(out) :: r
      call inner(r)
      r = r + twice(1)
      contains
      subroutine inner(x)
      integer, intent(out) :: x
      x = 40
      end subroutine inner
      integer function twice(y)
      integer, intent(in) :: y
      twice = 2*y
      end function
      end subroutine

      subroutine outer2(r)
      implicit none
      integer, intent(out) :: r
      call inner2()
      contains
      subroutine inner2()
      r = 7
      end subroutine
      end subroutine

      program fixed_form_contains_01
      implicit none
      integer :: r
      call outer(r)
      print *, r
      if (r /= 42) error stop
      call outer2(r)
      print *, r
      if (r /= 7) error stop
      end program
