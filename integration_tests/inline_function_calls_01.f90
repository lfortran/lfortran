program inline_function_calls_01
  ! With --fast, a function called inside an expression is inlined, which
  ! binds an array dummy to its actual as a pointer: the dummy then has the
  ! bounds of the actual. An assumed-shape dummy has the lower bound it
  ! declares (one by default) instead, so a call whose actual does not start
  ! where its dummy does must not be inlined.
  implicit none
  integer :: y(4), r(4)
  y = [1, 2, 3, 4]
  call s(y, r)
  print *, r
  if (any(r /= [14, 4, 4, 14])) error stop 1
contains
  subroutine s(x, r)
    integer, intent(in) :: x(-1:)
    integer, intent(out) :: r(4)
    r(1) = 0 + f(x)
    r(2) = 0 + g(x)
    r(3) = 0 + h(x)
    r(4) = 0 + f(x(-1:2))
  end subroutine
  integer function f(a) result(r)
    integer, intent(in) :: a(:)
    r = a(4) + 10*lbound(a, 1)
  end function
  integer function g(a) result(r)
    integer, intent(in) :: a(0:)
    r = a(3) + 10*lbound(a, 1)
  end function
  integer function h(a) result(r)
    integer, intent(in) :: a(:)
    r = a(4)
  end function
end program
