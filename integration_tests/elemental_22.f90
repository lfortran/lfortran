program elemental_22
  implicit none
  integer :: body(1,2)
  integer :: r(4)
  body = 1
  r = f(body)
  print *, r
  if (r(1) /= 4) error stop
  if (r(2) /= 4) error stop
  if (r(3) /= 4) error stop
  if (r(4) /= 4) error stop
contains
  elemental function g(x) result(r)
    integer, intent(in) :: x
    integer :: r
    r = x
  end function
  pure function f(body) result(lines)
    integer, intent(in) :: body(:,:)
    integer, parameter :: n = 1
    integer :: lines(size(body,1) + rank(body) + n)
    lines = 4
    lines = g(lines)
  end function
end program
