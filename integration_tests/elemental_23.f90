program elemental_23
  implicit none
  integer :: body(1,2)
  integer, allocatable :: r(:)
  body = 1
  r = f(body)
  if (size(r) /= 4) error stop
  if (any(r /= 1)) error stop
contains
  elemental function g(x, y) result(r)
    integer, intent(in) :: x
    integer, intent(in), optional :: y
    integer :: r
    r = x
  end function
  pure function f(body) result(lines)
    integer, intent(in) :: body(:,:)
    integer, parameter :: n = 1
    integer :: lines(size(body,1) + rank(body) + n)
    lines = 1
    lines = g(lines, 1)
  end function
end program
