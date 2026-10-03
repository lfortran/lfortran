program realloc_lhs_24
  implicit none
  integer, allocatable :: a(:)
  allocate(a(2))
  a(1) = 10
  a(2) = 20
  a = [f(), a(1:2)]

  if (size(a) /= 5) error stop
  if (a(1) /= 1) error stop
  if (a(2) /= 2) error stop
  if (a(3) /= 3) error stop
  if (a(4) /= 10) error stop
  if (a(5) /= 20) error stop
contains
  function f() result(r)
    integer :: r(3)
    r = [1, 2, 3]
  end function f
end program realloc_lhs_24
