module array_constructor_10_m
implicit none
contains
function t(x) result(l)
logical, intent(in) :: x(:)
integer, allocatable :: l(:)
allocate(l(count(x)))
l = 2
end function
function f(n) result(z)
integer, intent(in) :: n(:)
real :: z(size(n))
z = n
end function
subroutine s(a, c)
integer, intent(in) :: a(:)
real, intent(out) :: c
integer :: r(size(a))
r = a
c = sum([f(t(r >= 0)) - 1.0])
end subroutine
end module
