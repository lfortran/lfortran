module legacy_array_sections_22_mod
implicit none
contains
subroutine foo(n, x)
integer, intent(in) :: n
double precision, intent(in) :: x(n)
if (n >= 1) then
    if (abs(x(1) - 2.0d0) > 1.0d-12) error stop
end if
if (n >= 2) then
    if (abs(x(2) - 3.0d0) > 1.0d-12) error stop
end if
end subroutine
end module

program legacy_array_sections_22
use legacy_array_sections_22_mod
double precision :: w(10)
integer :: i
do i = 1, 10
    w(i) = dble(i)
end do
call foo(3, w(2))
print *, "PASSED"
end program
