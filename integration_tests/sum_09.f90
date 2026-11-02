! MAXVAL and MINVAL accumulated in a loop call an impure elemental function
! in their operand once per element. Not registered for GFortran: GFortran
! <= 13 evaluates the operand twice for each element that becomes the new
! extremum (the standard requires one evaluation per element; GFortran 15+
! is correct).
module sum_09_mod
implicit none

integer :: nelemental = 0

contains

    impure elemental real function twice(x)
        real, intent(in) :: x
        nelemental = nelemental + 1
        twice = 2 * x
    end function

end module

program sum_09
use sum_09_mod
implicit none
integer, parameter :: n = 7
real :: a(n), b(n), s
integer :: i

do i = 1, n
    a(i) = real(mod(3 * i, n)) / 2 - 1
    b(i) = real(i) / 4
end do

nelemental = 0
s = maxval(twice(a) + 1)
if (nelemental /= n) error stop 1
if (s /= 2 * maxval(a) + 1) error stop 2
nelemental = 0
s = minval(twice(a(2:n)) - b(2:n))
if (nelemental /= n - 1) error stop 3

print *, "ok"
end program
