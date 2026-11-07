! Implied-do constructors whose values cannot be evaluated at compile
! time are left unfolded, so that they can still be used where only
! their shape is needed
module implied_do_loops63_m
implicit none
type :: t
    real :: x
end type
type :: ta
    integer :: a(3)
end type
integer :: k
integer, parameter :: ip(3) = [7, 8, 9]
integer, parameter :: n = size([(t(abs(cmplx(k, 1))), k=1,3)])
integer, parameter :: n2 = size([(abs(cmplx(k, 1)), k=1,4)])
integer, parameter :: n3 = size([(ta(ip + k), k=1,2)])
end module

program implied_do_loops63
use implied_do_loops63_m
implicit none
if (n /= 3) error stop
if (n2 /= 4) error stop
if (n3 /= 2) error stop
print *, "ok"
end program
