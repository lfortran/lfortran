! An internal procedure declares an automatic array whose extent is a host
! variable, next to host dummy arrays whose bounds are dummies the host
! redefines before the call
module host_assoc_12_m
implicit none
contains
subroutine s(lo, n, a)
integer, intent(in) :: lo
integer :: n
real :: a(lo:n), b(n)
integer :: m
b = 7
m = n
n = 1
call inner()
if (m /= 4) error stop 1
contains
subroutine inner()
real :: w(m)
w = 3
if (size(w) /= 4) error stop 2
if (any(w /= 3)) error stop 3
if (size(a) /= 3) error stop 4
if (lbound(a, 1) /= 2) error stop 5
if (ubound(a, 1) /= 4) error stop 6
if (size(b) /= 4) error stop 7
if (any(b /= 7)) error stop 8
if (abs(a(4) - 3) > 1e-6) error stop 9
a(lo) = 10
end subroutine
end subroutine
end module

program host_assoc_12
use host_assoc_12_m
implicit none
real :: x(5)
integer :: nn
nn = 4
x = [1, 2, 3, 4, 5]
call s(2, nn, x)
if (nn /= 1) error stop 10
if (abs(x(1) - 10) > 1e-6) error stop 11
print *, x
end program
