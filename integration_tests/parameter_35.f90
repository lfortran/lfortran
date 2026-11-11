module parameter_35_mod
implicit none
integer :: i
integer, parameter :: mb(1:*) = [1, 2, 3]
integer, parameter :: ma(1:*) = [10, mb(2:3), mb, 20]
integer, parameter :: mn = size(ma)
real, parameter :: mr(0:*) = [(real(i), i = 1, 4), 9.0]
end module

program parameter_35
use parameter_35_mod
implicit none
real, parameter :: b(1:*) = [1.0, 2.0, 3.0]
real, parameter :: a(1:*) = [10.0, b(1:3), 20.0]
real, parameter :: a2(1:*) = [10.0, b, 20.0]
real, parameter :: c(0:*) = [b(2:3), 5.0, b]
real, parameter :: d(-2:*) = [0.0, b(1:2), (b(1) * i, i = 1, 2)]
real, parameter :: e(*) = [10.0, b(1:3), 20.0]
integer, parameter :: ia(0:*) = [7, mb, 8]
integer, parameter :: n = size(a)
integer, parameter :: m = ubound(c, 1)
real :: z(n)

if (size(a) /= 5 .or. lbound(a, 1) /= 1 .or. ubound(a, 1) /= 5) error stop 1
if (a(1) /= 10.0 .or. a(2) /= 1.0 .or. a(4) /= 3.0 .or. a(5) /= 20.0) error stop 2
if (size(a2) /= 5 .or. a2(5) /= 20.0 .or. a2(3) /= 2.0) error stop 3
if (size(c) /= 6 .or. lbound(c, 1) /= 0 .or. ubound(c, 1) /= 5) error stop 4
if (c(0) /= 2.0 .or. c(2) /= 5.0 .or. c(5) /= 3.0) error stop 5
if (size(d) /= 5 .or. lbound(d, 1) /= -2 .or. ubound(d, 1) /= 2) error stop 6
if (d(-2) /= 0.0 .or. d(-1) /= 1.0 .or. d(0) /= 2.0 .or. d(1) /= 1.0 .or. d(2) /= 2.0) error stop 7
if (size(e) /= 5 .or. lbound(e, 1) /= 1 .or. e(5) /= 20.0) error stop 8
if (size(ia) /= 5 .or. lbound(ia, 1) /= 0 .or. ubound(ia, 1) /= 4) error stop 9
if (ia(0) /= 7 .or. ia(3) /= 3 .or. ia(4) /= 8) error stop 10
if (n /= 5 .or. size(z) /= 5 .or. m /= 5) error stop 11
if (mn /= 7 .or. size(ma) /= 7 .or. ma(7) /= 20 .or. ma(2) /= 2 .or. ma(5) /= 2) error stop 12
if (size(mr) /= 5 .or. lbound(mr, 1) /= 0 .or. ubound(mr, 1) /= 4 .or. mr(4) /= 9.0) error stop 13
if (mb(3) /= 3 .or. ubound(mb, 1) /= 3) error stop 14
call sub()
contains
subroutine sub()
    integer, parameter :: lb(1:*) = [1, 2]
    integer, parameter :: la(1:*) = [0, lb, 5, lb(1:2)]
    if (size(la) /= 6 .or. la(6) /= 2 .or. la(4) /= 5) error stop 15
end subroutine
end program
