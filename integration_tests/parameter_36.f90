module parameter_36_mod
implicit none
real, parameter :: md(*,4:*) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2,3])
end module

program parameter_36
use parameter_36_mod
implicit none
real, parameter :: d(*,4:*) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2,3])
real, parameter :: f(0:*,-1:*) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2,3])
real, parameter :: g(*,*) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2,3])
integer, parameter :: h(2:*,*) = reshape([1, 2, 3, 4, 5, 6], [3,2])
integer, parameter :: n = size(d, 2)
integer, parameter :: u = ubound(d, 2)
integer :: z(n)

if (lbound(d, 1) /= 1 .or. ubound(d, 1) /= 2 .or. lbound(d, 2) /= 4 .or. ubound(d, 2) /= 6) error stop 1
if (d(1,4) /= 1.0 .or. d(2,4) /= 2.0 .or. d(1,5) /= 3.0 .or. d(2,6) /= 6.0) error stop 2
if (lbound(f, 1) /= 0 .or. ubound(f, 1) /= 1 .or. lbound(f, 2) /= -1 .or. ubound(f, 2) /= 1) error stop 3
if (f(0,-1) /= 1.0 .or. f(1,0) /= 4.0 .or. f(1,1) /= 6.0) error stop 4
if (size(g, 1) /= 2 .or. size(g, 2) /= 3 .or. g(2,3) /= 6.0) error stop 5
if (lbound(h, 1) /= 2 .or. ubound(h, 1) /= 4 .or. lbound(h, 2) /= 1 .or. ubound(h, 2) /= 2) error stop 6
if (h(2,1) /= 1 .or. h(4,2) /= 6 .or. h(3,2) /= 5) error stop 7
if (n /= 3 .or. u /= 6 .or. size(z) /= 3 .or. size(d) /= 6) error stop 8
if (lbound(md, 2) /= 4 .or. ubound(md, 2) /= 6 .or. md(2,6) /= 6.0 .or. md(1,5) /= 3.0) error stop 9
end program
