program realloc_lhs_28
! Intrinsic assignment to an unallocated allocatable array from an array
! expression that is not a whole array allocates it with lower bounds of 1
implicit none
type :: t
    real :: c = 0.0
    real, allocatable :: a(:)
end type
real, allocatable :: r(:), z(:), m(:,:), w(:,:)
real :: im(3)
type(t) :: s, x(0:2)

allocate(r(0:2))
r = [4.0, 9.0, 16.0]
im = 1.0

z = r
if (lbound(z, 1) /= 0) error stop
deallocate(z)

z = sqrt(r)
if (lbound(z, 1) /= 1) error stop
if (ubound(z, 1) /= 3) error stop
if (any(abs(z - [2.0, 3.0, 4.0]) > 1e-6)) error stop
deallocate(z)

z = r + 1.0
if (lbound(z, 1) /= 1) error stop
if (abs(z(3) - 17.0) > 1e-6) error stop
deallocate(z)

z = -r
if (lbound(z, 1) /= 1) error stop
if (abs(z(1) + 4.0) > 1e-6) error stop
deallocate(z)

z = r + im
if (lbound(z, 1) /= 1) error stop
if (abs(z(2) - 10.0) > 1e-6) error stop
deallocate(z)

allocate(s%a(0:2))
s%a = [1.0, 2.0, 3.0]
z = s%a
if (lbound(z, 1) /= 0) error stop
deallocate(z)

z = s%a * 2.0
if (lbound(z, 1) /= 1) error stop
if (abs(z(3) - 6.0) > 1e-6) error stop
deallocate(z)

x%c = [5.0, 6.0, 7.0]
z = x%c
if (lbound(z, 1) /= 1) error stop
if (abs(z(1) - 5.0) > 1e-6) error stop
deallocate(z)

allocate(m(0:1, -1:0))
m = 1.5
w = m
if (any(lbound(w) /= [0, -1])) error stop
deallocate(w)

w = m * 2.0
if (any(lbound(w) /= [1, 1])) error stop
if (any(ubound(w) /= [2, 2])) error stop
if (any(abs(w - 3.0) > 1e-6)) error stop

print *, "ok"
end program
