! spread() of a scalar complex or derived-type source, at run time and in
! PARAMETER initializers (#13597), and folding spread() of a derived-type
! array constant along dim 1 and 2
program spread_scalar_struct_01
implicit none
type :: w
    integer :: c
    real :: r
end type
type(w) :: s, b(3)
type(w), allocatable :: ba(:)
type(w), parameter :: pb(3) = spread(w(7, 2.5), 1, 3)
complex :: z, zb(2)
complex(8) :: z8
complex(8), allocatable :: z8b(:)
complex, parameter :: pz(3) = spread((1.0, 2.0), 1, 3)
complex(8), parameter :: pz8(2) = spread((3.0d0, -4.0d0), 1, 2)
type(w), parameter :: wa(2) = [w(1, 1.5), w(2, 2.5)]
type(w), parameter :: s1(6) = reshape(spread(wa, 1, 3), [6])
type(w), parameter :: s2(6) = reshape(spread(wa, 2, 3), [6])
type(w) :: r1(3, 2), r2(2, 3)
integer :: n, d, i, j

s = w(7, 2.5)
b = spread(s, 1, 3)
if (any(b%c /= 7)) error stop
if (any(b%r /= 2.5)) error stop

n = 4
d = 1
allocate(ba(n))
ba = spread(s, d, n)
if (size(ba) /= 4) error stop
if (any(ba%c /= 7)) error stop
if (any(ba%r /= 2.5)) error stop

if (size(pb) /= 3) error stop
if (any(pb%c /= 7)) error stop
if (any(pb%r /= 2.5)) error stop

if (any(s1%c /= [1, 1, 1, 2, 2, 2])) error stop
if (any(s1%r /= [1.5, 1.5, 1.5, 2.5, 2.5, 2.5])) error stop
if (any(s2%c /= [1, 2, 1, 2, 1, 2])) error stop
if (any(s2%r /= [1.5, 2.5, 1.5, 2.5, 1.5, 2.5])) error stop
r1 = spread(wa, 1, 3)
r2 = spread(wa, 2, 3)
do j = 1, 2
    do i = 1, 3
        if (r1(i, j)%c /= j) error stop
        if (r2(j, i)%c /= j) error stop
        if (r2(j, i)%r /= wa(j)%r) error stop
    end do
end do

z = (1.0, 2.0)
zb = spread(z, 1, 2)
if (any(zb /= (1.0, 2.0))) error stop
zb = spread((3.0, 4.0), 1, 2)
if (any(zb /= (3.0, 4.0))) error stop

z8 = (5.0d0, 6.0d0)
allocate(z8b(n))
z8b = spread(z8, 1, n)
if (size(z8b) /= 4) error stop
if (any(z8b /= (5.0d0, 6.0d0))) error stop

if (any(pz /= (1.0, 2.0))) error stop
if (any(pz8 /= (3.0d0, -4.0d0))) error stop
print *, b(1)%c, pb(3)%c, zb(1), pz(2), pz8(1)
end program
