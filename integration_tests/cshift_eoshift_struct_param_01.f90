! cshift() and eoshift() of a derived-type array constant in PARAMETER
! initializers, with a literal and a named-constant boundary and with shifts
! larger than the array
program cshift_eoshift_struct_param_01
implicit none
type :: w
    integer :: c
    real :: r
end type
type(w), parameter :: a(3) = [w(1, 1.5), w(2, 2.5), w(3, 3.5)]
type(w), parameter :: bnd = w(9, 9.5)
type(w), parameter :: c1(3) = cshift(a, 1)
type(w), parameter :: c2(3) = cshift(a, -1)
type(w), parameter :: c3(3) = cshift([w(4, 4.5), w(5, 5.5), w(6, 6.5)], 2)
type(w), parameter :: e1(3) = eoshift(a, 1, boundary=w(8, 8.5))
type(w), parameter :: e2(3) = eoshift(a, -2, bnd)
type(w), parameter :: e3(3) = eoshift(a, 1, w(7, 7.5))
type(w), parameter :: c4(3) = cshift(a, -5)
type(w), parameter :: e4(3) = eoshift(a, 4, w(0, 0.5))

if (any(c1%c /= [2, 3, 1])) error stop
if (any(c1%r /= [2.5, 3.5, 1.5])) error stop
if (any(c2%c /= [3, 1, 2])) error stop
if (any(c3%c /= [6, 4, 5])) error stop
if (any(c3%r /= [6.5, 4.5, 5.5])) error stop
if (any(e1%c /= [2, 3, 8])) error stop
if (any(e1%r /= [2.5, 3.5, 8.5])) error stop
if (any(e2%c /= [9, 9, 1])) error stop
if (any(e2%r /= [9.5, 9.5, 1.5])) error stop
if (any(e3%c /= [2, 3, 7])) error stop
if (any(c4%c /= [2, 3, 1])) error stop
if (any(e4%c /= 0)) error stop
if (any(e4%r /= 0.5)) error stop
print *, c1%c, c3%c, e1%c, e2%c, e3%c
end program
