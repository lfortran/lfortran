! Array constructors mixing scalars with run-time-sized array elements
! (compiled with -fdefault-integer-8, see array_constructor_13 for
! --descriptor-index-64)
module array_constructor_14_mod
implicit none
type :: d_t
    real, allocatable :: v(:)
end type
contains
function f2() result(res)
    integer(8), allocatable :: res(:)
    allocate(res(2))
    res = [3_8, 1_8]
end function

subroutine midpoints(x, y)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    integer :: n
    n = size(x)
    y = [x(1), 0.5*(x(1:n-1) + x(2:n)), x(n)]
end subroutine
end module

program array_constructor_14
use array_constructor_14_mod
implicit none
real, allocatable :: t(:), x(:)
real :: a4(4), a5(5)
integer :: arr(3), r(3), i, n
integer, allocatable :: idx(:)
type(d_t) :: d

allocate(t(2))
t = [3.0, 4.0]
n = 2

! allocatable variable next to scalars
a4 = [1.0, t, 2.0]
if (any(abs(a4 - [1.0, 3.0, 4.0, 2.0]) > 1e-6)) error stop 1

! array expression next to scalars
call midpoints([1.0, 2.0, 3.0, 4.0], a5)
if (any(abs(a5 - [1.0, 1.5, 2.5, 3.5, 4.0]) > 1e-6)) error stop 2

! vector subscript with an allocatable index array
arr = [10, 20, 30]
allocate(idx(2))
idx = [3, 1]
r = [arr(idx), 5]
if (any(r /= [30, 10, 5])) error stop 3

! vector subscript with an integer(8) function result
r = [arr(f2()), 5]
if (any(r /= [30, 10, 5])) error stop 4

! allocatable derived-type component next to scalars
allocate(d%v(2))
d%v = [5.0, 6.0]
a4 = [1.0, d%v, 2.0]
if (any(abs(a4 - [1.0, 5.0, 6.0, 2.0]) > 1e-6)) error stop 5

! allocatable variable next to a strided section
a4 = [t, t(1:n:1)]
if (any(abs(a4 - [3.0, 4.0, 3.0, 4.0]) > 1e-6)) error stop 6

! implied-do over an allocatable next to a scalar
a5 = [1.0, (t, i = 1, 2)]
if (any(abs(a5 - [1.0, 3.0, 4.0, 3.0, 4.0]) > 1e-6)) error stop 7

! append idiom
x = [1.0, 2.0, 3.0]
x = [x, 4.0]
if (size(x) /= 4) error stop 9
if (any(abs(x - [1.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop 10

print *, "ok"
end program
