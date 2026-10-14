! Module arrays of derived types are default initialized, element by element:
! plain, procedure pointer, nested, extended and multi-dimensional cases, and
! a type whose allocatable member is set up at run time (#13649)
module derived_types_230_m
implicit none
type :: base
    integer :: k = 7
    procedure(f), pointer, nopass :: fp => f
end type
type, extends(base) :: ext
    real :: r = 2.5
end type
type :: outer
    type(base) :: b
    integer :: j = 3
end type
type :: mixed
    integer :: k = 4
    integer, allocatable :: x(:)
    procedure(f), pointer, nopass :: fp => g
end type
type(base) :: ma(2)
type(base) :: m2(2, 3)
type(ext) :: me(3)
type(outer) :: mo(2)
type(mixed) :: mx(2)
contains
integer function f(x)
    integer, intent(in) :: x
    f = 2 * x
end function
integer function g(x)
    integer, intent(in) :: x
    g = 3 * x
end function
end module

program derived_types_230
use derived_types_230_m
implicit none
integer :: i, j
do i = 1, 2
    if (ma(i)%k /= 7) error stop
    if (.not. associated(ma(i)%fp)) error stop
    if (ma(i)%fp(3) /= 6) error stop
end do
do j = 1, 3
    do i = 1, 2
        if (m2(i, j)%k /= 7) error stop
        if (m2(i, j)%fp(i + j) /= 2 * (i + j)) error stop
    end do
end do
do i = 1, 3
    if (me(i)%k /= 7) error stop
    if (abs(me(i)%r - 2.5) > 1e-6) error stop
    if (me(i)%fp(5) /= 10) error stop
end do
do i = 1, 2
    if (mo(i)%b%k /= 7) error stop
    if (mo(i)%b%fp(4) /= 8) error stop
    if (mo(i)%j /= 3) error stop
end do
do i = 1, 2
    if (mx(i)%k /= 4) error stop
    if (allocated(mx(i)%x)) error stop
    if (mx(i)%fp(2) /= 6) error stop
end do
ma(1)%k = 1
ma(1)%fp => g
if (ma(2)%k /= 7) error stop
if (ma(2)%fp(3) /= 6) error stop
if (ma(1)%fp(3) /= 9) error stop
print *, ma(1)%k, ma(2)%k, m2(2, 3)%k, me(3)%r, mo(2)%b%k, mx(2)%k
end program
