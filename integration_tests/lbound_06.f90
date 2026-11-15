! A scalar component selected out of an array, such as pts%x, is not a
! whole array, so its lower bounds are 1 whatever the bounds of pts are.
program lbound_06
implicit none
type :: lbound_06_pt
    integer :: x
    integer :: y
end type
type :: lbound_06_holder
    type(lbound_06_pt) :: arr(0:2)
end type
type(lbound_06_pt), target :: pts(0:2)
type(lbound_06_pt), target :: grid(-1:1, 2:3)
type(lbound_06_pt), allocatable, target :: pa(:)
type(lbound_06_pt), parameter :: cp(0:2) = [lbound_06_pt(1, 2), &
    lbound_06_pt(3, 4), lbound_06_pt(5, 6)]
type(lbound_06_holder) :: h
integer, pointer :: xs(:)

pts%x = [4, 5, 6]
pts%y = 0
if (lbound(pts%x, 1) /= 1) error stop 1
if (ubound(pts%x, 1) /= 3) error stop 2
if (any(lbound(pts%x) /= [1])) error stop 3
xs => pts%x
if (lbound(xs, 1) /= 1 .or. ubound(xs, 1) /= 3) error stop 4
if (xs(1) /= 4 .or. xs(3) /= 6) error stop 5

grid%x = 0
if (any(lbound(grid%x) /= [1, 1])) error stop 6
if (any(ubound(grid%x) /= [3, 2])) error stop 7
if (lbound(grid%x, 2) /= 1) error stop 8

allocate(pa(-3:-1))
pa%x = [7, 8, 9]
xs => pa%x
if (lbound(xs, 1) /= 1 .or. ubound(xs, 1) /= 3) error stop 9
if (xs(1) /= 7) error stop 10

if (lbound(cp%x, 1) /= 1 .or. ubound(cp%x, 1) /= 3) error stop 11

h%arr%x = [1, 2, 3]
if (lbound(h%arr, 1) /= 0) error stop 12
if (lbound(h%arr%x, 1) /= 1 .or. ubound(h%arr%x, 1) /= 3) error stop 13

associate (a => pts%x)
    if (lbound(a, 1) /= 1 .or. ubound(a, 1) /= 3) error stop 14
    if (a(1) /= 4) error stop 15
end associate

call check_dummy(pts)
print *, "OK"

contains

subroutine check_dummy(d)
    type(lbound_06_pt), intent(in), target :: d(0:)
    integer, pointer :: q(:)
    if (lbound(d%x, 1) /= 1 .or. ubound(d%x, 1) /= 3) error stop 16
    q => d%x
    if (lbound(q, 1) /= 1 .or. ubound(q, 1) /= 3) error stop 17
    if (q(1) /= 4) error stop 18
end subroutine

end program
