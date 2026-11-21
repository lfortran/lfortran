! Pointer declaration initializers whose target is an array element with
! constant subscripts: elements of derived-type arrays at module, program and
! procedure scope, as the default of a pointer component, and an element of an
! intrinsic-type array.
module global_init_08_m
implicit none

type :: grid_dt
    integer :: size = 5
end type

type :: holder
    type(grid_dt) :: grids(2)
end type

type(grid_dt), target :: uarr(4)
type(grid_dt), save, target :: grid2(2, 3)
type(grid_dt), save, target :: shifted(-1:1)
type(holder), save, target :: h

type(grid_dt), pointer :: q => uarr(3)
type(grid_dt), pointer :: q2 => grid2(2, 3)
type(grid_dt), pointer :: q_shifted => shifted(-1)
type(grid_dt), pointer :: q_comp => h%grids(2)

type :: view
    type(grid_dt), pointer :: first => uarr(1)
end type

contains

subroutine bump(expected)
    integer, intent(in) :: expected
    type(grid_dt), save, target :: local(3)
    type(grid_dt), pointer, save :: lp => local(2)
    if (.not. associated(lp, local(2))) error stop 20
    if (local(2)%size /= expected) error stop 21
    lp%size = lp%size + 1
end subroutine

end module

program global_init_08
use global_init_08_m
implicit none

integer, parameter :: last = 3
type(grid_dt), target :: g(3)
type(grid_dt), pointer :: x => g(1), z => g(last)
real, target :: a(4) = [1.0, 2.0, 3.0, 4.0]
real, pointer :: p => a(2)
type(view) :: v

if (.not. associated(q, uarr(3))) error stop 1
if (q%size /= 5) error stop 2
q%size = 50
if (uarr(3)%size /= 50) error stop 3
if (uarr(2)%size /= 5 .or. uarr(4)%size /= 5) error stop 4

if (.not. associated(q2, grid2(2, 3))) error stop 5
q2%size = 23
if (grid2(2, 3)%size /= 23) error stop 6
if (grid2(1, 3)%size /= 5 .or. grid2(2, 2)%size /= 5) error stop 7

if (.not. associated(q_shifted, shifted(-1))) error stop 8
q_shifted%size = -1
if (shifted(-1)%size /= -1 .or. shifted(0)%size /= 5) error stop 9

if (.not. associated(q_comp, h%grids(2))) error stop 10
q_comp%size = 12
if (h%grids(2)%size /= 12 .or. h%grids(1)%size /= 5) error stop 11

g(1)%size = 10
g(3)%size = 30
if (.not. associated(x, g(1))) error stop 12
if (.not. associated(z, g(3))) error stop 13
if (x%size /= 10 .or. z%size /= 30) error stop 14
z%size = 31
if (g(3)%size /= 31 .or. g(2)%size /= 5) error stop 15

if (.not. associated(p, a(2))) error stop 16
if (p /= 2.0) error stop 17
p = 20.0
if (a(2) /= 20.0) error stop 18

if (.not. associated(v%first, uarr(1))) error stop 19
v%first%size = 41
if (uarr(1)%size /= 41) error stop 22

call bump(5)
call bump(6)

print *, "ok"
end program
