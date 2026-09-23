module global_init_03_m
implicit none

type :: holder
    integer :: x = 1
end type

integer, save, target :: scal = 5
type(holder), save, target :: dt = holder(9)

! A declaration that associates a pointer rather than giving it a value.
! No target can lay this out as static data, so it becomes the pointer
! assignment that runs before any user code observes the pointer.
integer, pointer :: p_scal => scal
type(holder), pointer :: p_dt => dt
integer, pointer :: p_null => null()

contains

subroutine local_pointer(expected)
    integer, intent(in) :: expected
    integer, save, target :: ltgt = 40
    integer, pointer :: lp => ltgt

    if (.not. associated(lp, ltgt)) error stop 10
    if (lp /= expected) error stop 11
    lp = lp + 1
    if (ltgt /= expected + 1) error stop 12
end subroutine

end module

program global_init_03
use global_init_03_m
implicit none

if (.not. associated(p_scal, scal)) error stop 1
if (p_scal /= 5) error stop 2

! The pointer really is an alias of its target, not a copy of it.
p_scal = 6
if (scal /= 6) error stop 3
scal = 7
if (p_scal /= 7) error stop 4

if (.not. associated(p_dt, dt)) error stop 5
if (p_dt%x /= 9) error stop 6
p_dt%x = 11
if (dt%x /= 11) error stop 7

if (associated(p_null)) error stop 8

! The pointer of a procedure is associated once, like every other
! initialized local, and stays associated across calls.
call local_pointer(40)
call local_pointer(41)

print *, "ok"
end program
