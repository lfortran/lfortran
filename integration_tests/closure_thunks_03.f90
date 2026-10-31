! Hand-lowered internal-procedure callback (#16400) passed to a C routine
! through an implicit interface: a thunk is an ordinary procedure, so C
! receives and calls a raw code pointer. The source program passes the
! internal procedure `bump`, which adds to the host's `total`, to
! `call_twice`, from two live activations of a recursive host.
module closure_thunks_03_m
implicit none
abstract interface
    subroutine callback(v)
        integer, intent(in) :: v
    end subroutine
end interface

type :: host_frame
    integer, pointer :: total => null()
    integer :: bump_thunk = 0
end type

type :: host_frame_ptr
    type(host_frame), pointer :: p => null()
end type

type(host_frame_ptr), save :: bump_slot(2)

contains

recursive subroutine host(depth, total_out)
    integer, intent(in) :: depth
    integer, intent(out) :: total_out
    integer, target :: total
    integer :: inner
    type(host_frame), target :: frame
    procedure(callback), pointer :: p
    external :: call_twice
    frame%total => total
    total = 0
    p => bump_value(frame)
    if (depth > 1) call host(depth - 1, inner)
    ! The newer activation has returned; this activation's thunk still
    ! reaches this activation's `total`.
    call call_twice(p, depth)
    total_out = total
    call bump_release(frame)
end subroutine

subroutine bump(v, frame)
    integer, intent(in) :: v
    type(host_frame), intent(in) :: frame
    frame%total = frame%total + v
end subroutine

function bump_value(frame) result(p)
    type(host_frame), target, intent(inout) :: frame
    procedure(callback), pointer :: p
    integer :: i
    if (frame%bump_thunk == 0) then
        do i = 1, size(bump_slot)
            if (.not. associated(bump_slot(i)%p)) exit
        end do
        if (i > size(bump_slot)) error stop "too many simultaneously live values of internal procedure 'bump'"
        bump_slot(i)%p => frame
        frame%bump_thunk = i
    end if
    select case (frame%bump_thunk)
    case (1)
        p => bump_thunk_1
    case (2)
        p => bump_thunk_2
    end select
end function

subroutine bump_release(frame)
    type(host_frame), intent(inout) :: frame
    if (frame%bump_thunk /= 0) then
        nullify(bump_slot(frame%bump_thunk)%p)
        frame%bump_thunk = 0
    end if
end subroutine

subroutine bump_thunk_1(v)
    integer, intent(in) :: v
    if (.not. associated(bump_slot(1)%p)) error stop "internal procedure 'bump' called after its host returned"
    call bump(v, bump_slot(1)%p)
end subroutine

subroutine bump_thunk_2(v)
    integer, intent(in) :: v
    if (.not. associated(bump_slot(2)%p)) error stop "internal procedure 'bump' called after its host returned"
    call bump(v, bump_slot(2)%p)
end subroutine
end module

program closure_thunks_03
use closure_thunks_03_m
implicit none
integer :: total
call host(2, total)
print *, total
if (total /= 4) error stop 1
end program
