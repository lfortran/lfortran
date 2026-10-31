! Hand-lowered form of internal-procedure callbacks through ASR thunk
! pools (#16400): the shape the closure_thunks pass is to generate, written
! as plain Fortran to check that every backend handles it. The source
! program is host_assoc_16: an older and a newer live activation of a
! recursive host each pass their own callback, and the older one must still
! see its own activation's variable.
module closure_thunks_01_m
implicit none
abstract interface
    subroutine callback(value)
        integer, intent(out) :: value
    end subroutine
end interface

! Frame of `host`: the captured variable by reference, plus this
! activation's thunk of `current` (0: none yet).
type :: host_frame
    integer, pointer :: captured => null()
    integer :: current_thunk = 0
end type

type :: host_frame_ptr
    type(host_frame), pointer :: p => null()
end type

! Thunk pool of `current`: slot i holds the frame bound to thunk i.
type(host_frame_ptr), save :: current_slot(4)

contains

subroutine invoke(p, value)
    procedure(callback) :: p
    integer, intent(out) :: value
    call p(value)
end subroutine

subroutine forward(older, newer, old_value, new_value)
    procedure(callback) :: older, newer
    integer, intent(out) :: old_value, new_value
    call invoke(newer, new_value)
    call invoke(older, old_value)
end subroutine

recursive subroutine host(depth, older)
    integer, intent(in) :: depth
    procedure(callback) :: older
    integer, target :: captured
    integer :: old_value, new_value
    type(host_frame), target :: frame
    procedure(callback), pointer :: p, q
    frame%captured => captured
    captured = 10*depth
    p => current_value(frame)
    call forward(older, p, old_value, new_value)
    if (new_value /= 10*depth) error stop 1
    if (old_value /= 10*(depth + 1)) error stop 2
    ! One thunk per activation: a second escape yields the same value.
    q => current_value(frame)
    if (.not. associated(p, q)) error stop 3
    if (depth > 1) then
        call host(depth - 1, p)
    end if
    call current_release(frame)
end subroutine

! The internal procedure `current`, with its frame as an extra dummy.
subroutine current(value, frame)
    integer, intent(out) :: value
    type(host_frame), intent(in) :: frame
    value = frame%captured
end subroutine

! ProcedureClosure(current, frame): acquire once per activation, select the
! activation's thunk.
function current_value(frame) result(p)
    type(host_frame), target, intent(inout) :: frame
    procedure(callback), pointer :: p
    integer :: i
        if (frame%current_thunk == 0) then
            do i = 1, size(current_slot)
                if (.not. associated(current_slot(i)%p)) exit
            end do
            if (i > size(current_slot)) error stop "too many simultaneously live values of internal procedure 'current'"
            current_slot(i)%p => frame
            frame%current_thunk = i
        end if
        select case (frame%current_thunk)
        case (1)
            p => current_thunk_1
        case (2)
            p => current_thunk_2
        case (3)
            p => current_thunk_3
        case (4)
            p => current_thunk_4
        end select
end function

subroutine current_release(frame)
    type(host_frame), intent(inout) :: frame
    if (frame%current_thunk /= 0) then
        nullify(current_slot(frame%current_thunk)%p)
        frame%current_thunk = 0
    end if
end subroutine

subroutine current_thunk_1(value)
    integer, intent(out) :: value
    if (.not. associated(current_slot(1)%p)) error stop "internal procedure 'current' called after its host returned"
    call current(value, current_slot(1)%p)
end subroutine

subroutine current_thunk_2(value)
    integer, intent(out) :: value
    if (.not. associated(current_slot(2)%p)) error stop "internal procedure 'current' called after its host returned"
    call current(value, current_slot(2)%p)
end subroutine

subroutine current_thunk_3(value)
    integer, intent(out) :: value
    if (.not. associated(current_slot(3)%p)) error stop "internal procedure 'current' called after its host returned"
    call current(value, current_slot(3)%p)
end subroutine

subroutine current_thunk_4(value)
    integer, intent(out) :: value
    if (.not. associated(current_slot(4)%p)) error stop "internal procedure 'current' called after its host returned"
    call current(value, current_slot(4)%p)
end subroutine

subroutine initial(value)
    integer, intent(out) :: value
    value = 40
end subroutine
end module

program closure_thunks_01
use closure_thunks_01_m
implicit none
integer :: i
call host(3, initial)
! Every activation released its thunk.
do i = 1, size(current_slot)
    if (associated(current_slot(i)%p)) error stop 4
end do
print *, "ok"
end program
