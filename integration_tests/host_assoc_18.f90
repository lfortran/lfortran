module host_assoc_18_hosts
use host_assoc_18_module
implicit none
contains
recursive subroutine host(depth, older)
    integer, intent(in) :: depth
    procedure(callback), pointer, intent(in) :: older
    procedure(callback), pointer :: current, copied, noncapturing, returned
    procedure(), pointer :: opaque
    type(holder) :: box, box_copy
    integer :: captured, value
    captured = depth * 10
    current => read_state
    call set_out(copied, current)
    returned => copy_function(current)
    if (.not. associated(returned, current)) error stop 15
    global_target => current
    call hold_target(global_target, value)
    if (value /= depth * 10) error stop 19
    if (.not. associated(global_target, plain)) error stop 20
    box = holder(read_state)
    box_copy = box
    if (.not. associated(box_copy%p, current)) error stop 10
    if (.not. associated(current, copied)) error stop 1
    if (associated(older)) then
        call older(value)
        if (value /= 10 * (depth + 1)) error stop 2
        if (associated(older, current)) error stop 3
    end if
    if (depth > 1) call host(depth - 1, current)
    captured = captured + 1
    call invoke(copied, value)
    if (value /= depth * 10 + 1) error stop 4
    call box_copy%p(value)
    if (value /= depth * 10 + 1) error stop 11
    call returned(value)
    if (value /= depth * 10 + 1) error stop 16
    opaque => current
    if (.not. associated(opaque, current)) error stop 17
    call opaque(value)
    if (value /= depth * 10 + 1) error stop 18
    noncapturing => plain
    call set_inout(copied, noncapturing)
    if (associated(current, copied)) error stop 5
    call copied(value)
    if (value /= -1) error stop 6
    call set_inout(copied, current)
    if (.not. associated(current, copied)) error stop 7
    opaque => reset_copied
    call opaque()
    if (associated(current, copied)) error stop 21
    call copied(value)
    if (value /= -1) error stop 22
    nullify(copied)
    if (associated(copied)) error stop 8
    call maybe(value)
    if (value /= -2) error stop 12
    call maybe(value, copied)
    if (value /= -3) error stop 13
    call maybe(value, current)
    if (value /= depth * 10 + 1) error stop 14
    call current(value)
    if (value /= depth * 10 + 1) error stop 9
contains
    subroutine reset_copied()
        copied => plain
    end subroutine
    subroutine read_state(value)
        integer, intent(out) :: value
        value = captured
    end subroutine
end subroutine
end module

program host_assoc_18
use host_assoc_18_hosts
implicit none
procedure(callback), pointer :: empty => null()
call host(3, empty)
end program
