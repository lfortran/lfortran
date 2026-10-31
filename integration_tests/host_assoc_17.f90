module host_assoc_17_m
implicit none
abstract interface
    subroutine callback(value)
        integer, intent(out) :: value
    end subroutine
end interface
type :: holder
    procedure(callback), pointer, nopass :: p => null()
end type
contains
recursive subroutine host(depth, older, previous)
    integer, intent(in) :: depth
    procedure(callback), pointer, intent(in) :: older
    type(holder), intent(in) :: previous
    procedure(callback), pointer :: current, copied
    type(holder) :: box, box_copy
    integer :: captured, value
    captured = 10*depth
    current => read_state
    copied => current
    box%p => current
    box_copy%p => box%p
    if (.not. associated(copied, current)) error stop 1
    if (.not. associated(box_copy%p, current)) error stop 2
    if (associated(older)) then
        call older(value)
        if (value /= 10*(depth + 1)) error stop 3
        call previous%p(value)
        if (value /= 10*(depth + 1)) error stop 4
        ! Distinct live instances of the same host have different targets.
        if (associated(older, current)) error stop 5
        if (associated(previous%p, current)) error stop 6
    end if
    call copied(value)
    if (value /= 10*depth) error stop 7
    if (depth > 1) call host(depth - 1, copied, box_copy)
    captured = captured + 1
    call box_copy%p(value)
    if (value /= 10*depth + 1) error stop 8
contains
    subroutine read_state(value)
        integer, intent(out) :: value
        value = captured
    end subroutine
end subroutine
end module

program host_assoc_17
use host_assoc_17_m
implicit none
procedure(callback), pointer :: empty => null()
type(holder) :: empty_box
call host(3, empty, empty_box)
end program
