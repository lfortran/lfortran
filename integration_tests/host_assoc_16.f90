module host_assoc_16_m
implicit none
abstract interface
    subroutine callback(value)
        integer, intent(out) :: value
    end subroutine
end interface
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
    integer :: captured, old_value, new_value
    captured = 10*depth
    ! Create this activation's callback before invoking the older one.
    call forward(older, current, old_value, new_value)
    if (new_value /= 10*depth) error stop 1
    if (old_value /= 10*(depth + 1)) error stop 2
    if (depth > 1) call host(depth - 1, current)
contains
    subroutine current(value)
        integer, intent(out) :: value
        value = captured
    end subroutine
end subroutine

subroutine initial(value)
    integer, intent(out) :: value
    value = 40
end subroutine
end module

program host_assoc_16
use host_assoc_16_m
implicit none
call host(3, initial)
end program
