module host_assoc_19_module
implicit none
abstract interface
    pure integer function bound_callback()
    end function
    function callback(x, text, extra) result(value)
        integer, intent(in) :: x(:)
        character(*), intent(in) :: text
        integer, intent(in), optional :: extra
        integer :: value(size(x))
    end function
end interface
contains
subroutine check_bound(bound, expected)
    procedure(bound_callback) :: bound
    integer, intent(in) :: expected
    integer :: workspace(bound())
    character(bound()) :: text
    if (size(workspace) /= expected .or. len(text) /= expected) error stop 7
end subroutine
subroutine apply(f, x, text, value, extra)
    procedure(callback) :: f
    integer, intent(in) :: x(:)
    character(*), intent(in) :: text
    integer, intent(out) :: value(:)
    integer, intent(in), optional :: extra
    value = f(x, text, extra)
end subroutine
recursive subroutine host(depth)
    integer, intent(in) :: depth
    procedure(callback), pointer :: current
    integer :: captured, before, value(2), evaluations
    captured = depth * 10
    evaluations = 0
    call check_bound(get_length, captured)
    current => calculate
    before = captured
    call apply(current, make_values(), 'abc', value, 5)
    if (any(value /= [1, 2] + 2 * before + 8)) error stop 1
    if (captured /= before + 1) error stop 2
    if (evaluations /= 1) error stop 8
    if (depth > 1) call host(depth - 1)
    before = captured
    call apply(current, [1, 2], make_text(), value)
    if (any(value /= [1, 2] + 2 * before + 1)) error stop 3
    if (captured /= before + 1) error stop 4
    if (evaluations /= 2) error stop 9
    call check_bound(get_length, captured)
contains
    function make_values() result(value)
        integer :: value(2)
        evaluations = evaluations + 1
        value = [1, 2]
    end function
    function make_text() result(value)
        character :: value
        evaluations = evaluations + 1
        value = 'q'
    end function
    pure integer function get_length()
        get_length = captured
    end function
    function calculate(x, text, extra) result(value)
        integer, intent(in) :: x(:)
        character(*), intent(in) :: text
        integer, intent(in), optional :: extra
        integer :: value(size(x)), snapshot(captured)
        character(captured) :: local
        integer :: original
        original = captured
        captured = captured + 1
        if (size(snapshot) /= original) error stop 5
        if (len(local) /= original) error stop 6
        value = x + size(snapshot) + len(local) + len(text)
        if (present(extra)) value = value + extra
    end function
end subroutine
end module
program host_assoc_19
use host_assoc_19_module
implicit none
call host(3)
end program
