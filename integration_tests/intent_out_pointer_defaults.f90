module intent_out_pointer_bounds
contains
    pure integer function extent()
        extent = 2
    end function
end module

module intent_out_pointer_targets
    implicit none
    integer, target, save :: initial = 17
    type :: data
        integer :: n = 19
    end type
    type(data), target, save :: record
contains
    subroutine convert(x, y)
        use intent_out_pointer_bounds, only: extent
        integer, intent(in) :: x(extent())
        integer, intent(out) :: y
        y = product(x)
    end subroutine
end module

module intent_out_pointer_types
    use intent_out_pointer_targets, only: initial, record, convert
    implicit none
    type :: item
        procedure(convert), nopass, pointer :: callback => convert
        integer, pointer :: value => initial
        integer, pointer :: member => record%n
        integer :: n = 0
    end type
contains
    subroutine reset_dynamic(x)
        class(item), intent(out) :: x
        integer :: y
        if (.not. associated(x%callback)) error stop 1
        if (.not. associated(x%value, initial)) error stop 2
        if (.not. associated(x%member, record%n)) error stop 13
        call x%callback([3, 5], y)
        if (y /= 15 .or. x%n /= 0) error stop 3
    end subroutine
end module

program intent_out_pointer_defaults
    use intent_out_pointer_types, only: item, reset_dynamic
    implicit none
    type(item) :: x

    nullify(x%callback, x%value, x%member)
    x%n = -1
    call reset_dynamic(x)
    if (.not. associated(x%callback)) error stop 4
    if (.not. associated(x%value)) error stop 5
    if (x%value /= 17) error stop 6
    x%value = 23
    nullify(x%callback, x%value, x%member)
    call reset_static(x)
    if (.not. associated(x%callback)) error stop 7
    if (.not. associated(x%value)) error stop 8
    if (x%value /= 23) error stop 9
contains
    subroutine reset_static(x)
        type(item), intent(out) :: x
        integer, parameter :: initial = -1, record = -2, convert = -3
        integer :: y
        if (.not. associated(x%callback)) error stop 10
        if (.not. associated(x%value)) error stop 11
        if (.not. associated(x%member)) error stop 14
        if (x%member /= 19) error stop 15
        call x%callback([3, 7], y)
        if (y /= 21) error stop 12
    end subroutine
end program
