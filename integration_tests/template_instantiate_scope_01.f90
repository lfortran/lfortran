program template_instantiate_scope_01
    implicit none

    template comparisons{t, lt}
        deferred type :: t
        deferred interface
            logical function lt(a, b)
                type(t), intent(in) :: a, b
            end function
        end interface

        type :: pair
            type(t) :: first, second
        end type
    contains
        logical function compare(a, b) result(value)
            type(t), intent(in) :: a, b
            value = lt(a, b)
        end function
    end template

    template updates{put}
        deferred interface
            subroutine put(x)
                integer, intent(out) :: x
            end subroutine
        end interface
    contains
        subroutine assign(x)
            integer, intent(out) :: x
            call put(x)
        end subroutine
    end template

    instantiate comparisons{integer, ilt}, only: less => compare, int_pair => pair
    ! Instantiation must still precede declarations using its derived types.
    type(int_pair) :: values
    integer :: offset, value
    instantiate comparisons{lt=igt, t=integer}, only: greater => compare
    instantiate comparisons{integer, ilt}, only: less_again => compare
    instantiate updates{put_value}, only: set_value => assign

    values%first = 4
    values%second = 7
    offset = 2
    if (.not. less(values%first, values%second)) error stop
    if (less(values%second, values%first)) error stop
    if (.not. greater(values%second, values%first)) error stop
    if (greater(values%first, values%second)) error stop
    offset = 4
    if (less_again(values%first, values%second)) error stop
    call set_value(value)
    if (value /= offset) error stop

contains

    logical function ilt(a, b)
        integer, intent(in) :: a, b
        ilt = a + offset < b
    end function

    logical function igt(a, b)
        integer, intent(in) :: a, b
        igt = a > b + offset
    end function

    subroutine put_value(x)
        integer, intent(out) :: x
        x = offset
    end subroutine

end program
