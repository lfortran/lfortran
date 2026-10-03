program template_instantiate_scope_05
    implicit none
    template unary{op}
        deferred interface
            integer function op(x)
                integer, intent(in) :: x
            end function
        end interface
        type :: box
            integer :: n
        end type
    contains
        integer function apply(x) result(value)
            integer, intent(in) :: x
            value = op(x)
        end function
    end template

    instantiate unary{abs}, only: apply_abs => apply, int_box => box
    integer, parameter :: ik = kind(0)
    type(int_box) :: item
    procedure(abs), pointer :: callback

    item%n = -3
    callback => abs
    if (apply_abs(item%n) /= 13) error stop
    if (callback(item%n) /= 13) error stop
contains
    integer(ik) function abs(x) result(value)
        integer(ik), intent(in) :: x
        ! This type is supplied by the instantiation that needs this interface.
        type(int_box) :: local
        local%n = x
        value = 10 - local%n
    end function
end program
