! Concrete standard-Fortran control for the host association in scope_04.
module template_instantiate_scope_07_m
    implicit none
    integer, parameter :: offset = 10
contains
    integer function op(x) result(value)
        integer, intent(in) :: x
        value = x + 20
    end function

    subroutine check()
        integer, parameter :: offset = 20
        procedure(op), pointer :: callback
        callback => op
        if (op(2) /= 22) error stop
        if (apply_op(3) /= 23) error stop
        if (callback(4) /= 24) error stop
    contains
        integer function apply_op(x) result(value)
            integer, intent(in) :: x
            value = op(x)
        end function

        integer function op(x) result(value)
            integer, intent(in) :: x
            integer, parameter :: captured = offset
            value = x + captured
        end function
    end subroutine
end module

program template_instantiate_scope_07
    use template_instantiate_scope_07_m, only: check
    implicit none
    call check()
end program
