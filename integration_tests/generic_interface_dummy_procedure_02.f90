module generic_interface_dummy_procedure_02_m
    implicit none

contains

    ! The generic interface `g` is local to `apply`; the module function
    ! `g` is a different procedure, visible everywhere else in the module.
    subroutine apply(f, x)
        interface
            integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        interface g
            procedure f
        end interface
        integer, intent(inout) :: x
        x = g(x)
    end subroutine

    integer function g(y)
        integer, intent(in) :: y
        g = y + 1000
    end function

    integer function dbl(y)
        integer, intent(in) :: y
        dbl = 2*y
    end function

    subroutine other(x)
        integer, intent(inout) :: x
        x = g(x)
    end subroutine
end module

program generic_interface_dummy_procedure_02
    use generic_interface_dummy_procedure_02_m
    implicit none
    integer :: v
    v = 5
    call apply(dbl, v)
    print *, v
    if (v /= 10) error stop
    call other(v)
    print *, v
    if (v /= 1010) error stop
    print *, g(1)
    if (g(1) /= 1001) error stop
end program
