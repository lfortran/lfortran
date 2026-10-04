module generic_interface_dummy_procedure_01_m
    implicit none
contains
    ! The specific procedure of the generic interface `g` is the dummy
    ! procedure `f`, which is only visible inside `apply`.
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

    ! A generic interface of the same name in another procedure is a
    ! different generic interface.
    subroutine apply_twice(f, x)
        interface
            integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        interface g
            procedure f
        end interface
        integer, intent(inout) :: x
        x = g(g(x))
    end subroutine

    integer function double_it(y)
        integer, intent(in) :: y
        double_it = 2*y
    end function

    integer function add_one(y)
        integer, intent(in) :: y
        add_one = y + 1
    end function
end module

program generic_interface_dummy_procedure_01
    use generic_interface_dummy_procedure_01_m
    implicit none
    integer :: v
    v = 5
    call apply(double_it, v)
    print *, v
    if (v /= 10) error stop
    call apply(add_one, v)
    print *, v
    if (v /= 11) error stop
    call apply_twice(double_it, v)
    print *, v
    if (v /= 44) error stop
end program
