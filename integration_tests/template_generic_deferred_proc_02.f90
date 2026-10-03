module template_generic_deferred_proc_02_m
    implicit none

contains

    ! The specifics of the generic interfaces `g` and `sg`, declared in a
    ! templated subroutine, are its deferred procedures `f` and `sf`.
    template subroutine apply{f, sf}(x)
        deferred interface
            integer function f(y)
                integer, intent(in) :: y
            end function
            subroutine sf(y)
                integer, intent(inout) :: y
            end subroutine
        end interface
        interface g
            procedure f
        end interface
        interface sg
            procedure sf
        end interface
        integer, intent(inout) :: x
        x = g(x)
        call sg(x)
    end subroutine

    integer function double_it(y)
        integer, intent(in) :: y
        double_it = 2*y
    end function

    integer function add_one(y)
        integer, intent(in) :: y
        add_one = y + 1
    end function

    subroutine add_three(y)
        integer, intent(inout) :: y
        y = y + 3
    end subroutine

    subroutine negate(y)
        integer, intent(inout) :: y
        y = -y
    end subroutine
end module

program template_generic_deferred_proc_02
    use template_generic_deferred_proc_02_m
    implicit none
    integer :: v
    v = 5
    call apply{double_it, add_three}(v)
    print *, v
    if (v /= 13) error stop
    call apply{add_one, negate}(v)
    print *, v
    if (v /= -14) error stop
end program
