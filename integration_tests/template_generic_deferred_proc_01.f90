module template_generic_deferred_proc_01_m
    implicit none

    ! The specific procedure of the generic interface `g` is the deferred
    ! procedure `f` of the template.
    template apply_tmpl{f}
        deferred interface
            integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        interface g
            procedure f
        end interface
    contains
        subroutine apply(x)
            integer, intent(inout) :: x
            x = g(x)
        end subroutine
    end template

contains

    integer function double_it(y)
        integer, intent(in) :: y
        double_it = 2*y
    end function

    integer function add_one(y)
        integer, intent(in) :: y
        add_one = y + 1
    end function
end module

program template_generic_deferred_proc_01
    use template_generic_deferred_proc_01_m
    implicit none
    instantiate apply_tmpl{double_it}, only: apply_double => apply
    instantiate apply_tmpl{add_one}, only: apply_add_one => apply
    integer :: v
    v = 5
    call apply_double(v)
    print *, v
    if (v /= 10) error stop
    call apply_add_one(v)
    print *, v
    if (v /= 11) error stop
end program
