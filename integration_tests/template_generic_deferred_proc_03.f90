module template_generic_deferred_proc_03_m
    implicit none

contains

    real function add_half(y)
        real, intent(in) :: y
        add_half = y + 0.5
    end function

    ! The generic interface `g`, declared in a templated subroutine, has a
    ! deferred procedure `f` and a module procedure `add_half` as specifics.
    template subroutine apply{f}(x, r, s)
        deferred interface
            integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        interface g
            procedure f, add_half
        end interface
        integer, intent(inout) :: x
        real, intent(inout) :: r, s
        x = g(x)
        r = g(r)
        s = add_half(s)
    end subroutine

    integer function double_it(y)
        integer, intent(in) :: y
        double_it = 2*y
    end function
end module

program template_generic_deferred_proc_03
    use template_generic_deferred_proc_03_m, only: apply, double_it
    implicit none
    integer :: v
    real :: r, s
    v = 5
    r = 1
    s = 2
    call apply{double_it}(v, r, s)
    print *, v, r, s
    if (v /= 10) error stop
    if (abs(r - 1.5) > 1e-6) error stop
    if (abs(s - 2.5) > 1e-6) error stop
end program
