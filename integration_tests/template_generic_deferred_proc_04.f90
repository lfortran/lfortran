module template_generic_deferred_proc_04_m
    implicit none

    ! Subroutine calls through generic interfaces of a template block: the
    ! specific of `g` is the deferred subroutine `s`, and the specific of
    ! `racc` is a recursive procedure of the template that calls itself
    ! through `racc`.
    template apply_tmpl{s}
        deferred interface
            subroutine s(y)
                integer, intent(inout) :: y
            end subroutine
        end interface
        interface g
            procedure s
        end interface
        interface racc
            procedure racc_s
        end interface
    contains
        subroutine apply(x)
            integer, intent(inout) :: x
            call g(x)
        end subroutine

        recursive subroutine racc_s(x, n)
            integer, intent(inout) :: x
            integer, intent(in) :: n
            if (n <= 0) return
            call g(x)
            call racc(x, n - 1)
        end subroutine
    end template

contains

    subroutine double_it(y)
        integer, intent(inout) :: y
        y = 2*y
    end subroutine

    subroutine add_one(y)
        integer, intent(inout) :: y
        y = y + 1
    end subroutine
end module

program template_generic_deferred_proc_04
    use template_generic_deferred_proc_04_m
    implicit none
    instantiate apply_tmpl{double_it}, only: apply_double => apply, &
        repeat_double => racc_s
    instantiate apply_tmpl{add_one}, only: apply_add_one => apply, &
        repeat_add_one => racc_s
    integer :: v
    v = 5
    call apply_double(v)
    print *, v
    if (v /= 10) error stop
    call apply_add_one(v)
    print *, v
    if (v /= 11) error stop
    call repeat_double(v, 3)
    print *, v
    if (v /= 88) error stop
    call repeat_add_one(v, 4)
    print *, v
    if (v /= 92) error stop
end program
