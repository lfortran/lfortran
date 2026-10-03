! A templated procedure that calls another templated procedure returning a
! derived type or an array. The pass that turns such functions into
! subroutines must transform the callee in the template itself, in a nested
! template, and in a template instantiated into the main program, since the
! calls to it in all of them are rewritten.
module template_derived_result_01_m
    implicit none
    type :: box
        integer :: v
    end type
    template tm {t}
        deferred type :: t
        template step_tmpl {k}
            deferred integer, parameter :: k
        contains
            function step(x) result(r)
                type(box), intent(in) :: x
                type(box) :: r
                r%v = x%v + k
            end function
            function step2(x) result(r)
                type(box), intent(in) :: x
                type(box) :: r
                r = step(step(x))
            end function
        end template
    contains
        function inc(x) result(r)
            type(box), intent(in) :: x
            type(box) :: r
            r%v = x%v + 1
        end function
        function inc2(x) result(r)
            type(box), intent(in) :: x
            type(box) :: r
            r = inc(inc(x))
        end function
        function get(x) result(r)
            type(box), intent(in) :: x
            integer :: r
            r = x%v
        end function
        function inc3(x) result(r)
            type(box), intent(in) :: x
            integer :: r
            r = get(inc(inc2(x)))
        end function
        function dup(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r(2)
            r = x
        end function
        function dup_first(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            type(t) :: y(2)
            y = dup(x)
            r = y(1)
        end function
    end template
end module

program template_derived_result_01
    use template_derived_result_01_m
    implicit none
    integer, parameter :: ten = 10
    instantiate tm {integer}, only: istep_tmpl => step_tmpl
    instantiate istep_tmpl {ten}, only: istep2 => step2
    instantiate tm {integer}, only: iinc2 => inc2, iinc3 => inc3, idup_first => dup_first
    instantiate tm {real}, only: rdup_first => dup_first
    type(box) :: b
    b%v = 3
    b = iinc2(b)
    if (b%v /= 5) error stop
    if (iinc3(b) /= 8) error stop
    if (idup_first(7) /= 7) error stop
    if (abs(rdup_first(1.5) - 1.5) > 1e-6) error stop
    b = istep2(b)
    if (b%v /= 25) error stop
    print *, b%v, iinc3(b), idup_first(7), rdup_first(1.5)
end program
