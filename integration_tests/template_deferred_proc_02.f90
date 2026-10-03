! A `deferred procedure(iface)` statement of a templated subprogram names an
! interface declared earlier in the same subprogram, by an abstract interface
! block or by an ordinary interface block, as it can in a template construct.
module template_deferred_proc_02_m
    implicit none
contains

    template subroutine apply{f}(x)
        abstract interface
            integer function iface(y)
                integer, intent(in) :: y
            end function
        end interface
        deferred procedure(iface) :: f
        integer, intent(inout) :: x
        x = f(x)
    end subroutine

    template function combine{f, g}(x) result(r)
        abstract interface
            integer function unary(y)
                integer, intent(in) :: y
            end function
        end interface
        interface
            integer function unary2(y)
                integer, intent(in) :: y
            end function
        end interface
        deferred procedure(unary) :: f
        deferred procedure(unary2) :: g
        integer, intent(in) :: x
        integer :: r
        r = f(x) + g(x)
    end function

    template subroutine modify{s}(x)
        abstract interface
            subroutine update(y)
                integer, intent(inout) :: y
            end subroutine
        end interface
        deferred procedure(update) :: s
        integer, intent(inout) :: x
        call s(x)
    end subroutine

    integer function double_it(y)
        integer, intent(in) :: y
        double_it = 2*y
    end function

    integer function triple_it(y)
        integer, intent(in) :: y
        triple_it = 3*y
    end function

    subroutine increment(y)
        integer, intent(inout) :: y
        y = y + 1
    end subroutine

end module

program template_deferred_proc_02
    use template_deferred_proc_02_m
    implicit none
    integer :: v
    v = 5
    call apply{double_it}(v)
    print *, v
    if (v /= 10) error stop
    v = combine{double_it, triple_it}(5)
    print *, v
    if (v /= 25) error stop
    v = combine{triple_it, triple_it}(1)
    if (v /= 6) error stop
    call modify{increment}(v)
    print *, v
    if (v /= 7) error stop
end program
