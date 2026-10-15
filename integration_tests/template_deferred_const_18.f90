module template_deferred_const_18_m
    implicit none
    template outer {n}
        deferred integer, parameter :: n
        integer, parameter :: k = n*2
        ! The deferred constant `n` of `inner` hides the one of `outer`.
        template inner {n}
            deferred integer, parameter :: n
            integer, parameter :: c = n + k
            type :: w
                integer :: b(n)
                integer :: d(c)
            end type
        contains
            integer function f()
                integer :: x(n)
                f = n*100 + size(x)*10 + c
            end function
        end template
    contains
        integer function g()
            g = n
        end function
    end template

    ! A template nested in `outer2`, instantiated in procedures of `outer2`,
    ! uses the deferred constant `n` of `outer2` by host association.
    template outer2 {n}
        deferred integer, parameter :: n
        template inner2 {m}
            deferred integer, parameter :: m
            integer, parameter :: c = n*m
        contains
            integer function f()
                integer :: x(n + m)
                f = c*100 + size(x)
            end function

            integer function e()
                integer :: x(n + m)
                e = size(x)
            end function
        end template
    contains
        integer function g()
            instantiate inner2 {n}, only: fi => f
            g = fi()
        end function

        integer function h()
            instantiate inner2 {5}, only: fi => f
            h = fi()
        end function

        integer function p()
            instantiate inner2 {n}, only: ei => e
            p = ei()
        end function
    end template
end module

program template_deferred_const_18
    use template_deferred_const_18_m
    implicit none
    instantiate outer {3}, only: inner3 => inner, g3 => g
    instantiate inner3 {5}, only: f5 => f, w5 => w
    instantiate outer2 {3}, only: inner23 => inner2, g23 => g, h23 => h, &
        p23 => p
    instantiate inner23 {5}, only: f25 => f
    type(w5) :: s
    print *, f5(), size(s%b), size(s%d), g3()
    if (f5() /= 561) error stop
    if (size(s%b) /= 5 .or. size(s%d) /= 11) error stop
    if (g3() /= 3) error stop
    print *, f25(), g23(), h23(), p23()
    if (f25() /= 1508) error stop
    if (g23() /= 906) error stop
    if (h23() /= 1508) error stop
    if (p23() /= 6) error stop
    call check_internal()
contains
    ! The copy of `inner2` is a template inside a procedure.
    subroutine check_internal()
        instantiate outer2 {4}, only: inner24 => inner2, g24 => g
        instantiate inner24 {5}, only: f45 => f
        print *, f45(), g24()
        if (f45() /= 2009) error stop
        if (g24() /= 1608) error stop
    end subroutine
end program
