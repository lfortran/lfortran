module template_deferred_const_17_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
    contains
        ! Entities of a procedure named like the deferred constant `n` hide it.
        integer function f() result(r)
            associate (n => 7)
                r = n
            end associate
        end function

        integer function g() result(r)
            integer :: n
            n = 5
            n = n + 1
            r = n
        end function

        integer function h(n) result(r)
            integer, intent(in) :: n
            r = n*10
        end function

        integer function c() result(r)
            integer, parameter :: n = 4
            integer :: x(n)
            r = size(x)
        end function

        integer function d() result(r)
            r = n
        end function

        ! An internal procedure with its own `n`
        integer function e() result(r)
            r = inner() + n
        contains
            integer function inner()
                integer :: n
                n = 100
                inner = n
            end function
        end function
    end template
end module

program template_deferred_const_17
    use template_deferred_const_17_m
    implicit none
    instantiate tm {3}, only: f3 => f, g3 => g, h3 => h, c3 => c, d3 => d, &
        e3 => e
    print *, f3(), g3(), h3(2), c3(), d3(), e3()
    if (f3() /= 7) error stop
    if (g3() /= 6) error stop
    if (h3(2) /= 20) error stop
    if (c3() /= 4) error stop
    if (d3() /= 3) error stop
    if (e3() /= 103) error stop
end program
