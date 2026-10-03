module template_intrinsic_func_02_m
    implicit none
contains
    template function g{f, t}(x, y) result(r)
        deferred type :: t
        interface
            pure elemental function f(a, b) result(c)
                import :: t
                type(t), intent(in) :: a, b
                type(t) :: c
            end function
        end interface
        type(t), intent(in) :: x, y
        type(t) :: r
        r = f(x, y)
    end function

    template subroutine s{f}(x, y, r)
        interface
            pure elemental function f(a, b) result(c)
                integer, intent(in) :: a, b
                integer :: c
            end function
        end interface
        integer, intent(in) :: x, y
        integer, intent(out) :: r
        r = f(x, y)
    end subroutine

    template function outer{u}(x, y) result(r)
        deferred type :: u
        type(u), intent(in) :: x, y
        integer :: r
        r = g{max, integer}(3, 7) + g{min, integer}(3, 7)
    end function

    subroutine check_real()
        if (abs(g{min, real}(2.5, -1.5) - (-1.5)) > 1e-6) error stop
        if (abs(g{max, real}(2.5, -1.5) - 2.5) > 1e-6) error stop
    end subroutine
end module

program template_intrinsic_func_02
    use template_intrinsic_func_02_m
    implicit none
    integer :: i
    if (g{max, integer}(3, 7) /= 7) error stop
    if (g{min, integer}(3, 7) /= 3) error stop
    call check_real()
    call s{max}(-4, -9, i)
    if (i /= -4) error stop
    call s{min}(-4, -9, i)
    if (i /= -9) error stop
    if (outer{real}(1.0, 2.0) /= 10) error stop
    print *, g{max, integer}(3, 7), g{min, integer}(3, 7)
end program
