module template_associate_01_m
    implicit none

    requirement op_r {t, f}
        deferred type :: t
        deferred interface
            pure elemental function f(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
    end requirement

    template associate_tmpl {t, plus}
        require :: op_r {t, plus}
    contains
        subroutine set(x, y)
            type(t), intent(inout) :: x
            type(t), intent(in) :: y
            associate (q => x)
                q = y
            end associate
        end subroutine

        subroutine add_to(x, y)
            type(t), intent(inout) :: x(:)
            type(t), intent(in) :: y
            integer :: i
            associate (a => x, s => plus(y, y))
                do i = 1, size(a)
                    associate (e => a(i))
                        e = plus(e, s)
                    end associate
                end do
            end associate
        end subroutine

        subroutine fill_first(x, y, n)
            integer, intent(in) :: n
            type(t), intent(inout) :: x(:)
            type(t), intent(in) :: y
            associate (b => x(1:n), m => n + 1)
                b = y
                if (m /= n + 1) error stop
            end associate
        end subroutine

        function twice(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            associate (d => plus(x, x))
                r = d
            end associate
        end function
    end template

contains

    pure elemental function add_integer(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        z = x + y
    end function

    pure elemental function add_real(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x + y
    end function

end module

program template_associate_01
    use template_associate_01_m
    implicit none
    instantiate associate_tmpl {integer, add_integer}, only: set_i => set, &
        add_to_i => add_to, fill_first_i => fill_first, twice_i => twice
    instantiate associate_tmpl {real, add_real}, only: set_r => set, &
        twice_r => twice
    integer :: v, a(3)
    real :: r

    v = 1
    call set_i(v, 2)
    if (v /= 2) error stop
    a = [1, 2, 3]
    call add_to_i(a, 5)
    if (any(a /= [11, 12, 13])) error stop
    call fill_first_i(a, 7, 2)
    if (any(a /= [7, 7, 13])) error stop
    if (twice_i(21) /= 42) error stop
    r = 0.0
    call set_r(r, 1.5)
    if (abs(r - 1.5) > 1e-6) error stop
    if (abs(twice_r(1.25) - 2.5) > 1e-6) error stop
    print *, v, a, r
end program
