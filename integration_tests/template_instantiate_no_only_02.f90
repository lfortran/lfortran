! An INSTANTIATE without an only-list makes the named generic interfaces of
! the template available, as well as their specific procedures (#13919).
module template_instantiate_no_only_02_m
    implicit none
    template tt {t, plus}
        deferred type :: t
        deferred interface
            function plus(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
        interface dbl
            procedure twice, plus
        end interface
        interface comb
            procedure twice, add3
        end interface
    contains
        function twice(x) result(z)
            type(t), intent(in) :: x
            type(t) :: z
            z = plus(x, x)
        end function
        function add3(x, y, w) result(z)
            type(t), intent(in) :: x, y, w
            type(t) :: z
            z = plus(plus(x, y), w)
        end function
    end template
contains
    function iadd(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        z = x + y
    end function
    function radd(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x + y
    end function
end module

module template_instantiate_no_only_02_real
    use template_instantiate_no_only_02_m, only: tt, radd
    implicit none
    instantiate tt {real, radd}
end module

program template_instantiate_no_only_02
    use template_instantiate_no_only_02_m, only: tt, iadd
    implicit none
    instantiate tt {integer, iadd}
    if (dbl(4) /= 8) error stop
    if (dbl(4, 5) /= 9) error stop
    if (twice(5) /= 10) error stop
    if (comb(3) /= 6) error stop
    if (comb(1, 2, 3) /= 6) error stop
    if (add3(1, 2, 4) /= 7) error stop
    call check_real()
    print *, dbl(4), twice(5), comb(1, 2, 3)
contains
    subroutine check_real()
        use template_instantiate_no_only_02_real, only: rdbl => dbl, &
            rcomb => comb, rtwice => twice
        if (abs(rdbl(1.5) - 3.0) > 1e-6) error stop
        if (abs(rdbl(1.5, 2.0) - 3.5) > 1e-6) error stop
        if (abs(rtwice(2.5) - 5.0) > 1e-6) error stop
        if (abs(rcomb(1.0, 2.0, 0.5) - 3.5) > 1e-6) error stop
    end subroutine
end program
