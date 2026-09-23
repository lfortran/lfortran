! Keyword instantiation arguments (R1630, 16.5.5.1 para 2) for INSTANTIATE,
! REQUIRE and inline instantiation. Every argument list below is deliberately
! written in an order different from the deferred-argument list it is matched
! against, and the deferred types and procedures are chosen so that matching
! them by position instead of by keyword gives a different answer.
module template_instantiate_kwargs_01_m
    implicit none
    private
    public :: test_keyword, test_mixed, test_positional, test_inline

    ! Deferred arguments in the order (a, b, g)
    requirement pair_r {a, b, g}
        deferred type :: a
        deferred type :: b
        deferred interface
            function g(x, y) result(z)
                type(a), intent(in) :: x
                type(b), intent(in) :: y
                integer :: z
            end function
        end interface
    end requirement

    ! Deferred arguments in the order (t, u, f). The REQUIRE below passes them
    ! to pair_r by keyword, reversing the order.
    template pair_t(t, u, f)
        require :: pair_r {g = f, b = u, a = t}
        private
        public :: combine
    contains
        function combine(x, y) result(n)
            type(t), intent(in) :: x
            type(u), intent(in) :: y
            integer :: n
            n = f(x, y)
        end function
    end template

    requirement two_types_r {p, q}
        deferred type :: p
        deferred type :: q
    end requirement

contains

    integer function int_real(x, y) result(z)
        integer, intent(in) :: x
        real, intent(in) :: y
        z = 10 * x + int(y)
    end function

    integer function real_int(x, y) result(z)
        real, intent(in) :: x
        integer, intent(in) :: y
        z = 1000 * int(x) + y
    end function

    ! Inline instantiation of a templated subprogram. The REQUIRE here also
    ! uses keyword arguments, in reverse order.
    subroutine copy_both{T, U}(x, y, a, b)
        require :: two_types_r {q = U, p = T}
        type(T), intent(in) :: x
        type(U), intent(in) :: y
        type(T), intent(out) :: a
        type(U), intent(out) :: b
        a = x
        b = y
    end subroutine

    function second_of{T, U}(x, y) result(z)
        require :: two_types_r {q = U, p = T}
        type(T), intent(in) :: x
        type(U), intent(in) :: y
        type(U) :: z
        z = y
    end function

    ! All three instantiation arguments given by keyword, in reverse order.
    subroutine test_keyword()
        instantiate pair_t {f = int_real, u = real, t = integer}, only: c_kw => combine
        if (c_kw(3, 4.5) /= 34) error stop
    end subroutine

    ! A positional argument followed by keyword ones (C1625 allows this order).
    ! Here t is real and u is integer, the other way round from test_keyword.
    subroutine test_mixed()
        instantiate pair_t {real, f = real_int, u = integer}, only: c_mix => combine
        if (c_mix(3.5, 7) /= 3007) error stop
    end subroutine

    ! The all-positional form keeps working unchanged.
    subroutine test_positional()
        instantiate pair_t {integer, real, int_real}, only: c_pos => combine
        if (c_pos(3, 4.5) /= 34) error stop
    end subroutine

    subroutine test_inline()
        integer :: i
        real :: r
        call copy_both{U = real, T = integer}(5, 2.5, i, r)
        if (i /= 5) error stop
        if (abs(r - 2.5) > 1e-6) error stop
        r = second_of{U = real, T = integer}(7, 8.5)
        if (abs(r - 8.5) > 1e-6) error stop
    end subroutine

end module

program template_instantiate_kwargs_01
use template_instantiate_kwargs_01_m
implicit none

call test_keyword()
call test_mixed()
call test_positional()
call test_inline()
print *, "ok"

end program
