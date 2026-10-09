! Traits are an LFortran extension; the concrete oracle also runs with GFortran.
module traits_numeric_02_m
    use iso_fortran_env, only: real64
    implicit none

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

contains

    function zero_like{INumeric :: T}(x)
        type(T), intent(in) :: x
        type(T) :: zero_like
        zero_like = T(0)
    end function zero_like

    function zero{INumeric :: T}() result(value)
        type(T) :: value
        value = T(0)
    end function zero

    function from_integer{INumeric :: T}(n) result(value)
        integer, intent(in) :: n
        type(T) :: value
        value = T(n)
    end function from_integer

    function evaluate{INumeric :: T}(a, b, n) result(value)
        type(T), intent(in) :: a, b
        integer, intent(in) :: n
        type(T) :: value
        value = (a + b) * T(n) - a / b
    end function evaluate

    function forward_result{INumeric :: T}(x, n) result(value)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        type(T) :: value
        value = identity{T}(x + T(n)) + identity(T(n))
    end function forward_result

    function identity{INumeric :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x
    end function identity
end module traits_numeric_02_m

program traits_numeric_02
    use traits_numeric_02_m, only: zero_like, zero, from_integer, evaluate, forward_result
    use iso_fortran_env, only: real64
    implicit none

    call expect_integer(zero_like(9), 0)
    call expect_integer(zero_like{integer}(9), 0)
    call expect_real64(zero_like(9.0_real64), 0.0_real64)
    call expect_real64(zero_like{real(real64)}(9.0_real64), 0.0_real64)
    call expect_integer(zero{integer}(), 0)
    call expect_real64(zero{real(real64)}(), 0.0_real64)
    call expect_integer(from_integer{integer}(-7), -7)
    call expect_real64(from_integer{real(real64)}(-7), -7.0_real64)
    call expect_real64(from_integer{real(real64)}(16777217), 16777217.0_real64)
    call expect_integer(evaluate(7, 2, 3), 24)
    call expect_integer(evaluate{integer}(7, 2, 3), 24)
    call expect_real64(evaluate(7.0_real64, 2.0_real64, 3), 23.5_real64)
    call expect_real64(evaluate{real(real64)}(7.0_real64, 2.0_real64, 3), 23.5_real64)
    call expect_integer(forward_result(7, 3), 13)
    call expect_real64(forward_result(7.5_real64, 3), 13.5_real64)
    if (kind(zero_like(1.0_real64)) /= real64) error stop
    if (kind(from_integer{real(real64)}(1)) /= real64) error stop

contains

    subroutine expect_integer(actual, expected)
        integer, intent(in) :: actual, expected
        if (actual /= expected) error stop
    end subroutine expect_integer

    subroutine expect_real64(actual, expected)
        real(real64), intent(in) :: actual, expected
        real(real64) :: tolerance
        tolerance = 32.0_real64 * epsilon(1.0_real64) * max(1.0_real64, abs(expected))
        if (.not. (abs(actual - expected) <= tolerance)) error stop
    end subroutine expect_real64
end program traits_numeric_02
