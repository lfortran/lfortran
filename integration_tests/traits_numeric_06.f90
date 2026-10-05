! Inline constraints are an LFortran extension, not accepted by GFortran.
module traits_numeric_06_m
    use iso_fortran_env, only: real64
    implicit none

contains

    function inline_shift{integer | real(real64) :: T}(x, n) result(value)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        type(T) :: value
        value = x + T(n)
    end function inline_shift

    function mean{integer | real(real64) :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T) :: r
        integer :: i
        r = T(0)
        do i = 1, size(x)
            r = r + x(i)
        end do
        r = r / T(size(x))
    end function mean

    recursive function advance{integer | real(real64) :: T}(x, n) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        type(T) :: r
        if (n == 0) then
            r = x
        else if (n == 1) then
            r = advance{T}(x + T(1), n - 1)
        else
            r = advance(x + T(1), n - 1)
        end if
    end function advance
end module traits_numeric_06_m

program traits_numeric_06
    use traits_numeric_06_m, only: inline_shift, mean, advance
    use iso_fortran_env, only: real64
    implicit none
    integer :: integers(4)
    real(real64) :: reals(4)

    call expect_integer(inline_shift(4, -2), 2)
    call expect_integer(inline_shift{integer}(4, -2), 2)
    call expect_real64(inline_shift(4.5_real64, -2), 2.5_real64)
    call expect_real64(inline_shift{real(real64)}(4.5_real64, -2), 2.5_real64)
    call expect_real64(inline_shift(0.0_real64, 16777217), 16777217.0_real64)
    if (kind(inline_shift(0.0_real64, 1)) /= real64) error stop

    integers = [1, 2, 4, 8]
    reals = real(integers, real64)
    call expect_integer(mean(integers(::2)), 2)
    call expect_real64(mean{real(real64)}(reals(::2)), 2.5_real64)
    call expect_integer(mean{integer}(-integers(:3)), -2)
    call expect_real64(mean(reals(:3)), 7.0_real64 / 3.0_real64)
    call expect_integer(advance(4, 3), 7)
    call expect_real64(advance{real(real64)}(1.5_real64, 3), 4.5_real64)

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
end program traits_numeric_06
