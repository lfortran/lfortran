module traits_numeric_02_oracle_m
    use iso_fortran_env, only: real64
    implicit none

    interface zero_like
        module procedure zero_like_integer, zero_like_real64
    end interface zero_like

    interface evaluate
        module procedure evaluate_integer, evaluate_real64
    end interface evaluate

contains

    function zero_like_integer(x)
        integer, intent(in) :: x
        integer :: zero_like_integer
        zero_like_integer = int(0, kind=kind(x))
    end function zero_like_integer

    function zero_like_real64(x)
        real(real64), intent(in) :: x
        real(real64) :: zero_like_real64
        zero_like_real64 = real(0, kind=kind(x))
    end function zero_like_real64

    function from_integer_integer(n) result(value)
        integer, intent(in) :: n
        integer :: value
        value = int(n, kind=kind(0))
    end function from_integer_integer

    function from_integer_real64(n) result(value)
        integer, intent(in) :: n
        real(real64) :: value
        value = real(n, kind=real64)
    end function from_integer_real64

    function evaluate_integer(a, b, n) result(value)
        integer, intent(in) :: a, b, n
        integer :: value
        value = (a + b) * int(n, kind=kind(0)) - a / b
    end function evaluate_integer

    function evaluate_real64(a, b, n) result(value)
        real(real64), intent(in) :: a, b
        integer, intent(in) :: n
        real(real64) :: value
        value = (a + b) * real(n, kind=real64) - a / b
    end function evaluate_real64

    function singleton_value(x, n) result(value)
        integer, intent(in) :: x, n
        integer :: value
        value = (x + int(n, kind=kind(0))) * 2 - 1
    end function singleton_value
end module traits_numeric_02_oracle_m

program traits_numeric_02_oracle
    use traits_numeric_02_oracle_m, only: zero_like, from_integer_integer, &
        from_integer_real64, evaluate, singleton_value
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    integer :: i, n
    integer(int32) :: i32
    integer(int64) :: i64
    real(real32) :: r32
    real(real64) :: r64
    complex(real32) :: c32
    complex(real64) :: c64

    call expect_integer(zero_like(9), 0)
    call expect_real64(zero_like(9.0_real64), 0.0_real64)
    call expect_integer(int(0, kind=kind(0)), 0)
    call expect_real64(real(0, kind=real64), 0.0_real64)
    call expect_integer(from_integer_integer(-7), -7)
    call expect_real64(from_integer_real64(-7), -7.0_real64)
    call expect_real64(from_integer_real64(16777217), 16777217.0_real64)
    call expect_integer(evaluate(7, 2, 3), 24)
    call expect_real64(evaluate(7.0_real64, 2.0_real64, 3), 23.5_real64)
    call expect_integer(singleton_value(4, 3), 13)
    call expect_integer(singleton_value(-4, 0), -9)
    if (kind(zero_like(1.0_real64)) /= real64) error stop
    if (kind(from_integer_real64(1)) /= real64) error stop

    n = 16777217
    call expect_real64(dble(a=n), 16777217.0_real64)
    i = 2
    r64 = 2.5_real64
    if (.not. (i > 1 .and. i < 3)) error stop
    if (.not. (r64 > 2.0_real64 .and. r64 < 3.0_real64)) error stop
    i = 3
    r64 = 3.0_real64
    if (i > 1 .and. i < 3) error stop
    if (r64 > 2.0_real64 .and. r64 < 3.0_real64) error stop
    call expect_integer(4 + int(-2, kind=kind(0)), 2)
    call expect_real64(4.5_real64 + real(-2, kind=real64), 2.5_real64)

    n = 5
    i32 = 2_int32 + int(n, kind=int32)
    i64 = 2_int64 + int(n, kind=int64)
    r32 = 2.0_real32 + real(n, kind=real32)
    r64 = 2.0_real64 + real(n, kind=real64)
    c32 = (2.0_real32, -1.0_real32) + cmplx(n, kind=real32)
    c64 = (2.0_real64, -1.0_real64) + cmplx(n, kind=real64)
    if (i32 /= 7_int32 .or. kind(i32) /= int32) error stop
    if (i64 /= 7_int64 .or. kind(i64) /= int64) error stop
    call expect_real32(r32, 7.0_real32)
    call expect_real64(r64, 7.0_real64)
    call expect_real32(abs(c32 - (7.0_real32, -1.0_real32)), 0.0_real32)
    call expect_real64(abs(c64 - (7.0_real64, -1.0_real64)), 0.0_real64)
    if (kind(real(n, kind=real32)) /= real32) error stop
    if (kind(real(n, kind=real64)) /= real64) error stop
    if (kind(cmplx(n, kind=real32)) /= real32) error stop
    if (kind(cmplx(n, kind=real64)) /= real64) error stop

    i = 17
    r64 = 17.5_real64
    call expect_integer(mod(i, 5), 2)
    call expect_integer(mod(-i, 5), -2)
    call expect_real64(mod(r64, 5.0_real64), 2.5_real64)
    call expect_real64(mod(-r64, 5.0_real64), -2.5_real64)
    r64 = 9.0_real64
    call expect_real64(sqrt(r64), 3.0_real64)
    call expect_default_real(real(7), 7.0)
    call expect_default_real(real(1.25_real64), 1.25)
    c64 = (3.0_real64, 4.0_real64)
    call expect_real64(abs(c64), 5.0_real64)
    call expect_real64(real(c64), 3.0_real64)
    if (kind(real(1.25_real64)) /= kind(0.0)) error stop
    if (kind(abs(c64)) /= real64) error stop
    if (kind(real(c64)) /= real64) error stop

contains

    subroutine expect_integer(actual, expected)
        integer, intent(in) :: actual, expected
        if (actual /= expected) error stop
    end subroutine expect_integer

    subroutine expect_default_real(actual, expected)
        real, intent(in) :: actual, expected
        real :: tolerance
        tolerance = 32.0 * epsilon(1.0) * max(1.0, abs(expected))
        if (.not. (abs(actual - expected) <= tolerance)) error stop
    end subroutine expect_default_real

    subroutine expect_real32(actual, expected)
        real(real32), intent(in) :: actual, expected
        real(real32) :: tolerance
        tolerance = 32.0_real32 * epsilon(1.0_real32) * max(1.0_real32, abs(expected))
        if (.not. (abs(actual - expected) <= tolerance)) error stop
    end subroutine expect_real32

    subroutine expect_real64(actual, expected)
        real(real64), intent(in) :: actual, expected
        real(real64) :: tolerance
        tolerance = 32.0_real64 * epsilon(1.0_real64) * max(1.0_real64, abs(expected))
        if (.not. (abs(actual - expected) <= tolerance)) error stop
    end subroutine expect_real64
end program traits_numeric_02_oracle
