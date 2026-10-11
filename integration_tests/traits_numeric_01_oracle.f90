module traits_numeric_01_oracle_m
    use iso_fortran_env, only: real64
    implicit none

    interface numeric_sum
        module procedure sum_integer, sum_real64
    end interface numeric_sum

    interface numeric_average
        module procedure average_integer, average_real64
    end interface numeric_average

contains

    function sum_integer(x) result(s)
        integer, intent(in) :: x(:)
        integer :: s, i
        s = int(0, kind=kind(0))
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function sum_integer

    function sum_real64(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: i
        s = real(0, kind=real64)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function sum_real64

    function average_integer(x) result(a)
        integer, intent(in) :: x(:)
        integer :: a
        a = numeric_sum(x) / int(size(x), kind=kind(0))
    end function average_integer

    function average_real64(x) result(a)
        real(real64), intent(in) :: x(:)
        real(real64) :: a
        a = numeric_sum(x) / real(size(x), kind=real64)
    end function average_real64
end module traits_numeric_01_oracle_m

program traits_numeric_01_oracle
    use traits_numeric_01_oracle_m, only: numeric_sum, numeric_average, &
        sum_integer, sum_real64, average_integer, average_real64
    use iso_fortran_env, only: real64
    implicit none
    integer :: integers(3), empty_integer(0)
    real(real64) :: reals(3), empty_real(0)

    integers = [1, 2, 4]
    reals = [1.0_real64, 2.0_real64, 4.0_real64]
    call expect_integer(numeric_sum(integers), 7)
    call expect_integer(sum_integer(integers), 7)
    call expect_real64(numeric_sum(reals), 7.0_real64)
    call expect_real64(sum_real64(reals), 7.0_real64)
    call expect_integer(numeric_average(integers), 2)
    call expect_integer(average_integer(integers), 2)
    call expect_real64(numeric_average(reals), 7.0_real64 / 3.0_real64)
    call expect_real64(average_real64(reals), 7.0_real64 / 3.0_real64)
    call expect_integer(numeric_sum(empty_integer), 0)
    call expect_real64(numeric_sum(empty_real), 0.0_real64)
    call expect_integer(numeric_sum(integers(1:3:2)), 5)
    call expect_real64(numeric_sum(reals(1:3:2)), 5.0_real64)
    call expect_integer(numeric_average(integers(1:3:2)), 2)
    call expect_real64(numeric_average(reals(1:3:2)), 2.5_real64)
    call expect_integer(numeric_average(-integers), -2)
    call expect_real64(numeric_average(-reals), -7.0_real64 / 3.0_real64)
    if (kind(numeric_sum(integers)) /= kind(0)) error stop
    if (kind(numeric_average(reals)) /= real64) error stop

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
end program traits_numeric_01_oracle
