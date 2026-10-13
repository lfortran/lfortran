program traits_runtime_numeric_separate_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_separate_01_contracts_m, only: ISum, IAverager, &
        make_sum, make_averager
    use traits_runtime_numeric_separate_01_consumer_m, only: integer_total, &
        real64_total, integer_mean, real64_mean, spread
    implicit none
    integer, parameter :: lengths(6) = [0, 1, 2, 5, 6, 8]
    integer :: xi(8), k, choice, factor, n, i, checks
    real(real64) :: xr(8)
    class(ISum), allocatable :: summer
    class(IAverager), allocatable :: averager

    checks = 0
    xi = [5, -2, 7, 1, 4, -3, 9, 6]
    xr = [0.5_real64, 2.25_real64, -1.75_real64, 3.0_real64, -0.125_real64, &
        4.5_real64, 1.25_real64, -2.0_real64]
    do k = 1, 3
        choice = k
        if (command_argument_count() > 0) choice = 4 - k
        factor = 1
        if (choice == 3) factor = 3
        call make_sum(choice, summer)
        call make_averager(choice, averager)
        do i = 1, size(lengths)
            n = lengths(i)
            call check_i(integer_total(summer, xi(1:n)), factor * reference_i(xi(1:n)))
            call check_r(real64_total(summer, xr(n:1:-1)), factor * reference_r(xr(1:n)))
            call check_i(summer%sum(xi(1:n:3)), factor * reference_i(xi(1:n:3)))
            call check_r(summer%sum{real(real64)}(xr(1:n:2)), &
                factor * reference_r(xr(1:n:2)))
            if (n > 0) then
                call check_i(integer_mean(averager, xi(1:n)), &
                    (factor * reference_i(xi(1:n))) / n)
                call check_r(real64_mean(averager, xr(1:n)), &
                    (factor * reference_r(xr(1:n))) / real(n, real64))
                call check_i(averager%average(xi(n:1:-1)), &
                    (factor * reference_i(xi(1:n))) / n)
            end if
        end do
        call check_i(spread(averager, xi), (factor * reference_i(xi)) / 8 - &
            (factor * reference_i(xi(1:8:2))) / 4)
        call check_r(spread(averager, xr), (factor * reference_r(xr)) / 8.0_real64 - &
            (factor * reference_r(xr(1:8:2))) / 4.0_real64)
        deallocate(summer, averager)
    end do
    print '(a,i0,a)', 'numeric separate: frozen provider, contract-only consumer, ', &
        checks, ' checks passed'

contains

    function reference_i(x) result(s)
        integer, intent(in) :: x(:)
        integer :: s, j
        s = 0
        do j = 1, size(x)
            s = s + x(j)
        end do
    end function reference_i

    function reference_r(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: j
        s = 0.0_real64
        do j = 1, size(x)
            s = s + x(j)
        end do
    end function reference_r

    subroutine check_i(actual, expected)
        integer, intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,i0,a,i0)', 'check ', checks, ': ', actual, ' /= ', expected
            error stop 1
        end if
    end subroutine check_i

    ! Dyadic data: every summation order and each power-of-two division is exact.
    subroutine check_r(actual, expected)
        real(real64), intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,es24.16,a,es24.16)', 'check ', checks, ': ', actual, &
                ' /= ', expected
            error stop 2
        end if
    end subroutine check_r
end program traits_runtime_numeric_separate_01
