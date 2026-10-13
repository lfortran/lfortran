! LFortran traits extension test; GFortran does not accept this syntax.
! Complex results of closed runtime slots across separately compiled modules:
! the contract module declares the integer | complex(real32) | complex(real64)
! type set, the provider fills the member slots (with nopass, first and
! non-first passed-object bindings, static recursion over sections and an
! ordinary message returning complex values of both kinds) and the consumer
! and this driver reach them only through class(...) views, with keyword
! arguments and inferred and explicit type arguments.
! traits_runtime_numeric_separate_01.py --case complex freezes and hides the
! provider before compiling the clients.
program traits_runtime_numeric_separate_02
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_runtime_numeric_separate_02_contracts_m, only: ISum, IAverager, &
        IPhase, make_sum, make_averager, make_phase
    use traits_runtime_numeric_separate_02_consumer_m, only: integer_total, &
        complex32_total, complex64_total, complex32_mean, complex64_mean, spread, &
        quarter_turns, mirrored
    implicit none
    integer, parameter :: lengths(6) = [0, 1, 2, 5, 6, 8]
    integer :: xi(8), k, choice, factor, n, i, checks
    complex(real32) :: c4(8)
    complex(real64) :: c8(8)
    class(ISum), allocatable :: summer
    class(IAverager), allocatable :: averager
    class(IPhase), allocatable :: phase

    checks = 0
    xi = [5, -2, 7, 1, 4, -3, 9, 6]
    c4 = [(0.5_real32, 1.0_real32), (2.25_real32, -0.5_real32), &
        (-1.75_real32, 0.0_real32), (3.0_real32, 2.5_real32), &
        (-0.125_real32, -1.25_real32), (4.5_real32, 0.75_real32), &
        (1.25_real32, -3.0_real32), (-2.0_real32, 0.125_real32)]
    c8 = [(1.5_real64, -0.25_real64), (-2.0_real64, 3.5_real64), &
        (0.75_real64, 0.5_real64), (4.0_real64, -1.0_real64), &
        (-0.5_real64, 2.25_real64), (2.5_real64, 0.0_real64), &
        (-3.25_real64, -1.5_real64), (1.0_real64, 1.0_real64)]
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
            call check_c4(complex32_total(summer, c4(n:1:-1)), &
                factor * reference_c4(c4(1:n)), .true.)
            call check_c8(complex64_total(summer, c8(1:n:2)), &
                factor * reference_c8(c8(1:n:2)), .true.)
            call check_c4(summer%sum(c4(1:n:3)), factor * reference_c4(c4(1:n:3)), .true.)
            call check_c8(summer%sum{complex(real64)}(x=c8(n:1:-1)), &
                factor * reference_c8(c8(1:n)), .true.)
            if (n > 0) then
                call check_i(averager%average(xi(n:1:-1)), &
                    (factor * reference_i(xi(1:n))) / n)
                call check_c4(complex32_mean(averager, c4(1:n)), &
                    factor * reference_c4(c4(1:n)) / cmplx(n, 0, real32), .false.)
                call check_c8(complex64_mean(averager, c8(1:n)), &
                    factor * reference_c8(c8(1:n)) / cmplx(n, 0, real64), .false.)
                call check_c8(averager%average(c8(n:1:-1)), &
                    factor * reference_c8(c8(1:n)) / cmplx(n, 0, real64), .false.)
            end if
        end do
        call check_i(spread(averager, xi), (factor * reference_i(xi)) / 8 - &
            (factor * reference_i(xi(1:8:2))) / 4)
        call check_c8(spread(averager, c8), factor * reference_c8(c8) / 8.0_real64 - &
            factor * reference_c8(c8(1:8:2)) / 4.0_real64, .false.)
        deallocate(summer, averager)
    end do

    call make_phase(phase)
    call check_c8(quarter_turns(phase, (1.5_real64, -2.0_real64), 1), &
        (2.0_real64, 1.5_real64), .true.)
    call check_c8(quarter_turns(phase, (1.5_real64, -2.0_real64), 2), &
        (-1.5_real64, 2.0_real64), .true.)
    call check_c8(phase%turn((1.5_real64, -2.0_real64), 7), &
        (-2.0_real64, -1.5_real64), .true.)
    call check_c8(phase%turn(quarters=4, z=(0.25_real64, 0.5_real64)), &
        (0.25_real64, 0.5_real64), .true.)
    call check_c4(mirrored(phase, (0.5_real32, 0.25_real32)), &
        (0.5_real32, -0.25_real32), .true.)
    call check_c4(phase%mirror(z=c4(2)), (2.25_real32, 0.5_real32), .true.)

    if (checks /= 162) error stop 3
    print '(a,i0,a)', 'complex separate: frozen provider, contract-only consumer, ', &
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

    function reference_c4(x) result(s)
        complex(real32), intent(in) :: x(:)
        complex(real32) :: s
        integer :: j
        s = (0.0_real32, 0.0_real32)
        do j = 1, size(x)
            s = s + x(j)
        end do
    end function reference_c4

    function reference_c8(x) result(s)
        complex(real64), intent(in) :: x(:)
        complex(real64) :: s
        integer :: j
        s = (0.0_real64, 0.0_real64)
        do j = 1, size(x)
            s = s + x(j)
        end do
    end function reference_c8

    subroutine check_i(actual, expected)
        integer, intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,i0,a,i0)', 'check ', checks, ': ', actual, ' /= ', expected
            error stop 1
        end if
    end subroutine check_i

    ! Sums of the dyadic data are exact in every order; quotients by other
    ! lengths are compared to a few units in the last place.
    subroutine check_c4(actual, expected, exact)
        complex(real32), intent(in) :: actual, expected
        logical,         intent(in) :: exact
        checks = checks + 1
        if (exact .and. actual == expected) return
        if (.not. exact .and. abs(actual - expected) <= &
                8 * epsilon(1.0_real32) * max(1.0_real32, abs(expected))) return
        print '(a,i0,a,4es16.8)', 'check ', checks, ': ', actual, expected
        error stop 2
    end subroutine check_c4

    subroutine check_c8(actual, expected, exact)
        complex(real64), intent(in) :: actual, expected
        logical,         intent(in) :: exact
        checks = checks + 1
        if (exact .and. actual == expected) return
        if (.not. exact .and. abs(actual - expected) <= &
                8 * epsilon(1.0_real64) * max(1.0_real64, abs(expected))) return
        print '(a,i0,a,4es24.16)', 'check ', checks, ': ', actual, expected
        error stop 2
    end subroutine check_c8
end program traits_runtime_numeric_separate_02
