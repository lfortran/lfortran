! The driver sees the contract, the intrinsic acknowledgements and the late
! module, never the provider. Runtime averagers and sums built by the frozen
! provider must handle integer, real(real64), real(real32) and Rational
! arrays, empty arrays, recursion over sections, a reversed strided section
! and forwarded type parameters (paper sect:ocp_conformance: a new module
! implementing INumeric is sufficient).
program traits_protocol_separate_01
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_protocol_separate_01_contracts_m, only: ISum, IAverager, &
        make_sum, make_averager
    use traits_protocol_separate_01_intrinsics_m
    ! Unrestricted: its real(real32) acknowledgement has no name to import.
    use traits_protocol_separate_01_late_m
    implicit none
    integer, parameter :: hnum(0:6) = [0, 1, 3, 11, 25, 137, 49]
    integer, parameter :: hden(0:6) = [1, 1, 2, 6, 12, 60, 20]
    class(ISum), allocatable :: summer
    class(IAverager), allocatable :: averager
    type(Rational) :: q(3), h(6), rq
    integer :: checks, choice, n, i, iv(9)
    real(real64) :: dv(5)
    real(real32) :: sv(5)

    checks = 0
    iv = [(i, i = 1, 9)]
    dv = [(real(i, real64), i = 1, 5)]
    sv = [(real(i, real32), i = 1, 5)]
    q = [Rational(1, 2), Rational(1, 3), Rational(1, 6)]
    h = [(Rational(1, i), i = 1, 6)]

    do choice = 1, 3
        call make_averager(choice, averager)
        call check(averager%average(iv(1:5)) == 3, 1)
        call check(averager%average(dv) == 3.0_real64, 2)
        call check(averager%average(sv) == 3.0_real32, 3)
        call check(kind(averager%average(sv)) == real32, 4)
        rq = averager%average(q)
        call check(rq%num == 1 .and. rq%den == 3, 5)
        call check(two_way_mean(averager, iv(1:5)) == 6, 6)
        rq = two_way_mean(averager, q)
        call check(rq%num == 2 .and. rq%den == 3, 7)

        call make_sum(choice, summer)
        do n = 0, 9
            call check(summer%sum(iv(1:n)) == n * (n + 1) / 2, 8)
        end do
        do n = 0, 6
            rq = summer%sum(h(1:n))
            call check(rq%num == hnum(n) .and. rq%den == hden(n), 9)
        end do
        call check(summer%sum(dv(5:1:-2)) == 9.0_real64, 10)
    end do

    print '(a,i0)', 'traits_protocol_separate_01 checks: ', checks
    if (checks /= 75) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0,a,i0)', 'traits_protocol_separate_01 failed check ', id, &
                ' for choice ', choice
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_protocol_separate_01
