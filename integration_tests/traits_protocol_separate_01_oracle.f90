! Standard-Fortran oracle for traits_protocol_separate_01, part 5 of 5: the
! driver, which sees the contracts, the intrinsic wrappers and the late
! module, never the provider. The checks, their order and the summary line
! match the extension test.
program traits_protocol_separate_01_oracle
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_protocol_separate_01_oracle_contracts_m, only: ISum, IAverager, &
        make_sum, make_averager
    use traits_protocol_separate_01_oracle_intrinsics_m, only: IntegerNumeric, &
        Real64Numeric, integer_of, real64_of
    use traits_protocol_separate_01_oracle_late_m, only: RationalNumeric, Real32Numeric, &
        rational_of, real32_of, two_way_mean
    implicit none
    integer, parameter :: hnum(0:6) = [0, 1, 3, 11, 25, 137, 49]
    integer, parameter :: hden(0:6) = [1, 1, 2, 6, 12, 60, 20]
    class(ISum), allocatable :: summer
    class(IAverager), allocatable :: averager
    type(RationalNumeric) :: q(3), h(6), rq
    type(IntegerNumeric) :: iv(9)
    type(Real64Numeric) :: dv(5)
    type(Real32Numeric) :: sv(5)
    integer :: checks, choice, n, i

    checks = 0
    iv = [(IntegerNumeric(i), i = 1, 9)]
    dv = [(Real64Numeric(real(i, real64)), i = 1, 5)]
    sv = [(Real32Numeric(real(i, real32)), i = 1, 5)]
    q = [RationalNumeric(1, 2), RationalNumeric(1, 3), RationalNumeric(1, 6)]
    h = [(RationalNumeric(1, i), i = 1, 6)]

    do choice = 1, 3
        call make_averager(choice, averager)
        call check(integer_of(averager%average(iv(1:5))) == 3, 1)
        call check(real64_of(averager%average(dv)) == 3.0_real64, 2)
        call check(real32_of(averager%average(sv)) == 3.0_real32, 3)
        call check(kind(real32_of(averager%average(sv))) == real32, 4)
        rq = rational_of(averager%average(q))
        call check(rq%num == 1 .and. rq%den == 3, 5)
        call check(integer_of(two_way_mean(averager, iv(1:5))) == 6, 6)
        rq = rational_of(two_way_mean(averager, q))
        call check(rq%num == 2 .and. rq%den == 3, 7)

        call make_sum(choice, summer)
        do n = 0, 9
            call check(integer_of(summer%sum(iv(1:n))) == n * (n + 1) / 2, 8)
        end do
        do n = 0, 6
            rq = rational_of(summer%sum(h(1:n)))
            call check(rq%num == hnum(n) .and. rq%den == hden(n), 9)
        end do
        call check(real64_of(summer%sum(dv(5:1:-2))) == 9.0_real64, 10)
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
end program traits_protocol_separate_01_oracle
