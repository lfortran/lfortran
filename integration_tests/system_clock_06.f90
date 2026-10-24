program system_clock_06
    ! Fortran 2018 allows SYSTEM_CLOCK integer arguments of kinds 1 and 2
    ! (Fortran 2023 prohibits them). Whether a clock is available for such a
    ! call is processor dependent, but the values must be consistent: either
    ! a clock (COUNT_RATE > 0, 0 <= COUNT <= COUNT_MAX) or no clock
    ! (COUNT = -HUGE(COUNT), COUNT_RATE = 0, COUNT_MAX = 0) (#15073).
    use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    integer(int8) :: c1, r1, m1
    integer(int16) :: c2, r2, m2
    integer(int32) :: c4, r4, m4
    integer(int64) :: m8
    real(real32) :: rr4
    real(real64) :: rr8

    call system_clock(c1, r1, m1)
    call check1(c1, r1, m1)
    call system_clock(c2, r2, m2)
    call check2(c2, r2, m2)

    call system_clock(count_max=m1)
    if (m1 < 0) error stop "int8 count_max only: negative"
    call system_clock(count_max=m2)
    if (m2 < 0) error stop "int16 count_max only: negative"

    call system_clock(c1, r1)
    call check1(c1, r1, merge(huge(m1), 0_int8, r1 > 0))
    call system_clock(c2, r2)
    call check2(c2, r2, merge(huge(m2), 0_int16, r2 > 0))

    call system_clock(count_rate=r1)
    if (r1 < 0) error stop "int8 count_rate only: negative"
    call system_clock(count_rate=r2)
    if (r2 < 0) error stop "int16 count_rate only: negative"

    call system_clock(c2, rr4)
    if (rr4 < 0) error stop "int16 count, real32 rate: negative rate"
    if (rr4 == 0 .and. c2 /= -huge(c2)) error stop "int16 count, real32 rate"
    call system_clock(c2, rr8)
    if (rr8 < 0) error stop "int16 count, real64 rate: negative rate"
    if (rr8 == 0 .and. c2 /= -huge(c2)) error stop "int16 count, real64 rate"

    call k1_then_k2()
    call k2_then_k4()

    call system_clock(c2, count_max=m8)
    if (m8 < 0) error stop "int16 count, int64 max: negative max"
    if (m8 == 0) then
        if (c2 /= -huge(c2)) error stop "int16 count, int64 max: no clock"
    else
        if (c2 < 0 .or. c2 > m8) error stop "int16 count, int64 max: range"
    end if
    call system_clock(c2, r4, m8)
    if (m8 < 0) error stop "int16 count, int32 rate, int64 max: negative max"
    if (m8 == 0) then
        if (c2 /= -huge(c2) .or. r4 /= 0) error stop "int16/int32/int64: no clock"
    else
        if (c2 < 0 .or. c2 > m8 .or. r4 <= 0) error stop "int16/int32/int64: range"
    end if

    ! Default integer kind calls still get a clock.
    call system_clock(c4, r4, m4)
    if (m4 <= 0 .or. r4 <= 0 .or. c4 < 0 .or. c4 > m4) error stop "int32 clock"

    print *, "PASS"

contains

    subroutine k1_then_k2()
        integer(int8) :: a1, b1, x1
        integer(int16) :: a2, b2, x2
        call system_clock(a1, b1, x1)
        call check1(a1, b1, x1)
        call system_clock(a2, b2, x2)
        call check2(a2, b2, x2)
    end subroutine

    subroutine k2_then_k4()
        integer(int16) :: a2, b2, x2
        integer(int32) :: a4, b4, x4
        call system_clock(a2, b2, x2)
        call check2(a2, b2, x2)
        call system_clock(a4, b4, x4)
        if (x4 <= 0 .or. b4 <= 0 .or. a4 < 0 .or. a4 > x4) error stop "k2_then_k4: int32 clock"
    end subroutine

    subroutine check1(c, r, m)
        integer(int8), intent(in) :: c, r, m
        if (m < 0) error stop "int8: negative count_max"
        if (m == 0) then
            if (c /= -huge(c) .or. r /= 0) error stop "int8: inconsistent no clock"
        else
            if (c < 0 .or. c > m .or. r <= 0) error stop "int8: inconsistent clock"
        end if
    end subroutine

    subroutine check2(c, r, m)
        integer(int16), intent(in) :: c, r, m
        if (m < 0) error stop "int16: negative count_max"
        if (m == 0) then
            if (c /= -huge(c) .or. r /= 0) error stop "int16: inconsistent no clock"
        else
            if (c < 0 .or. c > m .or. r <= 0) error stop "int16: inconsistent clock"
        end if
    end subroutine

end program system_clock_06
