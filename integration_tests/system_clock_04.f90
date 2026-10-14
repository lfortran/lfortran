program system_clock_04
    ! COUNT, COUNT_RATE and COUNT_MAX of one call must describe the same clock,
    ! whatever the kind of a real COUNT_RATE (#15007). Only consistency is
    ! checked: absolute counts and rates are processor dependent.
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    integer(int32) :: c4a, c4b, m4
    integer(int64) :: c8a, c8b, m8
    real(real32) :: r4
    real(real64) :: r8, cpu

    call system_clock(c4a, r4, m4)
    call busy(cpu)
    call system_clock(c4b, r4, m4)
    call check("int32 count, real32 rate", int(c4a, int64), int(c4b, int64), &
        real(r4, real64), int(m4, int64), cpu)

    call system_clock(c4a, r8, m4)
    call busy(cpu)
    call system_clock(c4b, r8, m4)
    call check("int32 count, real64 rate", int(c4a, int64), int(c4b, int64), &
        r8, int(m4, int64), cpu)

    call system_clock(c8a, r4, m8)
    call busy(cpu)
    call system_clock(c8b, r4, m8)
    call check("int64 count, real32 rate", c8a, c8b, real(r4, real64), m8, cpu)

    call system_clock(c8a, r8, m8)
    call busy(cpu)
    call system_clock(c8b, r8, m8)
    call check("int64 count, real64 rate", c8a, c8b, r8, m8, cpu)

    call system_clock(count_max=m8)
    call system_clock(count=c8a, count_rate=r4)
    call busy(cpu)
    call system_clock(count=c8b, count_rate=r4)
    call check("int64 count, real32 rate, no count_max", c8a, c8b, &
        real(r4, real64), m8, cpu)

    call system_clock(count_max=m4)
    call system_clock(count=c4a, count_rate=r8)
    call busy(cpu)
    call system_clock(count=c4b, count_rate=r8)
    call check("int32 count, real64 rate, no count_max", int(c4a, int64), &
        int(c4b, int64), r8, int(m4, int64), cpu)

    print *, "PASS"

contains

    subroutine busy(cpu)
        real(real64), intent(out) :: cpu
        real(real64) :: t0, t1
        call cpu_time(t0)
        do
            call cpu_time(t1)
            if (t1 - t0 >= 0.02_real64) exit
        end do
        cpu = t1 - t0
    end subroutine

    subroutine check(name, ca, cb, rate, cmax, cpu)
        character(*), intent(in) :: name
        integer(int64), intent(in) :: ca, cb, cmax
        real(real64), intent(in) :: rate, cpu
        integer(int64) :: diff
        real(real64) :: elapsed
        if (rate <= 0) error stop "count_rate must be positive"
        if (cmax <= 0) error stop "count_max must be positive"
        if (ca < 0 .or. ca > cmax) error stop "count outside 0..count_max"
        if (cb < 0 .or. cb > cmax) error stop "count outside 0..count_max"
        diff = cb - ca
        if (diff < 0) diff = diff + cmax + 1
        elapsed = real(diff, real64) / rate
        print *, name, ": elapsed =", elapsed, ", cpu =", cpu
        if (elapsed < 0.25_real64 * cpu .or. elapsed > 1000 * cpu) then
            error stop "elapsed system_clock time inconsistent with cpu_time"
        end if
    end subroutine

end program system_clock_04
