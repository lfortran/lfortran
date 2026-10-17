program system_clock_02
    use iso_fortran_env, only: int64, real64
    implicit none
    integer :: count, rate, maximum
    integer(int64) :: count8, rate8, maximum8
    real :: real_rate
    real(real64) :: real_rate8

    call system_clock(count, rate, maximum)
    call check_default()
    count = -1
    call system_clock(count=count)
    call check_default()
    rate = 0
    call system_clock(count_rate=rate)
    call check_default()
    maximum = 0
    call system_clock(count_max=maximum)
    call check_default()
    call system_clock(count=count, count_rate=rate)
    call check_default()
    call system_clock(count=count, count_max=maximum)
    call check_default()
    call system_clock(count_rate=rate, count_max=maximum)
    call check_default()
    call system_clock(count_rate=rate, count_max=maximum, count=count)
    call check_default()

    call system_clock(count8, rate8, maximum8)
    call check_int64()
    count8 = -1_int64
    call system_clock(count=count8)
    call check_int64()
    rate8 = 0_int64
    call system_clock(count_rate=rate8)
    call check_int64()
    maximum8 = 0_int64
    call system_clock(count_max=maximum8)
    call check_int64()
    call system_clock(count=count8, count_rate=rate8)
    call check_int64()
    call system_clock(count=count8, count_max=maximum8)
    call check_int64()
    call system_clock(count_rate=rate8, count_max=maximum8)
    call check_int64()
    call system_clock(count_rate=rate8, count_max=maximum8, count=count8)
    call check_int64()

    call system_clock(count, real_rate, maximum)
    if (real_rate <= 0.0 .or. real_rate /= real(rate)) error stop
    call check_default()
    call system_clock(count8, real_rate8, maximum8)
    if (real_rate8 <= 0.0_real64 .or. real_rate8 /= real(rate8, real64)) error stop
    call check_int64()
    call system_clock(count_rate=real_rate)
    if (real_rate <= 0.0 .or. real_rate /= real(rate)) error stop
    call system_clock(count_rate=real_rate8)
    if (real_rate8 <= 0.0_real64 .or. real_rate8 /= real(rate8, real64)) error stop
contains
    subroutine check_default()
        if (rate <= 0 .or. maximum <= 0) error stop
        if (count < 0 .or. count > maximum) error stop
    end subroutine

    subroutine check_int64()
        if (rate8 <= 0_int64 .or. maximum8 <= 0_int64) error stop
        if (count8 < 0_int64 .or. count8 > maximum8) error stop
    end subroutine
end program
