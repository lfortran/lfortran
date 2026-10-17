program system_clock_03
    use iso_fortran_env, only: int64
    implicit none
    integer :: count, maximum, i
    integer(int64) :: count8, maximum8

    call system_clock(count_max=maximum)
    call system_clock(count_max=maximum8)
    if (maximum <= 0) error stop
    if (maximum8 <= 0_int64) error stop
    do i = 1, 10
        call system_clock(count=count)
        call system_clock(count=count8)
        if (count < 0 .or. count > maximum) error stop
        if (count8 < 0_int64 .or. count8 > maximum8) error stop
    end do
end program
