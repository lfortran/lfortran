program intrinsics_319
    integer(4) :: count4, count_max4, count_rate4
    integer(8) :: count8, count_max8, count_rate8
    real(4) :: count_rate_r4
    real(8) :: count_rate_r8
    call system_clock(count4, count_rate4, count_max4)
    print *, count4, count_rate4, count_max4
    call system_clock(count8, count_rate8, count_max8)
    print *, count8, count_rate8, count_max8
    call system_clock(count4, count_rate_r4, count_max4)
    print *, count4, count_rate_r4, count_max4
    call system_clock(count8, count_rate_r8, count_max8)
    print *, count8, count_rate_r8, count_max8
end program