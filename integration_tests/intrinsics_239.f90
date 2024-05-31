program intrinsics_239
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    
    real(sp) :: x = 0.17_sp
    real(dp) :: y = 0.29_dp
    real(sp), parameter :: x1 = erfc_scaled(0.11_sp)
    real(dp), parameter :: y1 = erfc_scaled(0.34_dp)
    real(sp) :: x2(3) = [0.11_sp, 0.12_sp, 0.13_sp]
    real(dp) :: y2(3) = [0.34_dp, 0.35_dp, 0.36_dp]
    real(sp), parameter :: x3(3) = erfc_scaled([0.11_sp, 0.12_sp, 0.13_sp])
    real(dp), parameter :: y3(3) = erfc_scaled([0.34_dp, 0.35_dp, 0.36_dp])

    print *, erfc_scaled(x)
    if ( abs(erfc_scaled(x) - 0.833758295_sp) > 1e-6 ) error stop
    print *, erfc_scaled(y)
    if ( abs(erfc_scaled(y) - 0.74152871645209784_dp) > 1e-12 ) error stop
    print *, x1
    if ( abs(x1 - 0.887045681_sp) > 1e-6 ) error stop
    print *, y1
    if ( abs(y1 - 0.70791780660314296_dp) > 1e-12 ) error stop

    print *, erfc_scaled(x2)
    if ( any(abs(erfc_scaled(x2) - [0.887045741_sp, 0.877791286_sp, 0.868690372_sp]) > 1e-6) ) error stop
    print *, erfc_scaled(y2)
    if ( any(abs(erfc_scaled(y2) - [0.70791780660314296_dp, 0.70149633111958920_dp, 0.69517054536879996_dp]) > 1e-12) ) error stop
    print *, x3
    if ( any(abs(x3 - [0.887045741_sp, 0.877791286_sp, 0.868690372_sp]) > 1e-6) ) error stop
    print *, y3
    if ( any(abs(y3 - [0.70791780660314296_dp, 0.70149633111958909_dp, 0.69517054536879996_dp]) > 1e-12) ) error stop

end program