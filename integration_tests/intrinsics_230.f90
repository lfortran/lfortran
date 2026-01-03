program intrinsic_230
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    implicit none
    real(dp), parameter :: res0_j0(2) = bessel_j0([3.3398640543782485_dp, 3.3398640543782485_dp])
    real(dp), parameter :: res0_j1(2) = bessel_j1([3.3398640543782485_dp, 5.039184326269492_dp])
    real(dp), parameter :: res0_jn(2) = bessel_jn(1, [3.3398640543782485_dp, 5.039184326269492_dp])
    real(dp), parameter :: res0_y0(2) = bessel_y0([3.3398640543782485_dp, 3.3398640543782485_dp])
    real(dp), parameter :: res0_y1(2) = bessel_y1([3.3398640543782485_dp, 5.039184326269492_dp])
    real(dp), parameter :: res0_yn(2) = bessel_yn(1, [3.3398640543782485_dp, 5.039184326269492_dp])

    real(dp) :: res1_j0(2)
    real(dp) :: res1_j1(2)
    real(dp) :: res1_jn(2)
    real(dp) :: res1_y0(2)
    real(dp) :: res1_y1(2)
    real(dp) :: res1_yn(2)

    res1_j0 = bessel_j0([3.3398640543782485_dp, 3.3398640543782485_dp])
    res1_j1 = bessel_j1([3.3398640543782485_dp, 5.039184326269492_dp])
    res1_jn = bessel_jn(1, [3.3398640543782485_dp, 5.039184326269492_dp])
    res1_y0 = bessel_y0([3.3398640543782485_dp, 3.3398640543782485_dp])
    res1_y1 = bessel_y1([3.3398640543782485_dp, 5.039184326269492_dp])
    res1_yn = bessel_yn(1, [3.3398640543782485_dp, 5.039184326269492_dp])

    print*, res0_j0
    if (abs(res0_j0(1) - (-0.35276533724012676)) > 1e-6) error stop
    if (abs(res0_j0(2) - (-0.35276533724012676)) > 1e-6) error stop

    print*, res0_j1
    if (abs(res0_j1(1) - 0.20421621687655642) > 1e-6) error stop
    if (abs(res0_j1(2) - (-0.33171193864355270)) > 1e-6) error stop

    print*, res0_jn
    if (abs(res0_jn(1) - 0.20421621687655642) > 1e-6) error stop
    if (abs(res0_jn(2) - (-0.33171193864355270)) > 1e-6) error stop

    print*, res0_y0
    if (abs(res0_y0(1) - 0.25351438512601238) > 1e-6) error stop
    if (abs(res0_y0(2) - 0.25351438512601238) > 1e-6) error stop

    print*, res0_y1
    if (abs(res0_y1(1) - 0.39357815895597936) > 1e-6) error stop
    if (abs(res0_y1(2) - 0.13456148643731775) > 1e-6) error stop

    print*, res0_yn
    if (abs(res0_yn(1) - 0.39357815895597936) > 1e-6) error stop
    if (abs(res0_yn(2) - 0.13456148643731775) > 1e-6) error stop
    
    print*, res1_j0
    if (abs(res1_j0(1) - (-0.35276533724012676)) > 1e-6) error stop
    if (abs(res1_j0(2) - (-0.35276533724012676)) > 1e-6) error stop

    print*, res1_j1
    if (abs(res1_j1(1) - 0.20421621687655642) > 1e-6) error stop
    if (abs(res1_j1(2) - (-0.33171193864355270)) > 1e-6) error stop

    print*, res1_jn
    if (abs(res1_jn(1) - 0.20421621687655642) > 1e-6) error stop
    if (abs(res1_jn(2) - (-0.33171193864355270)) > 1e-6) error stop

    print*, res1_y0
    if (abs(res1_y0(1) - 0.25351438512601238) > 1e-6) error stop
    if (abs(res1_y0(2) - 0.25351438512601238) > 1e-6) error stop

    print*, res1_y1
    if (abs(res1_y1(1) - 0.39357815895597936) > 1e-6) error stop
    if (abs(res1_y1(2) - 0.13456148643731775) > 1e-6) error stop

    print*, res1_yn
    if (abs(res1_yn(1) - 0.39357815895597936) > 1e-6) error stop
    if (abs(res1_yn(2) - 0.13456148643731775) > 1e-6) error stop

end program
