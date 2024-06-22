program intrinsics_269
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    integer :: i

    real(dp) :: arg_x(15)
    real(sp) :: arg_y(15)
    real(dp) :: res_x(15)
    real(sp) :: res_y(15)
    real(dp) :: expected_res_x(15)
    real(sp) :: expected_res_y(15)
    real(dp) :: expected_x(10)
    real(sp) :: expected_y(10)

    real(dp) :: a = 0.5178181202_dp
    real(sp) :: b = -0.072816_sp
    real(dp) :: c = -0.5178181202_dp
    real(sp) :: d = 0.072816_sp

    real(dp), parameter :: res_dp(10) = acos([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = acos([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])

    expected_x = [2.2500188380589141_dp, 2.1144352537866755_dp, 1.8677118371228034_dp, &
            0.89157381553087900_dp, 1.0271573998031178_dp, 1.2738808164669899_dp, 0.0000000000000000_dp, &
            0.59283005557227886_dp, 0.30874327684982894_dp, 1.8148377145860481_dp]

    expected_y = [2.25001884_sp, 2.11443520_sp, 1.86771178_sp, &
            0.891573846_sp, 1.02715743_sp, 1.27388084_sp, 0.00000000_sp, &
            0.592830062_sp, 0.308743358_sp, 1.81483769_sp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    res_x = acos(arg_x)
    res_y = acos(arg_y)

    expected_res_x = [2.3174990949497780_dp, 2.1455629489588066_dp, 1.8722571937442951_dp, &
        0.82409355863454103_dp, 0.99602970463098672_dp, 1.2693354598454980_dp, 0.96332095907306292_dp, &
        1.4927508492754979_dp, 1.3056473413839662_dp, 1.8173273965123287_dp, &
        2.3864340230054486_dp, 0.96332095907306292_dp, 1.4927508492754979_dp, 1.3056473413839662_dp, &
        1.8173273965123287_dp]

    expected_res_y = [2.31749892_sp, 2.14556289_sp, 1.87225723_sp, &
            0.824093640_sp, 0.996029735_sp, 1.26933551_sp, 0.963320911_sp, &
            1.49275088_sp, 1.30564737_sp, 1.81732738_sp, 2.38643408_sp, &
            0.963320911_sp, 1.49275088_sp, 1.30564737_sp, &
            1.81732738_sp]

    do i = 1, size(res_dp)
        print *, res_dp(i)
        if (abs(res_dp(i) - expected_x(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(res_x)
        print *, res_x(i)
        if (abs(res_x(i)) - abs(expected_res_x(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(res_sp)
        print *, res_sp(i)
        if (abs(res_sp(i) - expected_y(i)) > 1e-5) error stop
    end do

    do i = 1, size(res_y)
        print *, res_y(i)
        if (abs(res_y(i)) - abs(expected_res_y(i)) > 1e-5) error stop
    end do

    print *, acos(a)
    if (abs(acos(a)) - 1.0264977928984282_dp > 1e-12) error stop

    print *, acos(b)
    if (acos(b) - (1.64367688_sp) > 1e-5) error stop

    print *, acos(c)
    if (acos(c) - (2.1150948606913649_dp) > 1e-12) error stop

    print *, acos(d)
    if (acos(d) - (1.49791586_sp) > 1e-5) error stop

end program