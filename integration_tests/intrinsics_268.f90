program intrinsics_268
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

         
    real(dp), parameter :: res_dp(10) = asin([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = asin([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])

    expected_x = [-0.67922251126401767_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 1.5707963267948966_dp, &
        0.97796627122261770_dp, 1.2620530499450677_dp, -0.24404138779115148_dp]


    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    expected_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
            0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 1.57079637_sp, &
            0.977966249_sp, 1.26205301_sp, -0.244041398_sp]

    res_x = asin(arg_x)
    res_y = asin(arg_y)

    expected_res_x = [-0.84309112198096470_dp, -0.61231892714946035_dp, -0.30622442822953339_dp, &
        0.84309112198919456_dp, 0.61231892714946035_dp, 0.30622442822953339_dp, 0.65287843094856091_dp, &
        7.8124925903421710E-002_dp, 0.26835843496713735_dp, -0.24909920725508472_dp, &
        -0.95383054718869975_dp, 0.65287843094856091_dp, 7.8124925903421710E-002_dp, &
        0.26835843496713735_dp, -0.24909920725508472_dp]

    expected_res_y = [-0.843091071_sp, -0.612318873_sp, -0.306224436_sp, &
        0.843091071_sp, 0.612318873_sp, 0.306224436_sp, 0.652878463_sp, &
        7.81249031E-02_sp, 0.268358380_sp, -0.249099225_sp, &
        -0.953830540_sp, 0.652878463_sp, 7.81249031E-02_sp, 0.268358380_sp, -0.249099225_sp]

    do i = 1, size(res_dp)
        print *, res_dp(i)
        if (abs(res_dp(i) - expected_x(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(res_x)
        print *, asin(res_x(i))
        if (abs(res_x(i)) - abs(expected_res_x(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(res_sp)
        print *, res_sp(i)
        if (abs(res_sp(i) - expected_y(i)) > 1e-5) error stop
    end do

    do i = 1, size(res_y)
        print *, asin(res_y(i))
        if (abs(res_y(i)) - abs(expected_res_y(i)) > 1e-5) error stop
    end do


    print *, asin(a)
    if (abs(asin(a)) - 0.54429853389646843_dp > 1e-12) error stop

    print *, asin(b)
    if (asin(b) - (-7.28804991E-02_sp) > 1e-5) error stop

    print *, asin(c)
    if (asin(c) - (-5.44298533896468428e-01_dp) > 1e-12) error stop

    print *, asin(d)
    if (asin(d) - (7.28804991E-02_sp) > 1e-5) error stop

end program