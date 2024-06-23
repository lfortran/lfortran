program intrinsics_270
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

    real(dp), parameter :: res_dp(10) = atan([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = atan([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])

    expected_x = [-0.56088872649127330_dp, -0.47735511827211041_dp, -0.28462833491299150_dp, &
            0.56088872649127330_dp, 0.47735511827211041_dp, 0.28462833491299150_dp, 0.78539816339744828_dp, &
            0.69239044268167327_dp, 0.76118851632111739_dp, -0.23708207111515284_dp]
            
    expected_y = [-0.560888708_sp, -0.477355093_sp, -0.284628332_sp, &
        0.560888708_sp, 0.477355093_sp, 0.284628332_sp, 0.785398185_sp, &
        0.692390442_sp, 0.761188507_sp, -0.237082079_sp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    res_x = atan(arg_x)
    res_y = atan(arg_y)

    expected_res_x = [-0.59664481328347907_dp, -0.49794635545452487_dp, -0.28862459062845508_dp, &
            0.59664481328622843_dp, 0.49794635545452487_dp, 0.28862459062845508_dp, 0.51866936925501661_dp, &
            7.7808866037245739E-002_dp, 0.25629014680688234_dp, -0.23936274280729375_dp, &
            -0.62937821282601869_dp, 0.51866936925501661_dp, 7.7808866037245739E-002_dp, &
            0.25629014680688234_dp, -0.23936274280729375_dp]

    expected_res_y = [-0.596644759_sp, -0.497946322_sp, -0.288624585_sp, &
                    0.596644759_sp, 0.497946322_sp, 0.288624585_sp, 0.518669426_sp, &
                    7.78088495E-02_sp, 0.256290108_sp, -0.239362746_sp, &
                    -0.629378200_sp, 0.518669426_sp, 7.78088495E-02_sp, &
                    0.256290108_sp, -0.239362746_sp]

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

    print *, atan(a)
    if (abs(atan(a)) - 0.47780028380709660_dp > 1e-12) error stop

    print *, atan(0.5178181202_dp)
    if (abs(atan(0.5178181202_dp) - 0.47780028380709660_dp) > 1e-12) error stop

    print *, atan(b)
    if (atan(b) - (-7.26877153E-02_sp) > 1e-5) error stop

    print *, atan(-0.072816_sp)
    if (atan(-0.072816_sp) - (-7.26877153E-02_sp) > 1e-5) error stop

    print *, atan(c)
    if (atan(c) - (-0.47780028380709660_dp) > 1e-12) error stop

    print *, atan(-0.5178181202_dp)
    if (atan(-0.5178181202_dp) - (-0.47780028380709660_dp) > 1e-12) error stop

    print *, atan(d)
    if (atan(d) - (7.26877153E-02_sp) > 1e-5) error stop

    print *, atan(0.072816_sp)
    if (atan(0.072816_sp) - (7.26877153E-02_sp) > 1e-5) error stop

    a = -0.271927291_dp
    b = -0.6382728_sp
    c = 0.271927291_dp
    d = 0.6382728_sp

    print *, atan(a)
    if (abs(atan(a) - (-0.26550729967368875_dp)) > 1e-12) error stop

    print *, atan(-0.271927291_dp)
    if (abs(atan(-0.271927291_dp) - (-0.26550729967368875_dp)) > 1e-12) error stop

    print *, atan(b)
    if (atan(b) - ( -0.568086922_sp) > 1e-5) error stop

    print *, atan(-0.6382728_sp)
    if (atan(-0.6382728_sp) - (-0.568086922_sp) > 1e-5) error stop

    print *, atan(c)
    if (atan(c) - (0.26550729967368875_dp) > 1e-12) error stop

    print *, atan(0.271927291_dp)
    if (atan(0.271927291_dp) - (0.26550729967368875_dp) > 1e-12) error stop

    print *, atan(d)
    if (atan(d) - (0.568086922_sp) > 1e-5) error stop

    print *, atan(0.6382728_sp)
    if (atan(0.6382728_sp) - (0.568086922_sp) > 1e-5) error stop


end program