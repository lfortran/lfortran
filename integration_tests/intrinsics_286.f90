program intrinsics_286
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
         
    real(dp), parameter :: res_dp(10) = atanh([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 0.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = atanh([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 0.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])   

    expected_x = [-0.73841779693335385_dp, -0.57258300054508338_dp, -0.30137681412533290_dp, &
        0.73841779693335385_dp, 0.57258300054508338_dp, 0.30137681412533290_dp, 0.00_dp, &
         1.1860917289062738_dp, 1.8604043450053602_dp, -0.24650045327602343_dp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]   

    expected_y = [-0.738417745_sp, -0.572582960_sp, -0.301376820_sp, &
        0.738417745_sp, 0.572582960_sp, 0.301376820_sp, 0.00_sp, 1.18609166_sp, 1.86040413_sp, &
        -0.246500462_sp]

    res_x = atanh(arg_x)
    res_y = atanh(arg_y)            

    expected_res_x = [-0.82766923654327862_dp, -0.60930676235179848_dp, -0.30613348538306767_dp, &
        0.82766923655073721_dp, 0.60930676235179848_dp, 0.30613348538306767_dp, 0.64870320764593825_dp, &
        7.8124828806607219E-002_dp, 0.26831157000093397_dp, -0.24906695687441660_dp, -0.92480545603497655_dp, &
        0.64870320764593825_dp, 7.8124828806607219E-002_dp, 0.26831157000093397_dp, -0.24906695687441660_dp]

    expected_res_y = [-0.827669144_sp, -0.609306693_sp, -0.306133479_sp, &
        0.827669144_sp, 0.609306693_sp, 0.306133479_sp, 0.648703277_sp, &
        7.81248063E-02_sp, 0.268311530_sp, -0.249066964_sp, -0.924805462_sp, &
        0.648703277_sp, 7.81248063E-02_sp, 0.268311530_sp, -0.249066964_sp]

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

    print *, atanh(a)
    if (abs(atanh(a)) - 0.57335387425222428_dp > 1e-12) error stop

    print *, atanh(0.5178181202_dp)
    if (abs(atanh(0.5178181202_dp) - 0.57335387425222428_dp) > 1e-12) error stop

    print *, atanh(b)
    if (atanh(b) - (-7.29451030E-02_sp) > 1e-5) error stop

    print *, atanh(-0.072816_sp)
    if (atanh(-0.072816_sp) - (-7.29451030E-02_sp) > 1e-5) error stop

    print *, atanh(c)
    if (atanh(c) - (-0.57335387425222428_dp) > 1e-12) error stop

    print *, atanh(-0.5178181202_dp)
    if (atanh(-0.5178181202_dp) - (-0.57335387425222428_dp) > 1e-12) error stop

    print *, atanh(d)
    if (atanh(d) - (7.29451030E-02_sp) > 1e-5) error stop

    print *, atanh(0.072816_sp)
    if (atanh(0.072816_sp) - (7.29451030E-02_sp) > 1e-5) error stop

    a = -0.271927291_dp
    b = -0.6382728_sp
    c = 0.271927291_dp
    d = 0.6382728_sp

    print *, atanh(a)
    if (abs(atanh(a) - (-0.27894383144663687_dp)) > 1e-12) error stop

    print *, atanh(-0.271927291_dp)
    if (abs(atanh(-0.271927291_dp) - (-0.27894383144663687_dp)) > 1e-12) error stop

    print *, atanh(b)
    if (atanh(b) - (0.755253792_sp) > 1e-5) error stop

    print *, atanh(-0.6382728_sp)
    if (atanh(-0.6382728_sp) - (0.755253792_sp) > 1e-5) error stop

    print *, atanh(c)
    if (atanh(c) - (0.27894383144663687_dp) > 1e-12) error stop

    print *, atanh(0.271927291_dp)
    if (atanh(0.271927291_dp) - (0.27894383144663687_dp) > 1e-12) error stop

    print *, atanh(d)
    if (atanh(d) - (0.755253792_sp) > 1e-5) error stop

    print *, atanh(0.6382728_sp)
    if (atanh(0.6382728_sp) - (0.755253792_sp) > 1e-5) error stop

end program