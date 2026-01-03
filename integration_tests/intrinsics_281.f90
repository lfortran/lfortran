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
         
    real(dp), parameter :: res_dp(10) = tanh([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = tanh([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])   

    expected_x = [-0.55680344433989626_dp, -0.47557764686289139_dp, -0.28450043308229522_dp, &
        0.55680344433989626_dp, 0.47557764686289139_dp, 0.28450043308229522_dp, 0.76159415595576485_dp, &
        0.68013372295360075_dp, 0.74101025957155131_dp, -0.23703120039784659_dp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    expected_y = [-0.556803405_sp, -0.475577623_sp, -0.284500420_sp, &
        0.556803405_sp, 0.475577623_sp, 0.284500420_sp, 0.761594176_sp, &
        0.680133700_sp, 0.741010249_sp, -0.237031206_sp]

    res_x = tanh(arg_x)
    res_y = tanh(arg_y)    

    expected_res_x = [-0.59101371380532286_dp, -0.49573755452355417_dp, -0.28848735226454980_dp, &
        0.59101371380793721_dp, 0.49573755452355417_dp, 0.28848735226454980_dp, 0.51594386420508198_dp, &
        7.7808675521968823E-002_dp, 0.25621481487810227_dp, -0.23930935875580195_dp, -0.62194009906582415_dp, &
        0.51594386420508198_dp, 7.7808675521968823E-002_dp, 0.25621481487810227_dp, -0.23930935875580195_dp]

    expected_res_y = [-0.591013670_sp, -0.495737523_sp, -0.288487345_sp, &
        0.591013670_sp, 0.495737523_sp, 0.288487345_sp, 0.515943885_sp, &
        7.78086558E-02_sp, 0.256214768_sp, -0.239309371_sp, -0.621940076_sp, &
        0.515943885_sp, 7.78086558E-02_sp, 0.256214768_sp, -0.239309371_sp]

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

    print *, tanh(a)
    if (abs(tanh(a)) - 0.47601427711167560_dp > 1e-12) error stop

    print *, tanh(0.5178181202_dp)
    if (abs(tanh(0.5178181202_dp) - 0.47601427711167560_dp) > 1e-12) error stop

    print *, tanh(b)
    if (tanh(b) - (-7.26875812E-02_sp) > 1e-5) error stop

    print *, tanh(-0.072816_sp)
    if (tanh(-0.072816_sp) - (-7.26875812E-02_sp) > 1e-5) error stop

    print *, tanh(c)
    if (tanh(c) - (-0.47601427711167560_dp) > 1e-12) error stop

    print *, tanh(-0.5178181202_dp)
    if (tanh(-0.5178181202_dp) - (-0.47601427711167560_dp) > 1e-12) error stop

    print *, tanh(d)
    if (tanh(d) - (7.26875812E-02_sp) > 1e-5) error stop

    print *, tanh(0.072816_sp)
    if (tanh(0.072816_sp) - (7.26875812E-02_sp) > 1e-5) error stop

    a = -0.271927291_dp
    b = -0.6382728_sp
    c = 0.271927291_dp
    d = 0.6382728_sp

    print *, tanh(a)
    if (abs(tanh(a) - (-0.265417270575538994_dp)) > 1e-12) error stop

    print *, tanh(-0.271927291_dp)
    if (abs(tanh(-0.271927291_dp) - (-0.265417270575538994_dp)) > 1e-12) error stop

    print *, tanh(b)
    if (tanh(b) - (-0.563722372_sp) > 1e-5) error stop

    print *, tanh(-0.6382728_sp)
    if (tanh(-0.6382728_sp) - (-0.563722372_sp) > 1e-5) error stop

    print *, tanh(c)
    if (tanh(c) - (0.26541727057553899_dp) > 1e-12) error stop

    print *, tanh(0.271927291_dp)
    if (tanh(0.271927291_dp) - (0.26541727057553899_dp) > 1e-12) error stop

    print *, tanh(d)
    if (tanh(d) - (0.563722372_sp) > 1e-5) error stop

    print *, tanh(0.6382728_sp)
    if (tanh(0.6382728_sp) - (0.563722372_sp) > 1e-5) error stop

end program