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
         
    real(dp), parameter :: res_dp(10) = sinh([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = sinh([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])

    expected_x = [-0.67032718570509986_dp, -0.54062957805332923_dp, -0.29676393202885426_dp, &
        0.67032718570509986_dp, 0.54062957805332923_dp, 0.29676393202885426_dp, 1.1752011936438014_dp, &
        0.92776536742907800_dp, 1.1035252521116077_dp, -0.24398425438275537_dp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    expected_y = [-0.670327127_sp, -0.540629566_sp, -0.296763927_sp, &
        0.670327127_sp, 0.540629566_sp, 0.296763927_sp, 1.17520118_sp, &
        0.927765369_sp, 1.10352528_sp, -0.243984252_sp]

    res_x = sinh(arg_x)
    res_y = sinh(arg_y)

    expected_res_x = [-0.73266631321193876_dp, -0.57081556675017531_dp, -0.30129740109954350_dp, &
        0.73266631321691944_dp, 0.57081556675017531_dp, 0.30129740109954350_dp, 0.60230022788767801_dp, &
        7.8045284675324761E-002_dp, 0.26506264089844722_dp, -0.24647097427195003_dp, -0.79423833474419503_dp, &
        0.60230022788767801_dp, 7.8045284675324761E-002_dp, 0.26506264089844722_dp, -0.24647097427195003_dp]

    expected_res_y = [-0.732666254_sp, -0.570815504_sp, -0.301297396_sp, &
        0.732666254_sp, 0.570815504_sp, 0.301297396_sp, 0.602300286_sp, &
        7.80452639E-02_sp, 0.265062600_sp, -0.246470988_sp, -0.794238329_sp, &
        0.602300286_sp, 7.80452639E-02_sp, 0.265062600_sp, -0.246470988_sp]

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

    print *, sinh(a)
    if (abs(sinh(a)) - 0.54127126592762664_dp > 1e-12) error stop

    print *, sinh(0.5178181202_dp)
    if (abs(sinh(0.5178181202_dp) - 0.54127126592762664_dp) > 1e-12) error stop

    print *, sinh(b)
    if (sinh(b) - (-7.28803650E-02_sp) > 1e-5) error stop

    print *, sinh(-0.072816_sp)
    if (sinh(-0.072816_sp) - (-7.28803650E-02_sp) > 1e-5) error stop

    print *, sinh(c)
    if (sinh(c) - (-0.54127126592762664_dp) > 1e-12) error stop

    print *, sinh(-0.5178181202_dp)
    if (sinh(-0.5178181202_dp) - (-0.54127126592762664_dp) > 1e-12) error stop

    print *, sinh(d)
    if (sinh(d) - (7.28803650E-02_sp) > 1e-5) error stop

    print *, sinh(0.072816_sp)
    if (sinh(0.072816_sp) - (7.28803650E-02_sp) > 1e-5) error stop

    a = -0.271927291_dp
    b = -0.6382728_sp
    c = 0.271927291_dp
    d = 0.6382728_sp

    print *, sinh(a)
    if (abs(sinh(a) - (-0.27529095556357069_dp)) > 1e-12) error stop

    print *, sinh(-0.271927291_dp)
    if (abs(sinh(-0.271927291_dp) - (-0.27529095556357069_dp)) > 1e-12) error stop

    print *, sinh(b)
    if (sinh(b) - (-0.682502091_sp) > 1e-5) error stop

    print *, sinh(-0.6382728_sp)
    if (sinh(-0.6382728_sp) - (-0.682502091_sp) > 1e-5) error stop

    print *, sinh(c)
    if (sinh(c) - (0.27529095556357069_dp) > 1e-12) error stop

    print *, sinh(0.271927291_dp)
    if (sinh(0.271927291_dp) - (0.27529095556357069_dp) > 1e-12) error stop

    print *, sinh(d)
    if (sinh(d) - (.682502091_sp) > 1e-5) error stop

    print *, sinh(0.6382728_sp)
    if (sinh(0.6382728_sp) - (.682502091_sp) > 1e-5) error stop

end program