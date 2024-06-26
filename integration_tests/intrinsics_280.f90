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
         
    real(dp), parameter :: res_dp(10) = cosh([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = cosh([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])

    expected_x = [1.2038847685286660_dp, 1.1367850899207470_dp, 1.0431053788343854_dp, &
        1.2038847685286660_dp, 1.1367850899207470_dp, 1.0431053788343854_dp, 1.5430806348152437_dp, &
        1.3640925837349942_dp, 1.4892172380307673_dp, 1.0293339187973498_dp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    expected_y = [1.20388472_sp, 1.13678503_sp, 1.04310536_sp, &
        1.20388472_sp, 1.13678503_sp, 1.04310536_sp, 1.54308069_sp, &
        1.36409259_sp, 1.48921716_sp, 1.02933395_sp]

    res_x = cosh(arg_x)
    res_y = cosh(arg_y)   

    expected_res_x = [1.2396773477464105_dp, 1.1514470944182906_dp, 1.0444041956586250_dp, &
        1.2396773477493541_dp, 1.1514470944182906_dp, 1.0444041956586250_dp, 1.1673755027897188_dp, &
        1.0030409096642332_dp, 1.0345328431712835_dp, 1.0299261823832639_dp, 1.2770334891369264_dp, &
        1.1673755027897188_dp, 1.0030409096642332_dp, 1.0345328431712835_dp, 1.0299261823832639_dp]

    expected_res_y = [1.23967731_sp, 1.15144706_sp, 1.04440415_sp, &
        1.23967731_sp, 1.15144706_sp, 1.04440415_sp, 1.16737556_sp, &
        1.00304091_sp, 1.03453279_sp, 1.02992618_sp, 1.27703345_sp, &
        1.16737556_sp, 1.00304091_sp, 1.03453279_sp, 1.02992618_sp]

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

    print *, cosh(a)
    if (abs(cosh(a)) - 1.1370904024390038_dp > 1e-12) error stop

    print *, cosh(0.5178181202_dp)
    if (abs(cosh(0.5178181202_dp) - 1.1370904024390038_dp) > 1e-12) error stop

    print *, cosh(b)
    if (cosh(b) - (1.00265229_sp) > 1e-5) error stop

    print *, cosh(-0.072816_sp)
    if (cosh(-0.072816_sp) - (1.00265229_sp) > 1e-5) error stop

    print *, cosh(c)
    if (cosh(c) - (1.1370904024390038_dp) > 1e-12) error stop

    print *, cosh(-0.5178181202_dp)
    if (cosh(-0.5178181202_dp) - (1.1370904024390038_dp) > 1e-12) error stop

    print *, cosh(d)
    if (cosh(d) - (1.00265229_sp) > 1e-5) error stop

    print *, cosh(0.072816_sp)
    if (cosh(0.072816_sp) - (1.00265229_sp) > 1e-5) error stop

    a = -0.271927291_dp
    b = -0.6382728_sp
    c = 0.271927291_dp
    d = 0.6382728_sp

    print *, cosh(a)
    if (abs(cosh(a) - (1.0372006123287354_dp)) > 1e-12) error stop

    print *, cosh(-0.271927291_dp)
    if (abs(cosh(-0.271927291_dp) - (1.0372006123287354_dp)) > 1e-12) error stop

    print *, cosh(b)
    if (cosh(b) - (1.21070600_sp) > 1e-5) error stop

    print *, cosh(-0.6382728_sp)
    if (cosh(-0.6382728_sp) - (1.21070600_sp) > 1e-5) error stop

    print *, cosh(c)
    if (cosh(c) - (1.0372006123287354_dp) > 1e-12) error stop

    print *, cosh(0.271927291_dp)
    if (cosh(0.271927291_dp) - (1.0372006123287354_dp) > 1e-12) error stop

    print *, cosh(d)
    if (cosh(d) - (1.21070600_sp) > 1e-5) error stop

    print *, cosh(0.6382728_sp)
    if (cosh(0.6382728_sp) - (1.21070600_sp) > 1e-5) error stop

end program