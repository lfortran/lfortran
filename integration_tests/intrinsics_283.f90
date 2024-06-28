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
         
    real(dp), parameter :: res_dp(10) = asinh([-0.62818828_dp, -0.51725372_dp, -0.29257208_dp, &
        0.62818828_dp, 0.51725372_dp, 0.29257208_dp, 1.00_dp, 0.829362821_dp, 0.952716192_dp, -0.241626228_dp])

    real(sp), parameter :: res_sp(10) = asinh([-0.62818828_sp, -0.51725372_sp, -0.29257208_sp, &
        0.62818828_sp, 0.51725372_sp, 0.29257208_sp, 1.00_sp, 0.829362821_sp, 0.952716192_sp, -0.241626228_sp])

    expected_x = [-0.59284540422070708_dp, -0.49659053010574444_dp, -0.28855116339769732_dp, &
        0.59284540422070708_dp, 0.49659053010574444_dp, 0.28855116339769732_dp, 0.88137358701954305_dp, &
        0.75543292593861289_dp, 0.84754059979088248_dp, -0.23933478027454286_dp]

    arg_x = [-0.67922251126_dp, -0.54363892699177885_dp, -0.29691551032790681_dp, &
        0.67922251126401767_dp, 0.54363892699177885_dp, 0.29691551032790681_dp, 0.5707963267948966_dp, &
        0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp, -0.7281628262782_dp, &
        0.5707963267948966_dp, 0.07796627122261770_dp, 0.2620530499450677_dp, -0.24404138779115148_dp]
    
    arg_y = [-0.679222465_sp, -0.543638885_sp, -0.296915501_sp, &
        0.679222465_sp, 0.543638885_sp, 0.296915501_sp, 0.57079637_sp, &
        0.077966249_sp, 0.262053013_sp, -0.244041398_sp, -0.7281628262782_sp, &
        0.57079637_sp, 0.077966249_sp, 0.262053013_sp, -0.244041398_sp]

    expected_y = [-0.592845380_sp, -0.496590495_sp, -0.288551152_sp, &
        0.592845380_sp, 0.496590495_sp, 0.288551152_sp, 0.881373584_sp, &
        0.755432904_sp, 0.847540557_sp, -0.239334792_sp]

    res_x = asinh(arg_x)
    res_y = asinh(arg_y)   

    expected_res_x = [-0.63556192753734742_dp, -0.51989929786720857_dp, -0.29271739145984588_dp, &
        0.63556192754067098_dp, 0.51989929786720857_dp, 0.29271739145984588_dp, 0.54362221755279683_dp, &
        7.7887497072915673E-002_dp, 0.25914283865915849_dp, -0.24168173285755049_dp, -0.67558582147651625_dp, &
        0.54362221755279683_dp, 7.7887497072915673E-002_dp, 0.25914283865915849_dp, -0.24168173285755049_dp]

    expected_res_y = [-0.635561883_sp, -0.519899249_sp, -0.292717397_sp, &
        0.635561883_sp, 0.519899249_sp, 0.292717397_sp, 0.543622255_sp, &
        7.78874755E-02_sp, 0.259142816_sp, -0.241681740_sp, -0.675585806_sp, &
        0.543622255_sp, 7.78874755E-02_sp, 0.259142816_sp, -0.241681740_sp]

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

    print *, asinh(a)
    if (abs(asinh(a)) - 0.49709178005922866_dp > 1e-12) error stop

    print *, asinh(0.5178181202_dp)
    if (abs(asinh(0.5178181202_dp) - 0.49709178005922866_dp) > 1e-12) error stop

    print *, asinh(b)
    if (asinh(b) - (-7.27518052E-02_sp) > 1e-5) error stop

    print *, asinh(-0.072816_sp)
    if (asinh(-0.072816_sp) - (-7.27518052E-02_sp) > 1e-5) error stop

    print *, asinh(c)
    if (asinh(c) - (-0.49709178005922866_dp) > 1e-12) error stop

    print *, asinh(-0.5178181202_dp)
    if (asinh(-0.5178181202_dp) - (-0.49709178005922866_dp) > 1e-12) error stop

    print *, asinh(d)
    if (asinh(d) - (7.27518052E-02_sp) > 1e-5) error stop

    print *, asinh(0.072816_sp)
    if (asinh(0.072816_sp) - (7.27518052E-02_sp) > 1e-5) error stop

    a = -0.271927291_dp
    b = -0.6382728_sp
    c = 0.271927291_dp
    d = 0.6382728_sp

    print *, asinh(a)
    if (abs(asinh(a) - (-0.26868287761696719_dp)) > 1e-12) error stop

    print *, asinh(-0.271927291_dp)
    if (abs(asinh(-0.271927291_dp) - (-0.26868287761696719_dp)) > 1e-12) error stop

    print *, asinh(b)
    if (asinh(b) - (-0.601365387_sp) > 1e-5) error stop

    print *, asinh(-0.6382728_sp)
    if (asinh(-0.6382728_sp) - (-0.601365387_sp) > 1e-5) error stop

    print *, asinh(c)
    if (asinh(c) - (0.26868287761696719_dp) > 1e-12) error stop

    print *, asinh(0.271927291_dp)
    if (asinh(0.271927291_dp) - (0.26868287761696719_dp) > 1e-12) error stop

    print *, asinh(d)
    if (asinh(d) - (0.601365387_sp) > 1e-5) error stop

    print *, asinh(0.6382728_sp)
    if (asinh(0.6382728_sp) - (0.601365387_sp) > 1e-5) error stop

end program
