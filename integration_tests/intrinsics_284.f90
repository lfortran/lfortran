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

    real(dp) :: a = 1.5178181202_dp
    real(sp) :: b = 1.072816_sp
    real(dp) :: c = 1.5178181202_dp
    real(sp) :: d = 1.072816_sp
         
    real(dp), parameter :: res_dp(10) = acosh([1.62818828_dp, 1.51725372_dp, 1.29257208_dp, &
        1.62818828_dp, 1.51725372_dp, 1.29257208_dp, 1.00_dp, 1.829362821_dp, 1.952716192_dp, 1.241626228_dp])

    real(sp), parameter :: res_sp(10) = acosh([1.62818828_sp, 1.51725372_sp, 1.29257208_sp, &
        1.62818828_sp, 1.51725372_sp, 1.29257208_sp, 1.00_sp, 1.829362821_sp, 1.952716192_sp, 1.241626228_sp])

    expected_x = [1.0692176655903725_dp, 0.97769872817826375_dp, 0.74742740658933504_dp, &
        1.0692176655903725_dp, 0.97769872817826375_dp, 0.74742740658933504_dp, 0.00_dp, &
        1.2123022470110765_dp, 1.2892178294282344_dp, 0.68187653246421109_dp]

    arg_x = [1.67922251126_dp, 1.54363892699177885_dp, 1.29691551032790681_dp, &
        1.67922251126401767_dp, 1.54363892699177885_dp, 1.29691551032790681_dp, 1.5707963267948966_dp, &
        1.07796627122261770_dp, 1.2620530499450677_dp, 1.24404138779115148_dp, 1.7281628262782_dp, &
        1.5707963267948966_dp, 1.07796627122261770_dp, 1.2620530499450677_dp, 1.24404138779115148_dp]
    
    arg_y = [1.679222465_sp, 1.543638885_sp, 1.296915501_sp, &
        1.679222465_sp, 1.543638885_sp, 1.296915501_sp, 1.57079637_sp, &
        1.077966249_sp, 1.262053013_sp, 1.244041398_sp, 1.7281628262782_sp, &
        1.57079637_sp, 1.077966249_sp, 1.262053013_sp, 1.244041398_sp]

    expected_y = [1.06921768_sp, 0.977698743_sp, 0.747427344_sp, &
        1.06921768_sp, 0.977698743_sp, 0.747427344_sp, 0.00_sp, &
        1.21230233_sp, 1.28921783_sp, 0.681876600_sp]

    res_x = acosh(arg_x)
    res_y = acosh(arg_y)      

    expected_res_x = [1.1079742559357899_dp, 1.0004749128435737_dp, 0.75270877751131104_dp, &
        1.1079742559387682_dp, 1.0004749128435737_dp, 0.75270877751131104_dp, 1.0232274785475506_dp, &
        0.39236130842345907_dp, 0.70900761659991784_dp, 0.68514914208529898_dp, 1.1434619762512719_dp, &
        1.0232274785475506_dp, 0.39236130842345907_dp, 0.70900761659991784_dp, 0.68514914208529898_dp]

    expected_res_y = [1.10797417_sp, 1.00047481_sp, 0.752708793_sp, &
        1.10797417_sp, 1.00047481_sp, 0.752708793_sp, 1.02322757_sp, &
        0.392361164_sp, 0.709007561_sp, 0.685149193_sp, 1.14346206_sp, &
        1.02322757_sp, 0.392361164_sp, 0.709007561_sp, 0.685149193_sp]

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

    print *, acosh(a)
    if (abs(acosh(a)) - 0.97819318581773873_dp > 1e-12) error stop

    print *, acosh(1.5178181202_dp)
    if (abs(acosh(1.5178181202_dp) - 0.97819318581773873_dp) > 1e-12) error stop

    print *, acosh(b)
    if (acosh(b) - (0.379339129_sp) > 1e-5) error stop

    print *, acosh(1.072816_sp)
    if (acosh(1.072816_sp) - (0.379339129_sp) > 1e-5) error stop

    print *, acosh(c)
    if (acosh(c) - (0.97819318581773873_dp) > 1e-12) error stop

    print *, acosh(1.5178181202_dp)
    if (acosh(1.5178181202_dp) - (0.97819318581773873_dp) > 1e-12) error stop

    print *, acosh(d)
    if (acosh(d) - (0.379339129_sp) > 1e-5) error stop

    print *, acosh(1.072816_sp)
    if (acosh(1.072816_sp) - (0.379339129_sp) > 1e-5) error stop

    a = 1.271927291_dp
    b = 1.6382728_sp
    c = 1.271927291_dp
    d = 1.6382728_sp

    print *, acosh(a)
    if (abs(acosh(a) - (0.72170024457681714_dp)) > 1e-12) error stop

    print *, acosh(1.271927291_dp)
    if (abs(acosh(1.271927291_dp) - (0.72170024457681714_dp)) > 1e-12) error stop

    print *, acosh(b)
    if (acosh(b) - (1.07702732_sp) > 1e-5) error stop

    print *, acosh(1.6382728_sp)
    if (acosh(1.6382728_sp) - (1.07702732_sp) > 1e-5) error stop

    print *, acosh(c)
    if (acosh(c) - (0.72170024457681714_dp) > 1e-12) error stop

    print *, acosh(1.271927291_dp)
    if (acosh(1.271927291_dp) - (0.72170024457681714_dp) > 1e-12) error stop

    print *, acosh(d)
    if (acosh(d) - (1.07702732_sp) > 1e-5) error stop

    print *, acosh(1.6382728_sp)
    if (acosh(1.6382728_sp) - (1.07702732_sp) > 1e-5) error stop

end program