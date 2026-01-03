program intrinsics_203
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(sp) :: x,y,z
    real(dp) :: i,j,k
    integer :: iterator

    real(dp) :: w(19)
    real(sp) :: u(19)
    real(sp) :: res_u(19)
    real(dp) :: res_w(19)
    real(dp) :: expected_w(19)
    real(sp) :: expected_u(19)
         
    real(dp), parameter :: res1(19) = asind([0.321_dp, 0.526_dp, 0.728_dp, -0.321_dp, -0.526_dp, -0.728_dp, &
        -1.0_dp, -0.9995004995004995_dp, -0.999000999000999_dp, &
        -0.9985014985014985_dp, -0.998001998001998_dp, -0.9975024975024975_dp, &
        -0.997002997002997_dp, -0.9965034965034965_dp, -0.996003996003996_dp, &
        -0.9955044955044955_dp, -0.995004995004995_dp, -0.9945054945054945_dp, &
        -0.994005994005994_dp])
    
    real(sp), parameter :: res_2(19) = asind([0.321_sp, 0.526_sp, 0.728_sp, -0.321_sp, -0.526_sp, -0.728_sp, &
        -1.0_sp, -0.9995004995004995_sp, -0.999000999000999_sp, &
        -0.9985014985014985_sp, -0.998001998001998_sp, -0.9975024975024975_sp, &
        -0.997002997002997_sp, -0.9965034965034965_sp, -0.996003996003996_sp, &
        -0.9955044955044955_sp, -0.995004995004995_sp, -0.9945054945054945_sp, &
        -0.994005994005994_sp])

    w = [0.321_dp, 0.526_dp, 0.728_dp, -0.321_dp, -0.526_dp, -0.728_dp, &
         -1.0_dp, -0.9995004995004995_dp, -0.999000999000999_dp, &
         -0.9985014985014985_dp, -0.998001998001998_dp, -0.9975024975024975_dp, &
         -0.997002997002997_dp, -0.9965034965034965_dp, -0.996003996003996_dp, &
         -0.9955044955044955_dp, -0.995004995004995_dp, -0.9945054945054945_dp, &
         -0.994005994005994_dp]

    u = [0.321_sp, 0.526_sp, 0.728_sp, -0.321_sp, -0.526_sp, -0.728_sp, &
        -1.0_sp, -0.9995004995004995_sp, -0.999000999000999_sp, &
        -0.9985014985014985_sp, -0.998001998001998_sp, -0.9975024975024975_sp, &
        -0.997002997002997_sp, -0.9965034965034965_sp, -0.996003996003996_sp, &
        -0.9955044955044955_sp, -0.995004995004995_sp, -0.9945054945054945_sp, &
        -0.994005994005994_sp]

    expected_w = [18.7234114226922479_dp, 31.7355871858161613_dp, 46.7189879975828859_dp, &
        -18.7234114226922479_dp, -31.7355871858161613_dp, -46.7189879975828859_dp, &
        -90.0000000000000000_dp, -88.1889782219856926_dp, -87.4387217993888584_dp, &
        -86.8629570229495442_dp, -86.3775039053440139_dp, -85.9497575149846398_dp, &
        -85.5629967823348778_dp, -85.2072890476047604_dp, -84.8761629060389708_dp, &
        -84.5651234927047142_dp, -84.2708982602068915_dp, -83.9910168022338581_dp, &
        -83.7235599383380418_dp]

    expected_u = [18.7234116_sp, 31.7355881_sp, 46.7189827_sp, &
        -18.7234116_sp, -31.7355881_sp, -46.7189827_sp, &
        -90.0000000_sp, -88.1890030_sp, -87.4387589_sp, &
        -86.8629379_sp, -86.3774948_sp, -85.9497604_sp, &
        -85.5630112_sp, -85.2072754_sp, -84.8761520_sp, &
        -84.5651245_sp, -84.2709045_sp, -83.9909973_sp, &
        -83.7235489_sp]
    
    res_w = asind(w)
    res_u = asind(u)

    do iterator = 1, size(res1)
        print *, res1(iterator)
        if (abs(res1(iterator) - expected_w(iterator)) > 1e-12_dp) error stop
    end do

    do iterator = 1, size(res_2)
        print *, res_2(iterator)
        if (abs(res_2(iterator) - expected_u(iterator)) > 1e-5_sp) error stop
    end do

    do iterator = 1, size(res_w)
        print *, asind(res_w(iterator))
        if (abs(res_w(iterator) - expected_w(iterator)) > 1e-12_dp) error stop
    end do

    do iterator = 1, size(res_u)
        print *, asind(res_u(iterator))
        if (abs(res_u(iterator) - expected_u(iterator)) > 1e-5_sp) error stop
    end do

    x = 0.123
    y = 0.876
    z = 0.542

    i = 0.321
    j = 0.526
    k = 0.728

    print *, asind(x)
    if (asind(x) - 7.06527281 > 1e-5) error stop

    print *, asind(y)
    if (asind(y) - 61.1635399 > 1e-5) error stop

    print *, asind(z)
    if (asind(z) - 32.8198891 > 1e-5) error stop

    print *, asind(i)
    if (asind(i) - 18.723412014063609_dp > 1e-12) error stop

    print *, asind(j)
    if (asind(j) - 31.735588727756351_dp > 1e-12) error stop

    print *, asind(k)
    if (asind(k) - 46.7189879975828859_dp > 1e-12) error stop

    print *, asind(0.123_sp)
    if (asind(0.123_sp) - 7.06527328 > 1e-5) error stop

    print *, asind(0.876_sp)
    if (asind(0.876_sp) - 61.1635437 > 1e-5) error stop

    print *, asind(0.542_sp)
    if (asind(0.542_sp) - 32.8198929 > 1e-5) error stop

    print *, asind(0.321_dp)
    if (asind(0.321_dp) - 18.723411422692248_dp > 1e-12) error stop

    print *, asind(0.526_dp)
    if (asind(0.526_dp) - 31.735587185816161_dp > 1e-12) error stop

    print *, asind(0.728_dp)
    if (asind(0.728_dp) - 46.718987997582886_dp > 1e-12) error stop

    x = -0.123
    y = -0.876
    z = -0.542

    i = -0.321
    j = -0.526
    k = -0.728

    print *, asind(x)
    if (asind(x) - (-7.06527281) > 1e-5) error stop

    print *, asind(y)
    if (asind(y) - (-61.1635399) > 1e-5) error stop

    print *, asind(z)
    if (asind(z) - (-32.8198891) > 1e-5) error stop

    print *, asind(i)
    if (asind(i) - (-18.7234114226922479_dp) > 1e-12) error stop

    print *, asind(j)
    if (asind(j) - (-31.7355871858161613_dp) > 1e-12) error stop

    print *, asind(k)
    if (asind(k) - (-46.718986762209930_dp) > 1e-12) error stop


end program
