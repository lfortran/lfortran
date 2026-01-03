program intrinsics_205
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    integer :: i
    real(sp) :: x,y,z
    real(dp) :: k,l,m

    real(dp) :: w(19)
    real(dp) :: res_w(19)
    real(dp) :: expected_w(19)
    real(sp) :: expected_u(19)
         
    real(dp), parameter :: res(19) = acosd([0.321_dp, 0.526_dp, 0.728_dp, -0.321_dp, -0.526_dp, -0.728_dp, &
        -1.0_dp, -0.9995004995004995_dp, -0.999000999000999_dp, &
        -0.9985014985014985_dp, -0.998001998001998_dp, -0.9975024975024975_dp, &
        -0.997002997002997_dp, -0.9965034965034965_dp, -0.996003996003996_dp, &
        -0.9955044955044955_dp, -0.995004995004995_dp, -0.9945054945054945_dp, &
        -0.994005994005994_dp])

    real(sp), parameter :: res_1(19) = acosd([0.321_sp, 0.526_sp, 0.728_sp, -0.321_sp, -0.526_sp, -0.728_sp, &
        -1.0_sp, -0.9995004_sp, -0.999000_sp, &
        -0.998501_sp, -0.998001_sp, -0.997_sp, &
        -0.997002_sp, -0.99650_sp, -0.99600_sp, &
        -0.9955_sp, -0.99500_sp, -0.99450_sp, &
        -0.99400_sp])

    w = [0.321_dp, 0.526_dp, 0.728_dp, -0.321_dp, -0.526_dp, -0.728_dp, &
         -1.0_dp, -0.9995004995004995_dp, -0.999000999000999_dp, &
         -0.9985014985014985_dp, -0.998001998001998_dp, -0.9975024975024975_dp, &
         -0.997002997002997_dp, -0.9965034965034965_dp, -0.996003996003996_dp, &
         -0.9955044955044955_dp, -0.995004995004995_dp, -0.9945054945054945_dp, &
         -0.994005994005994_dp]

    expected_w = [71.276588577307763_dp, 58.264412814183842_dp, 43.281012002417114_dp, &
                  108.72341142269225_dp, 121.73558718581616_dp, 136.718987997582900_dp, &
                  180.0_dp, 178.18897822198568_dp, 177.43872179938884_dp, &
                  176.86295702294953_dp, 176.377503905344_dp, 175.94975751498464_dp, &
                  175.5629967823349_dp, 175.20728904760477_dp, 174.87616290603896_dp, &
                  174.5651234927047_dp, 174.27089826020691_dp, 173.99101680223387_dp, &
                  173.72355993833804_dp]

    expected_u = [71.2765884_sp, 58.2644081_sp, 43.2810135_sp, &
                  108.723404_sp, 121.735596_sp, 1.36718994e+02_sp, &
                  180.0_sp, 178.188782_sp, 177.437454_sp, &
                  1.76862442e+02_sp, 176.376556_sp, 175.560760_sp, &
                  175.562256_sp, 175.204895_sp, 174.873581_sp, &
                  174.562424_sp, 174.268036_sp, 173.987991_sp, &
                  173.720413_sp]

    res_w = acosd(w)

    do i = 1, size(res)
        print *, res(i)
        if (abs(res(i) - expected_w(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(w)
        print *, res_w(i)
        if (abs(res_w(i) - expected_w(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(res_1)
        print *, res_1(i)
        if (abs(res_1(i) - expected_u(i)) > 1e-4_sp) error stop
    end do

    x = 0.123
    y = 0.876
    z = 0.542

    k = 0.1237382_dp
    l = 0.8767382_dp
    m = 0.5427382_dp

    print *, acosd(x)
    if (acosd(x) - 8.29347229e+01 > 1e-5) error stop

    print *, acosd(y)
    if (acosd(y) - 2.88364525e+01 > 1e-5) error stop

    print *, acosd(z)
    if (acosd(z) - 5.71801071e+01 > 1e-5) error stop

    print *, acosd(0.123_sp)
    if (acosd(0.123_sp) - 8.29347229e+01 > 1e-5) error stop

    print *, acosd(0.876_sp)
    if (acosd(0.876_sp) - 2.88364525e+01 > 1e-5) error stop

    print *, acosd(0.542_sp)
    if (acosd(0.542_sp) - 5.71801071e+01 > 1e-5) error stop

    print *, acosd(k)
    if (acosd(k) - 8.29347229e+01 > 1e-12) error stop

    print *, acosd(l)
    if (acosd(l) - 2.88364525e+01 > 1e-12) error stop

    print *, acosd(m)
    if (acosd(m) - 5.71801071e+01 > 1e-12) error stop

    print *, acosd(0.1237382_dp)
    if (acosd(0.1237382_dp) - 8.29347229e+01 > 1e-12) error stop

    print *, acosd(0.8767382_dp)
    if (acosd(0.8767382_dp) - 2.88364525e+01 > 1e-12) error stop

    print *, acosd(0.5427382_dp)
    if (acosd(0.5427382_dp) - 5.71801071e+01 > 1e-12) error stop

    x = -0.123
    y = -0.876
    z = -0.542

    k = -0.1237382_dp
    l = -0.8767382_dp
    m = -0.5427382_dp

    print *, acosd(x)
    if (acosd(x) - 9.70652695e+01 > 1e-5) error stop

    print *, acosd(y)
    if (acosd(y) - 1.51163544e+02 > 1e-5) error stop

    print *, acosd(z)
    if (acosd(z) - 1.22819893e+02 > 1e-5) error stop

    print *, acosd(-0.123_sp)
    if (acosd(-0.123_sp) - 9.70652695e+01 > 1e-5) error stop

    print *, acosd(-0.876_sp)
    if (acosd(-0.876_sp) - 1.51163544e+02 > 1e-5) error stop

    print *, acosd(-0.542_sp)
    if (acosd(-0.542_sp) - 1.22819893e+02 > 1e-5) error stop

    print *, acosd(k)
    if (acosd(k) - 9.71078942667026581e+01_dp > 1e-12) error stop

    print *, acosd(l)
    if (acosd(l) - 1.51251363283139312e+02_dp > 1e-12) error stop

    print *, acosd(m)
    if (acosd(m) - 1.22870235241297351e+02_dp > 1e-12) error stop

end program
