program intrinsics_209
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    integer :: i
    real(sp) :: x,y,z

    real(dp) :: w(19)
    real(dp) :: expected_w(19)

    real(dp), parameter :: w_comp(19) = [0.321_dp, 0.526_dp, 0.728_dp, -0.321_dp, -0.526_dp, -0.728_dp, &
                                        -1.0_dp, -0.9995004995004995_dp, -0.999000999000999_dp, &
                                        -0.9985014985014985_dp, -0.998001998001998_dp, -0.9975024975024975_dp, &
                                        -0.997002997002997_dp, -0.9965034965034965_dp, -0.996003996003996_dp, &
                                        -0.9955044955044955_dp, -0.995004995004995_dp, -0.9945054945054945_dp, &
                                        -0.994005994005994_dp]
         
    real(dp), parameter :: res(19) = atand(w_comp)

    w = [0.321_dp, 0.526_dp, 0.728_dp, -0.321_dp, -0.526_dp, -0.728_dp, &
         -1.0_dp, -0.9995004995004995_dp, -0.999000999000999_dp, &
         -0.9985014985014985_dp, -0.998001998001998_dp, -0.9975024975024975_dp, &
         -0.997002997002997_dp, -0.9965034965034965_dp, -0.996003996003996_dp, &
         -0.9955044955044955_dp, -0.995004995004995_dp, -0.9945054945054945_dp, &
         -0.994005994005994_dp]

    expected_w = [17.796630203668819_dp, 27.744370169482565_dp, 36.054618166461040_dp, &
                  -17.796630203668819_dp, -27.744370169482565_dp, -36.054618166461040_dp, &
                  -45.000000000000000_dp, -44.985686790326952_dp, -44.971366429413692_dp, &
                  -44.957038913689956_dp, -44.942704239585503_dp, -44.928362403530095_dp, &
                  -44.914013401953525_dp, -44.899657231285609_dp, -44.885293887956209_dp, &
                  -44.870923368395196_dp, -44.856545669032499_dp, -44.842160786298116_dp, &
                  -44.827768716622060_dp]

    do i = 1, size(res)
        print *, res(i)
        if (abs(res(i) - expected_w(i)) > 1e-12_dp) error stop
    end do

    do i = 1, size(w)
        print *, atand(w(i))
        if (abs(atand(w(i)) - expected_w(i)) > 1e-12_dp) error stop
    end do

    x = 0.123
    y = 0.876
    z = 0.542

    print *, atand(x)
    if (atand(x) - (7.01215982) > 1e-5) error stop

    print *, atand(y)
    if (atand(y) - (41.2183571) > 1e-5) error stop

    print *, atand(z)
    if (atand(z) - (28.4576912) > 1e-5) error stop

    print *, atand(0.123_sp)
    if (atand(0.123_sp) - (7.01216030) > 1e-5) error stop

    print *, atand(0.876_sp)
    if (atand(0.876_sp) - (41.2183571) > 1e-5) error stop

    print *, atand(0.542_sp)
    if (atand(0.542_sp) - (28.4576931) > 1e-5) error stop

    x = -0.123
    y = -0.876
    z = -0.542

    print *, atand(x)
    if (atand(x) - (-7.01215982) > 1e-5) error stop

    print *, atand(y)
    if (atand(y) - (-41.2183571) > 1e-5) error stop

    print *, atand(z)
    if (atand(z) - (-28.4576912) > 1e-5) error stop

    print *, atand(-0.123_sp)
    if (atand(-0.123_sp) - (-7.01216030) > 1e-5) error stop

    print *, atand(-0.876_sp)
    if (atand(-0.876_sp) - (-41.2183571) > 1e-5) error stop

    print *, atand(-0.542_sp)
    if (atand(-0.542_sp) - (-28.4576931) > 1e-5) error stop

end program
