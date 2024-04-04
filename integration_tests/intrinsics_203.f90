program intrinsics_203
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(sp) :: x,y,z
    real(dp) :: i,j,k

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
