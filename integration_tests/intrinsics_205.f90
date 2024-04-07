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

    print *, acosd(x)
    if (acosd(x) - 82.9347229 > 1e-5) error stop

    print *, acosd(y)
    if (acosd(y) - 28.8364525 > 1e-5) error stop

    print *, acosd(z)
    if (acosd(z) - 57.1801071 > 1e-5) error stop

    print *, acosd(i)
    if (acosd(i) - 71.2765885773077628_dp > 1e-12) error stop

    print *, acosd(j)
    if (acosd(j) - 58.2644128141838422_dp > 1e-12) error stop

    print *, acosd(k)
    if (acosd(k) - 43.281013237790070_dp > 1e-12) error stop

    print *, acosd(0.123_sp)
    if (acosd(0.123_sp) - 82.9347229 > 1e-5) error stop

    print *, acosd(0.876_sp)
    if (acosd(0.876_sp) - 28.8364525 > 1e-5) error stop

    print *, acosd(0.542_sp)
    if (acosd(0.542_sp) - 57.1801071 > 1e-5) error stop

    print *, acosd(0.321_dp)
    if (acosd(0.321_dp) - 71.276588577307763_dp > 1e-12) error stop

    print *, acosd(0.526_dp)
    if (acosd(0.526_dp) - 58.264412814183842_dp > 1e-12) error stop

    print *, acosd(0.728_dp)
    if (acosd(0.728_dp) - 43.281012002417114_dp > 1e-12) error stop

    x = -0.123
    y = -0.876
    z = -0.542

    i = -0.321
    j = -0.526
    k = -0.728

    print *, acosd(x)
    if (acosd(x) - 97.0652695 > 1e-5) error stop

    print *, acosd(y)
    if (acosd(y) - 151.163544 > 1e-5) error stop

    print *, acosd(z)
    if (acosd(z) - 122.819878 > 1e-5) error stop

    print *, acosd(i)
    if (acosd(i) - 108.72341201406361_dp > 1e-12) error stop

    print *, acosd(j)
    if (acosd(j) - 121.73558872775635_dp > 1e-12) error stop

    print *, acosd(k)
    if (acosd(k) - 136.718987997582900_dp > 1e-12) error stop

end program
