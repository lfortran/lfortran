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

    i = -1.0_dp
    j = -0.9995004995004995_dp
    k = -0.999000999000999_dp

    print *, acosd(i)
    if (acosd(i) - 180.0_dp > 1e-12) error stop

    print *, acosd(j)
    if (acosd(j) - 178.18897822198568_dp > 1e-12) error stop

    print *, acosd(k)
    if (acosd(k) - 177.43872179938884_dp > 1e-12) error stop

    i = -0.9985014985014985_dp
    j = -0.998001998001998_dp
    k = -0.9975024975024975_dp

    print *, acosd(i)
    if (acosd(i) - 176.86295702294953_dp > 1e-12) error stop

    print *, acosd(j)
    if (acosd(j) - 176.377503905344_dp > 1e-12) error stop

    print *, acosd(k)
    if (acosd(k) - 175.94975751498464_dp > 1e-12) error stop

    i = -0.997002997002997_dp
    j = -0.9965034965034965_dp
    k = -0.996003996003996_dp

    print *, acosd(i)
    if (acosd(i) - 175.5629967823349_dp > 1e-12) error stop

    print *, acosd(j)
    if (acosd(j) - 175.20728904760477_dp > 1e-12) error stop

    print *, acosd(k)
    if (acosd(k) - 174.87616290603896_dp > 1e-12) error stop

    i = -0.9955044955044955_dp
    j = -0.995004995004995_dp
    k = -0.9945054945054945_dp

    print *, acosd(i)
    if (acosd(i) - 174.5651234927047_dp > 1e-12) error stop

    print *, acosd(j)
    if (acosd(j) - 174.27089826020691_dp > 1e-12) error stop

    print *, acosd(k)
    if (acosd(k) - 173.99101680223387_dp > 1e-12) error stop

    i = -0.994005994005994_dp

    print *, acosd(i)
    if (acosd(i) - 173.72355993833804_dp > 1e-12) error stop

    print *, acosd(0.123)
    if (acosd(0.123) - 97.0652695 > 1e-5) error stop

end program




