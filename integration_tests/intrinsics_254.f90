program intrinsics_251
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64
    real(sp), parameter :: r1 = erfc(1.1_sp)
    real(dp), parameter :: r2 = erfc(4.12_dp)
    real(sp), parameter :: ar1(3) = erfc([0.5_sp, -1.5_sp, 2.2_sp])
    real(dp), parameter :: ar2(2) = erfc([-0.2_dp, 0.0_dp])
    real(sp) :: x
    real(dp) :: y, z
    real(sp) :: arr1(3)
    real(dp) :: arr2(3), res(3)

    x = 6.738377383_sp
    y = 3.1473863781_dp
    z = 7389.83936383_dp

    print *, erfc(x)
    if (abs(erfc(x) - 1.58035037e-21_sp) > 1e-6_sp) error stop
    print *, erfc(y)
    if (abs(erfc(y) - 8.54408946394494113e-06_dp) > 1e-12) error stop
    print *, erfc(z)
    if (abs(erfc(z) - 0.00000000000000000e+00_dp) > 1e-12) error stop

    x = -6.738377383_sp
    y = -3.1473863781_dp
    z = -7389.83936383_dp

    print *, erfc(x)
    if (abs(erfc(x) - 2.00000000e+00_sp) > 1e-6_sp) error stop
    print *, erfc(y)
    if (abs(erfc(y) - 1.99999145591053606e+00_dp) > 1e-12_dp) error stop
    print *, erfc(z)
    if (abs(erfc(z) - 2.00000000000000000e+00_dp) > 1e-12_dp) error stop

    print *, r1
    if (abs(r1 - 1.19794928e-01_sp) > 1e-6_sp) error stop

    print *, r2
    if (abs(r2 - 5.65815721936131674e-09_dp) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [4.79500115e-01_sp, 1.96610510e+00_sp, 1.86284585e-03_sp]) > 1e-6_sp)) error stop

    print *, ar2
    if (any(abs(ar2 - [1.22270258921047859e+00_dp, 1.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

    arr1 = [-6.738377383_sp, -3.1473863781_sp, -7389.83936383_sp]
    print *, erfc(arr1)
    if (any(abs(erfc(arr1) - [2.00000000e+00_sp, 1.99999142e+00_sp, 2.00000000e+00_sp]) > 1e-6_sp)) error stop

    arr2 = [6.738377383_dp, 3.1473863781_dp, 7389.83936383_dp]
    res = erfc(arr2)
    print *, res
    if (any(abs(res - [1.58035442023170862e-21_dp, 8.54408946394494113e-06_dp, 0.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

end program