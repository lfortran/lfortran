program intrinsics_253
    use, intrinsic :: iso_fortran_env, only: dp => real64
    real(dp), parameter :: r1 = derfc(1.1_dp)
    real(dp), parameter :: r2 = derfc(4.12_dp)
    real(dp), parameter :: ar1(3) = derfc([0.5_dp, -1.5_dp, 2.2_dp])
    real(dp), parameter :: ar2(2) = derfc([-0.2_dp, 0.0_dp])
    real(dp) :: x, y, z
    real(dp) :: arr1(3), arr2(3), res(3)

    x = 6.738377383_dp
    y = 3.1473863781_dp
    z = 7389.83936383_dp

    print *, derfc(x)
    if (abs(derfc(x) - 1.58035442023170862e-21_dp) > 1e-12) error stop
    print *, derfc(y)
    if (abs(derfc(y) - 8.54408946394494113e-06_dp) > 1e-12) error stop
    print *, derfc(z)
    if (abs(derfc(z) - 0.00000000000000000e+00_dp) > 1e-12) error stop

    x = -6.738377383_dp
    y = -3.1473863781_dp
    z = -7389.83936383_dp

    print *, derfc(x)
    if (abs(derfc(x) - 2.00000000000000000e+00_dp) > 1e-12) error stop
    print *, derfc(y)
    if (abs(derfc(y) - 1.99999145591053606e+00_dp) > 1e-12) error stop
    print *, derfc(z)
    if (abs(derfc(z) - 2.00000000000000000e+00_dp) > 1e-12) error stop

    print *, r1
    if (abs(r1 - 1.19794930425918267e-01_dp) > 1e-12_dp) error stop

    print *, r2
    if (abs(r2 - 5.65815721936131674e-09_dp) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [4.79500122186953481e-01_dp, 1.96610514647531076e+00_dp, &
    1.86284629798188941e-03_dp]) > 1e-12_dp)) error stop

    print *, ar2
    if (any(abs(ar2 - [1.22270258921047859e+00_dp, 1.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

    arr1 = [-6.738377383_dp, -3.1473863781_dp, -7389.83936383_dp]
    print *, derfc(arr1)
    if (any(abs(derfc(arr1) - [2.00000000000000000e+00_dp, 1.99999145591053606e+00_dp, &
    2.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

    arr2 = [6.738377383_dp, 3.1473863781_dp, 7389.83936383_dp]
    res = derfc(arr2)
    print *, res
    if (any(abs(res - [1.58035442023170862e-21_dp, 8.54408946394494113e-06_dp, & 
    0.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

end program