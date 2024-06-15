program intrinsics_252
    use, intrinsic :: iso_fortran_env, only: dp => real64
    real(dp), parameter :: r1 = derf(1.1_dp)
    real(dp), parameter :: r2 = derf(40.12_dp)
    real(dp), parameter :: ar1(3) = derf([0.5_dp, -1.5_dp, 2.2_dp])
    real(dp), parameter :: ar2(2) = derf([-0.2_dp, 0.0_dp])
    real(dp) :: x, y, z
    real(dp) :: arr1(3), arr2(3), res(3)

    x = 6.738377383_dp
    y = 3.1473863781_dp
    z = 7389.83936383_dp

    print *, derf(x)
    if (abs(derf(x) - 1.00000000000000000e+00_dp) > 1e-12) error stop
    print *, derf(y)
    if (abs(derf(y) - 9.99991455910536065e-01_dp) > 1e-12) error stop
    print *, derf(z)
    if (abs(derf(z) - 1.00000000000000000e+00_dp) > 1e-12) error stop

    x = -6.738377383_dp
    y = -3.1473863781_dp
    z = -7389.83936383_dp

    print *, derf(x)
    if (abs(derf(x) - (-1.00000000000000000e+00_dp)) > 1e-12) error stop
    print *, derf(y)
    if (abs(derf(y) - (-9.99991455910536065e-01_dp)) > 1e-12) error stop
    print *, derf(z)
    if (abs(derf(z) - (-1.00000000000000000e+00_dp)) > 1e-12) error stop

    print *, r1
    if (abs(r1 - 8.80205069574081733e-01_dp) > 1e-12_dp) error stop

    print *, r2
    if (abs(r2 - 1.00000000000000000e+00_dp) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [5.20499877813046519e-01_dp, -9.66105146475310761e-01_dp, 9.98137153702018165e-01_dp]) > 1e-12_dp)) error stop

    print *, ar2
    if (any(abs(ar2 - [-2.22702589210478474e-01_dp, 0.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

    arr1 = [-6.738377383_dp, -3.1473863781_dp, -7389.83936383_dp]
    print *, derf(arr1)
    if (any(abs(derf(arr1) - [-1.00000000000000000e+00_dp, -9.99991455910536065e-01_dp, -1.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

    arr2 = [6.738377383_dp, 3.1473863781_dp, 7389.83936383_dp]
    res = derf(arr2)
    print *, res
    if (any(abs(res - [1.00000000000000000e+00_dp, 9.99991455910536065e-01_dp, 1.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

end program