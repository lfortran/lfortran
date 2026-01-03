program intrinsics_251
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64
    real(sp), parameter :: r1 = erf(1.1_sp)
    real(dp), parameter :: r2 = erf(40.12_dp)
    real(sp), parameter :: ar1(3) = erf([0.5_sp, -1.5_sp, 2.2_sp])
    real(dp), parameter :: ar2(2) = erf([-0.2_dp, 0.0_dp])
    real(sp) :: x
    real(dp) :: y, z
    real(sp) :: arr1(3)
    real(dp) :: arr2(3), res(3)

    x = 6.738377383_sp
    y = 3.1473863781_dp
    z = 7389.83936383_dp

    print *, erf(x)
    if (abs(erf(x) - 1.00000000e+00_sp) > 1e-6) error stop
    print *, erf(y)
    if (abs(erf(y) - 9.99991455910536065e-01_dp) > 1e-12) error stop
    print *, erf(z)
    if (abs(erf(z) - 1.00000000000000000e+00_dp) > 1e-12) error stop

    x = -6.738377383_sp
    y = -3.1473863781_dp
    z = -7389.83936383_dp

    print *, erf(x)
    if (abs(erf(x) - (-1.00000000e+00_sp)) > 1e-6) error stop
    print *, erf(y)
    if (abs(erf(y) - (-9.99991455910536065e-01_dp)) > 1e-12) error stop
    print *, erf(z)
    if (abs(erf(z) - (-1.00000000000000000e+00_dp)) > 1e-12) error stop

    print *, r1
    if (abs(r1 - 8.80205095e-01_sp) > 1e-6_sp) error stop

    print *, r2
    if (abs(r2 - 1.00000000000000000e+00_dp) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [5.20499885e-01_sp, -9.66105163e-01_sp, 9.98137176e-01_sp]) > 1e-6_sp)) error stop

    print *, ar2
    if (any(abs(ar2 - [-2.22702589210478474e-01_dp, 0.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

    arr1 = [-6.738377383_sp, -3.1473863781_sp, -7389.83936383_sp]
    print *, erf(arr1)
    if (any(abs(erf(arr1) - [-1.00000000e+00_sp, -9.99991477e-01_sp, -1.00000000e+00_sp]) > 1e-6_sp)) error stop

    arr2 = [6.738377383_dp, 3.1473863781_dp, 7389.83936383_dp]
    res = erf(arr2)
    print *, res
    if (any(abs(res - [1.00000000000000000e+00_dp, 9.99991455910536065e-01_dp, 1.00000000000000000e+00_dp]) > 1e-12_dp)) error stop

end program