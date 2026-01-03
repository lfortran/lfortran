program intrinsics_228
    use, intrinsic :: iso_fortran_env, only: dp => real64
    real(dp) :: x, y, z

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

end program