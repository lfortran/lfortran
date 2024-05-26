program intrinsics_229
    use, intrinsic :: iso_fortran_env, only: dp => real64
    real(dp) :: x, y, z

    x = 6.738377383_dp
    y = 3.1473863781_dp
    z = 7389.83936383_dp

    print *, derfc(x)
    if (abs(derfc(x) - 1.5803544202317088e-021_dp) > 1e-12) error stop
    print *, derfc(y)
    if (abs(derfc(y) - 8.5440894639449411e-006_dp) > 1e-12) error stop
    print *, derfc(z)
    if (abs(derfc(z) - 0.0000000000000000_dp) > 1e-12) error stop

    x = -6.738377383_dp
    y = -3.1473863781_dp
    z = -7389.83936383_dp

    print *, derfc(x)
    if (abs(derfc(x) - 2.0000000000000000_dp) > 1e-12) error stop
    print *, derfc(y)
    if (abs(derfc(y) - 1.9999914559105361_dp) > 1e-12) error stop
    print *, derfc(z)
    if (abs(derfc(z) - 2.0000000000000000_dp) > 1e-12) error stop

end program