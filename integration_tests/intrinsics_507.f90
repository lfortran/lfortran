program intrinsics_507
    ! expm1 is an LFortran extension (not a GFortran intrinsic), so this
    ! test is not labelled gfortran and does not run with --std=f23.
    implicit none
    real(8) :: x, y8
    real(4) :: s, y4
    x = 1.0_8
    s = 1.0
    y8 = expm1(x)
    y4 = expm1(s)
    print *, y8, y4
    if (abs(y8 - 1.7182818284590451_8) > 1e-14_8) error stop 1
    if (abs(y4 - 1.71828183) > 1e-6) error stop 2
    x = 0.5_8
    if (abs(expm1(x) - 0.64872127070012819_8) > 1e-14_8) error stop 3
    x = -1.0_8
    if (abs(expm1(x) - (-0.63212055882855767_8)) > 1e-14_8) error stop 4
end program
