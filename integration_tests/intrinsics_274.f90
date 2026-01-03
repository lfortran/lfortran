program intrinsics_274
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none

    real :: x = 3.143
    real(dp) :: y = 2.33

    real(sp), parameter :: r1 = epsilon(1._sp)
    real(dp), parameter :: r2 = epsilon(67._dp)
    real(sp), parameter :: ar1 = epsilon([11.3_sp, 1.7_sp, 0.0_sp])
    real(dp), parameter :: ar2 = epsilon([11.3_dp, 1.7_dp, 0.0_dp])

    real(sp) :: arr1(3) = [11.3_sp, 1.7_sp, 0.0_sp]
    real(dp) :: arr2(3) = [19.3_dp, 3.7_dp, 0.0_dp]

    print *, r1
    if (abs(r1 - 1.19209290E-07) > 1e-6) error stop
    print *, r2
    if (abs(r2 - 2.2204460492503131E-016_dp) > 1e-12_dp) error stop

    print *, ar1
    if (abs(ar1 - 1.19209290E-07) > 1e-6) error stop
    print *, ar2
    if (abs(ar2 - 2.2204460492503131E-016_dp) > 1e-12_dp) error stop

    print *, epsilon(x)
    if (abs(epsilon(x) - 1.19209290E-07) > 1e-6) error stop
    print *, epsilon(y)
    if (abs(epsilon(y) - 2.2204460492503131E-016_dp) > 1e-12_dp) error stop
    print *, epsilon(1._dp) ** 0.5 !Part of Minpack
    if (abs((epsilon(1._dp) ** 0.5_dp) - 1.4901161193847656E-008_dp) > 1e-12_dp) error stop

    print *, epsilon(arr1)
    if (abs(epsilon(arr1) - 1.19209290E-07) > 1e-6) error stop
    print *, epsilon(arr2)
    if (abs(epsilon(arr2) - 2.2204460492503131E-016_dp) > 1e-12_dp) error stop

end program