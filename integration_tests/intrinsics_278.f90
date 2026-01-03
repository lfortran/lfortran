program intrinsics_278
    use iso_fortran_env, only: dp => real64, sp => real32
    implicit none
    real, parameter :: x = tiny(1.0)
    real (kind = dp) :: b
    real (kind = dp), parameter :: y = tiny(b)
    real, parameter :: z = tiny([1.0_sp, 2.0_sp])

    real(sp) :: r1 = 2.3_sp
    real(dp) :: r2 = 4.5_dp
    real(sp) :: ar1(3) = [1.0_sp, 2.0_sp, 3.0_sp]
    real(dp) :: ar2(3) = [4.0_dp, 5.0_dp, 6.0_dp]

    print*, x
    if (abs(x - 1.17549435e-38_sp) > 1e-6_sp) error stop
    print*, y
    if (abs(y - 2.2250738585072014e-308_dp) > 1e-6_dp) error stop
    print*, z
    if (abs(z - 1.17549435e-38_sp) > 1e-6_sp) error stop

    print*, tiny(r1)
    if (abs(tiny(r1) - 1.17549435e-38_sp) > 1e-6_sp) error stop
    print*, tiny(r2)
    if (abs(tiny(r2) - 2.2250738585072014e-308_dp) > 1e-6_dp) error stop
    print*, tiny(ar1)
    if (abs(tiny(ar1) - 1.17549435e-38_sp) > 1e-6_sp) error stop
    print*, tiny(ar2)
    if (abs(tiny(ar2) - 2.2250738585072014e-308_dp) > 1e-6_dp) error stop

    print *, kind(tiny(1.0))
    print *, kind(tiny(1.0_dp))

end program
