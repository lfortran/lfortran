program intrinsics_223
  
    use iso_fortran_env, only:  dp=>real64, sp=>real32  
    implicit none
  
    real(4) :: x1 = 1.0_4, x2 = 42.0_4
    real(8) :: y1 = 1.0_8, y2 = 42.0_8
    real :: s1 = 1.0_4, s2 = -1.0_4
  
    real(4), parameter :: x3 = 1.0_sp, x4 = 42.0_sp
    real(8), parameter :: y3 = 1.0_dp, y4 = 42.0_dp
    real, parameter :: s3 = 1.0, s4 = -1.0
  
    print*, nearest(x1, s1)
    if (abs(nearest(x1, s1) - 1.00000012_sp) > 1e-6) error stop
    print*, nearest(y1, s1)
    if (abs(nearest(y1, s1) - 1.0000000000000002_dp) > 1e-12) error stop
    print*, nearest(x2, s1)
    if (abs(nearest(x2, s1) - 42.0000038_sp) > 1e-6) error stop
    print*, nearest(y2, s1)
    if (abs(nearest(y2, s1) - 42.000000000000007_dp) > 1e-12) error stop
  
    print*, nearest(x1, s2)
    if (abs(nearest(x1, s2) - 0.999999940_sp) > 1e-6) error stop
    print*, nearest(y1, s2)
    if (abs(nearest(y1, s2) - 0.99999999999999989_dp) > 1e-12) error stop
    print*, nearest(x2, s2)
    if (abs(nearest(x2, s2) - 41.9999962_sp) > 1e-6) error stop
    print*, nearest(y2, s2)
    if (abs(nearest(y2, s2) - 41.999999999999993_dp) > 1e-12) error stop
  
    print*, nearest(x3, s3)
    if (abs(nearest(x3, s3) - 1.00000012e+00_sp) > 1e-6) error stop
    print*, nearest(y3, s3)
    if (abs(nearest(y3, s3) - 1.0000000000000001e+00_dp) > 1e-12) error stop
    print*, nearest(x4, s3)
    if (abs(nearest(x4, s3) - 42.0000038_sp) > 1e-6) error stop
    print*, nearest(y4, s3)
    if (abs(nearest(y4, s3) - 42.000000000000007_dp) > 1e-12) error stop
  
    print*, nearest(x3, s4)
    if (abs(nearest(x3, s4) - 0.999999940_sp) > 1e-6) error stop
    print*, nearest(y3, s4)
    if (abs(nearest(y3, s4) - 0.99999999999999989_dp) > 1e-12) error stop
    print*, nearest(x4, s4)
    if (abs(nearest(x4, s4) - 41.9999962_sp) > 1e-6) error stop
    print*, nearest(y4, s4)
    if (abs(nearest(y4, s4) - 41.999999999999993_dp) > 1e-12) error stop
  
end program
  
