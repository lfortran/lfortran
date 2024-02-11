program intrinsics_137
    use iso_fortran_env, only: int32, int64
    integer, parameter :: sp = kind(0.0)
    integer, parameter :: dp = kind(0.d0)
    integer(int32) :: i
    integer(int64) :: j
    real(sp) :: r
    real(dp) :: d
    i = 0
    j = 1
    r = 0.0_sp
    d = 0.0_dp
    
    print *, huge(i)
    if (huge(i) /= 2147483647_int32) error stop
  
    print*, huge(j)
    if (huge(j) /= 9223372036854775807_int64) error stop
  
    print*, huge(r)
    ! if (abs(huge(r) - 3.40282347e+38_sp) > 1e-5_sp) error stop
  
    print*, huge(d)
    if (abs(huge(d) - 1.79769313486231571e+308_dp) > 1e-5_dp) error stop

end program intrinsics_137
