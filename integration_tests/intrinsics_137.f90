program intrinsics_137
    use iso_fortran_env, only: int32, int64
    implicit none
    integer, parameter :: sp = kind(0.0)
    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: i1 = huge(0)
    integer(8), parameter :: i2 = huge(12_dp)
    real, parameter :: i3 = huge(1.1_sp)
    real(8), parameter :: i4 = huge(1.5_dp)
    integer, parameter :: ar1(3) = huge([1, 2, 45])
    real, parameter :: ar2(3) = huge([1.0, 2.0, 45.0])

    integer(int32) :: i
    integer(int64) :: j
    real(sp) :: r
    real(dp) :: d
    integer :: arr1(3) = [11, 13, 7]
    real :: arr2(3) = [11.0, 13.0, 7.0]

    i = 0
    j = 1
    r = 0.0_sp
    d = 0.0_dp

    print *, i1
    if (i1 /= 2147483647) error stop
    print *, i2
    if (i2 /= 9223372036854775807_int64) error stop
    print *, i3
    if (abs((i3 - 3.40282347e+38_sp) / 3.40282347e+38_sp) > 1e-6_sp) error stop
    print *, i4
    if (abs(i4 - 1.79769313486231571e+308_dp) > 1e-12_dp) error stop
    
    print *, ar1
    if (any(ar1 /= [2147483647, 2147483647, 2147483647])) error stop
    print *, ar2
    if (any(abs((ar2 - [3.40282347e+38_sp, 3.40282347e+38_sp, 3.40282347e+38_sp]) / 3.40282347e+38_sp) > 1e-6_sp)) error stop

    print *, huge(i)
    if (huge(i) /= 2147483647_int32) error stop
  
    print*, huge(j)
    if (huge(j) /= 9223372036854775807_int64) error stop
  
    print*, huge(r)
    if ((huge(r) - 3.40282347e+38) / 3.40282347e+38 > 1e-8_sp) error stop
  
    print*, huge(d)
    if (abs(huge(d) - 1.79769313486231571e+308_dp) > 1e-8_dp) error stop

    print *, kind(huge(1))
    if (kind(huge(1)) /= int32) error stop
    print *, kind(huge(1.0))
    if (kind(huge(1.0)) /= sp) error stop
    print *, kind(huge(1.0_dp))
    if (kind(huge(1.0_dp)) /= dp) error stop

    print *, huge(arr1)
    if (any(huge(arr1) /= [2147483647, 2147483647, 2147483647])) error stop
    print *, huge(arr2)
    if (any(abs((huge(arr2) - [3.40282347e+38, 3.40282347e+38, 3.40282347e+38]) / 3.40282347e+38) > 1e-6)) error stop
    
end program intrinsics_137
