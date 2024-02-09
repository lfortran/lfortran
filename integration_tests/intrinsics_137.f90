program intrinsics_137
    integer(4) :: i
    integer(8) :: j
    real(4) :: r
    real(8) :: d
    i = 0
    j = 1
    r = 0.0
    d = 0.0d0
    print *, huge(i)
    if (huge(i) /= 2147483647) error stop
  
    print*, huge(j)
    if (huge(j) - 9223372036854775807_8 > 1e-5) error stop
  
    print*, huge(r)
    ! if (abs(huge(r) - 3.40282347e+38) > 1e-5) error stop
  
    print*, huge(d)
    ! if (abs(huge(d) - 1.79769313486231571e+308_8) > 1e-5) error stop

end program intrinsics_137
