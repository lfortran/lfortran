program intrinsics_137
    
    print *, huge(0_4)
    if (huge(0_4) /= 2147483647) error stop
  
    print*, huge(1_8)
    ! if (huge(1_8) - 9223372036854775807 > 1e-5) error stop
  
    print*, huge(0.0)
    ! if (abs(huge(0.0) - 3.40282347e+38) > 1e-5) error stop
  
    print*, huge(0.0d0)
    ! if (abs(huge(0.0d0) - 1.79769313486231571e+308) > 1e-5) error stop

end program intrinsics_137
