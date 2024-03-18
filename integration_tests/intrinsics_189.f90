program intrinsics_189

    character(7) :: fortran = "FORTRAN"
    character(2) :: ao = "AO"
    character(3) :: c_plus_plus = "C++"

    print*, scan("FORTRAN", "AO", kind = 4) 
    if (scan("fortran", "ao", kind = 4) /= 2) error stop         
    print*, scan("FORTRAN", "AO", .TRUE., 8)  
    if (scan("fortran", "ao", .true., 8) /= 6_8) error stop
    print*, scan("FORTRAN", "C++")  
    if (scan("fortran", "c_plus_plus") /= 0) error stop

    print*, scan(fortran, ao, kind = 4)       
    if (scan(fortran, ao, kind = 4) /= 2) error stop
    print*, scan(fortran, ao, .TRUE., 8)  
    if (scan(fortran, ao, .true., 8) /= 6_8) error stop
    print*, scan(fortran, c_plus_plus) 
    if (scan(fortran, c_plus_plus) /= 0) error stop

end program
  
  