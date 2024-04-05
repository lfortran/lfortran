program intrinsics_185

    character(7) :: fortran = "FORTRAN"
    character(2) :: af = "AF"
    character(3) :: foo = "FOO"
    character(3) :: c_plus_plus = "C++"
    character(6) :: fortr = "FORTR"
    character(1) :: n = "N"
    integer :: arr2(2)
    character(len=5) :: string = "hello"
	character(len=1) :: set(2) = ["l", "h"]
  
    print*, verify("FORTRAN", "AF", .true., 4)    
    if ( verify("FORTRAN", "AF", .true., 4) /= 7 ) error stop   
    print*, verify("FORTRAN", "FOO", kind = 8)
    if ( verify("FORTRAN", "FOO", kind = 8) /= 3_8 ) error stop
    print*, verify("FORTRAN", "C++", .true.)
    if ( verify("FORTRAN", "C++", .true.) /= 7 ) error stop
    print*, verify("FORTR", "N")
    if ( verify("FORTR", "N") /= 1 ) error stop
    print*, verify("FORTRAN", "FORTRAN", .true.)
    if ( verify("FORTRAN", "FORTRAN", .true.) /= 0 ) error stop
  
    print*, verify(fortran, af, kind = 4)
    if ( verify(fortran, af, kind = 4) /= 2_4 ) error stop
    print*, verify(fortran, foo, .true.)
    if ( verify(fortran, foo, .true.) /= 7 ) error stop  
    print*, verify(fortran, c_plus_plus)
    if ( verify(fortran, c_plus_plus) /= 1 ) error stop
    print*, verify(fortr, n, .true., 8)
    if ( verify(fortr, n, .true., 8) /= 6_8 ) error stop
    print*, verify(fortran, fortran)  
    if ( verify(fortran, fortran) /= 0 ) error stop 

    ! make sure that broadcasting is done correctly for `verify`
    arr2 = verify(["FORTRAN", "GORTRAN"], ["FO", "GR"])
    if (arr2(1) /= 3) error stop
    if (arr2(2) /= 2) error stop

    arr2 = verify(["FORTRAN", "GORTRAN"], ["FN", "NA"], .TRUE.)
    if (arr2(1) /= 6) error stop
    if (arr2(2) /= 5) error stop

    arr2 = verify(string, set)
    print*, arr2
    if (arr2(1) /= 1) error stop
    if (arr2(2) /= 2) error stop

    arr2 = verify(string, set, .TRUE., 4)
    print*, arr2
    if (arr2(1) /= 5) error stop
    if (arr2(2) /= 5) error stop
end program
