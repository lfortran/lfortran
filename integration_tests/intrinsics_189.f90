program intrinsics_189
    character(7) :: fortran = "FORTRAN"
    character(2) :: ao = "AO"
    character(3) :: c_plus_plus = "C++"
    integer, parameter :: i1 = scan("FORTRAN", "AO")
    integer, parameter :: i2 = scan("FORTRAN", "AO", kind = 4)
    integer, parameter :: i3 = scan("FORTRAN", "AO", .TRUE., 8)
    integer, parameter :: i4 = scan("FORTRAN", "C++")
    integer, parameter :: ar1(2) = scan(["LFORTRAN", "GFORTRAN"], ["AO", "AO"])
    integer, parameter :: ar2(2) = scan(["LFORTRAN", "GFORTRAN"], ["LO", "TR"], kind = 4)

    character(8) :: arr1(2)
    character(2) :: arr2(2)
    character(4) :: arr3(3)
    character(2) :: arr4(3)

    arr1 = ["LFORTRAN", "GFORTRAN"]
    arr2 = ["AO", "AO"]
    arr3 = ["LFOR", "TRAN", "GFOR"]
    arr4 = ["LO", "TR", "AN"]
    
    print*, i1
    if (i1 /= 2) error stop
    print*, i2
    if (i2 /= 2) error stop
    print*, i3
    if (i3 /= 6_8) error stop
    print*, i4
    if (i4 /= 0) error stop

    print*, ar1
    if (any(ar1 /= [3, 3])) error stop
    print*, ar2
    if (any(ar2 /= [1, 4])) error stop

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

    print*, scan(arr1, arr2)
    if (any(scan(arr1, arr2) /= [3, 3])) error stop
    print*, scan(arr1, arr2, kind = 4)
    if (any(scan(arr1, arr2, kind = 4) /= [3, 3])) error stop
    print*, scan(arr3, arr4)
    if (any(scan(arr3, arr4) /= [1, 1, 0])) error stop

end program
  
  
