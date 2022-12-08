program test_scan
    print *, scan("fortran", "ao")          ! 2, found 'o'
    print *, scan("fortran", "ao", .true.)  ! 6, found 'a'
    print *, scan("fortran", "c++")         ! 0, found none
    print *, verify("fortran", "ao")           ! 1, found 'f'
    print *, verify("fortran", "foo")          ! 3, found 'r'
    print *, verify("fortran", "c++")          ! 1, found 'f'
    print *, verify("fortran", "c++", .true.)  ! 7, found 'n'
    print *, verify("fortran", "fortran")      ! 0' found none
 end program
