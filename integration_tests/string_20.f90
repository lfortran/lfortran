program test_scan
    write(*,*) scan("fortran", "ao")          ! 2, found 'o'
    write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
    write(*,*) scan("fortran", "c++")         ! 0, found none
    write(*,*) verify("fortran", "ao")           ! 1, found 'f'
    write(*,*) verify("fortran", "foo")          ! 3, found 'r'
    write(*,*) verify("fortran", "c++")          ! 1, found 'f'
    write(*,*) verify("fortran", "c++", .true.)  ! 7, found 'n'
    write(*,*) verify("fortran", "fortran")      ! 0' found none
end program
