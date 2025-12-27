! MRE: Formatted READ with mixed types (string + logical)
! ICE in visit_FileRead: get_string_type() called on non-character type
! Reduced from LAPACK BLAS/TESTING/dblat2.f
program read_07
    implicit none
    character(6) :: sname
    logical :: ltest
    integer :: u, stat

    open(newunit=u, file="read_07_data.txt", status="old", iostat=stat)
    if (stat /= 0) error stop

    read(u, fmt=100, iostat=stat) sname, ltest
    if (stat /= 0) error stop
100 format(a6, l2)
    close(u)

    if (sname /= "ABCDEF") error stop
    if (.not. ltest) error stop

    print *, "PASS"
end program read_07
