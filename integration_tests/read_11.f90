! MRE: Formatted READ with mixed types (string + logical)
! ICE in visit_FileRead: get_string_type() called on non-character type
! Reduced from LAPACK BLAS/TESTING/dblat2.f
program read_11
    implicit none
    character(6) :: sname
    logical :: ltest

    open(10, file='/dev/null')
    read(10, fmt=100, end=200) sname, ltest
100 format(a6, l2)
200 continue
    print *, "PASS"
end program
