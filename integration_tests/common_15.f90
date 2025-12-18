! ICE: segfault when COMMON block has different variable names in different units
! This is valid Fortran - COMMON blocks map storage locations, not variable names
program common_15
    implicit none
    integer :: infot, noutc
    common /infoc/ infot, noutc
    noutc = 6
    call sub1()
    print *, "PASS"
end program

subroutine sub1()
    implicit none
    ! Same COMMON block but different variable name (NOUT vs NOUTC)
    ! Both refer to the same storage location
    integer :: infot, nout
    common /infoc/ infot, nout
    write(nout, *) "TEST"
end subroutine
