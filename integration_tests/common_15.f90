! Test: COMMON block with different variable names across units
! This is the simple case - same layout, different names
program common_15
    implicit none
    integer :: infot, noutc
    common /infoc/ infot, noutc
    noutc = 6
    infot = 42
    call sub1()
    if (infot /= 100) error stop "infot should be 100"
    print *, "PASS: common_15"
end program

subroutine sub1()
    implicit none
    ! Same COMMON block but different variable name (NOUT vs NOUTC)
    ! Both refer to the same storage location
    integer :: infot, nout
    common /infoc/ infot, nout
    if (nout /= 6) error stop "nout should be 6"
    if (infot /= 42) error stop "infot should be 42"
    infot = 100
end subroutine
