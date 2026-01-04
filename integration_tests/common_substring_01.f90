! Reproducer: COMMON block CHARACTER substring must use COMMON struct member
! (reduced from LAPACK TESTING/LIN/xerbla.f)
program common_substring_01
    implicit none
    character(len=32) :: srnamt
    integer :: nout
    common /srname/ srnamt
    common /iounit/ nout

    nout = 6
    srnamt = "XERBLA"

    call sub_use()

    print *, "PASS: common_substring_01"
end program

subroutine sub_use()
    implicit none
    character(len=32) :: srnamt
    integer :: nout
    common /srname/ srnamt
    common /iounit/ nout

    ! Substring read triggers StringSection on COMMON-backed character
    if (srnamt(1:1) /= "X") error stop "substring mismatch"

    write(nout, *) srnamt(1:6)
end subroutine
