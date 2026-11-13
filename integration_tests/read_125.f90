program read_125
    ! List-directed internal read of a Q exponent into real(16) (#17485).
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp) :: x
    integer :: ios
    character(len=8) :: buf

    buf = '1.0q1'
    x = 0
    read(buf, *, iostat=ios) x
    if (ios /= 0) error stop "iostat"
    if (x /= 10.0_qp) error stop "q"

    buf = '1.0e1'
    x = 0
    read(buf, *, iostat=ios) x
    if (ios /= 0) error stop "iostat e"
    if (x /= 10.0_qp) error stop "e"

    print *, "ok"
end program
