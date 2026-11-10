program complex128_tiny_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp) :: x, y
    character(len=60) :: s

    ! Subnormal results and their text round trip
    x = tiny(1._qp) * 0.7_qp
    if (x <= 0 .or. x >= tiny(1._qp)) error stop "subnormal"
    write(s, "(es45.35e4)") x
    read(s, *) y
    if (x /= y) error stop "subnormal round trip"
    x = huge(1._qp) / 3
    write(s, "(es45.35e4)") x
    read(s, *) y
    if (x /= y) error stop "huge round trip"
    x = 0.3_qp
    write(s, "(es45.35e4)") x
    read(s, *) y
    if (x /= y) error stop "0.3 round trip"
    print *, "ok"
end program
