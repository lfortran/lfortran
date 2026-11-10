program complex128_io_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    complex(qp) :: z, w, a(2), b(2)
    character(len=200) :: s
    integer :: u, ios

    z = (1.5_qp, -2.5_qp)
    write(s, *) z
    if (index(s, "1.5") == 0 .or. index(s, "-2.5") == 0) error stop "list directed write"
    read(s, *) w
    if (w /= z) error stop "internal list directed read"

    z = cmplx(0.1_qp, huge(1._qp) / 3, kind=qp)
    write(s, "(2es45.35e4)") z
    read(s, "(2es45.35e4)") w
    if (w /= z) error stop "formatted round trip"

    a = [cmplx(1._qp, 2._qp, kind=qp), cmplx(-3._qp / 7, 4._qp / 9, kind=qp)]
    open(newunit=u, file="complex128_io_01.dat", status="replace")
    write(u, "(*(es45.35e4, 1x))") a
    close(u)
    open(newunit=u, file="complex128_io_01.dat", status="old")
    read(u, "(*(es45.35e4, 1x))", iostat=ios) b
    close(u, status="delete")
    if (ios /= 0) error stop "iostat"
    if (any(a /= b)) error stop "file round trip"

    open(newunit=u, file="complex128_io_01.dat", status="replace")
    write(u, *) a
    close(u)
    open(newunit=u, file="complex128_io_01.dat", status="old")
    read(u, *) b
    close(u, status="delete")
    if (any(abs(a - b) > 1.0e-30_qp)) error stop "list directed file round trip"

    z = (1.25_qp, 0.5_qp)
    write(s, "(es12.4, es12.4)") z
    if (index(s, "1.2500E+00") == 0) error stop "es12.4"
    print *, "ok"
end program
