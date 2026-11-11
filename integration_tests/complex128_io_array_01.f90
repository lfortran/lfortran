program complex128_io_array_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    complex(qp) :: a(4), b(4), m(2, 2), n(2, 2)
    complex(qp), allocatable :: c(:), d(:)
    character(len=400) :: s
    character(len=40) :: line(2)
    integer :: i, ios

    a = [(cmplx(i / 3.0_qp, -i / 7.0_qp, kind=qp), i = 1, 4)]

    write(s, *) a
    read(s, *) b
    if (any(abs(a - b) > 1.0e-30_qp)) error stop "list directed round trip"

    b = 0
    write(s, "(4(2es45.35e4))") a
    read(s, "(4(2es45.35e4))") b
    if (any(a /= b)) error stop "formatted round trip"

    b = 0
    write(s, "(*(2es45.35e4))") a(4:1:-1)
    read(s, "(*(2es45.35e4))") b
    if (any(a(4:1:-1) /= b)) error stop "reversed section"

    m = reshape(a, [2, 2])
    write(s, "(*(2es45.35e4))") m
    read(s, "(*(2es45.35e4))") n
    if (any(m /= n)) error stop "2d round trip"
    n = 0
    write(s, *) m
    read(s, *) n
    if (any(abs(m - n) > 1.0e-30_qp)) error stop "2d list directed"

    write(s, "(*(2es45.35e4))") (a(i), i = 1, 2)
    read(s, "(*(2es45.35e4))") b(1:2)
    if (any(a(1:2) /= b(1:2))) error stop "implied do output"

    write(line, "(2es20.10e3)") a(1:2)
    if (index(line(1), "3.3333333333E-001") == 0) error stop "real part text"
    read(line, "(2es20.10e3)") b(3:4)
    if (abs(b(3) - a(1)) > 1.0e-9_qp) error stop "multi record"
    if (abs(b(4) - a(2)) > 1.0e-9_qp) error stop "multi record 2"

    allocate(c(3), d(3))
    c = [(1, 2), (3, 4), (5, 6)]
    write(s, *) c
    read(s, *) d
    if (any(c /= d)) error stop "allocatable list directed"
    d = 0
    write(s, "(3(f10.2,f10.2))") c
    read(s, "(3(f10.2,f10.2))", iostat=ios) d
    if (ios /= 0) error stop "iostat"
    if (any(c /= d)) error stop "allocatable fixed format"

    write(s, *) (1.5_qp, 2.5_qp), (3.5_qp, 4.5_qp)
    read(s, *) b(1), b(2)
    if (b(1) /= (1.5_qp, 2.5_qp) .or. b(2) /= (3.5_qp, 4.5_qp)) error stop "scalar sequence"
    print *, "ok"
end program
