program read_real16_internal_01
    ! list-directed and formatted READ into real(16) from an internal file
    ! and list-directed READ from an external file.
    implicit none
    integer, parameter :: qp = 16
    real(qp) :: x, y(2)
    character(len=40) :: s
    integer :: u

    s = "0.25"
    read(s, *) x
    if (x /= 0.25_qp) error stop "internal list-directed"

    s = "1.5 -2.5"
    read(s, *) y
    if (y(1) /= 1.5_qp) error stop "internal list-directed array 1"
    if (y(2) /= -2.5_qp) error stop "internal list-directed array 2"

    s = "0.25"
    read(s, '(F20.10)') x
    if (x /= 0.25_qp) error stop "internal formatted"

    open(newunit=u, file="read_real16_internal_01_data.txt", status="old")
    read(u, *) x
    if (x /= 0.375_qp) error stop "external list-directed"
    close(u)
end program
