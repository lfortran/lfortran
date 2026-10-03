program real128_read_01
    ! list-directed READ of real(16) from internal files and from units
    implicit none
    real(16) :: x, y, z, a(3)
    real(16), parameter :: third = 1.0_16 / 3.0_16
    character(80) :: s
    integer :: u, ios

    s = "0.25"
    read(s, *) x
    if (x /= 0.25_16) error stop 1

    s = "0.333333333333333333333333333333333333"
    read(s, *) x
    if (abs(x - third) > 1.0e-33_16) error stop 2

    s = "1.5d0, -2.5e-2 3"
    read(s, *) x, y, z
    if (x /= 1.5_16) error stop 3
    if (y /= 0.0_16 - 0.025_16) error stop 4
    if (z /= 3.0_16) error stop 5

    s = "1.0 2.0 3.0"
    read(s, *) a
    if (a(1) /= 1.0_16 .or. a(2) /= 2.0_16 .or. a(3) /= 3.0_16) error stop 6

    s = "1.0e-4000"
    read(s, *) x
    if (abs(x / 1.0e-4000_16 - 1.0_16) > 1.0e-30_16) error stop 7

    s = "bad"
    read(s, *, iostat=ios) x
    if (ios == 0) error stop 8

    ! formatted sequential file
    open(newunit=u, file="real128_read_01.txt", status="replace")
    write(u, *) "0.125 ", "4.0d0"
    write(u, *) "7.0"
    close(u)
    open(newunit=u, file="real128_read_01.txt", status="old")
    read(u, *) x, y
    read(u, *) z
    close(u, status="delete")
    if (x /= 0.125_16 .or. y /= 4.0_16 .or. z /= 7.0_16) error stop 9

    ! unformatted sequential file
    open(newunit=u, file="real128_read_01.bin", form="unformatted", status="replace")
    write(u) third
    close(u)
    open(newunit=u, file="real128_read_01.bin", form="unformatted", status="old")
    read(u) x
    close(u, status="delete")
    if (x /= third) error stop 10
    print *, "ok"
end program
