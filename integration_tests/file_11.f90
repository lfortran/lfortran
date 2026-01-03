program file_11
    implicit none

    integer :: u
    open(unit = 0, file="file_01_data.txt", status="old")
    open(unit = 1, file="file_01_data.txt", status="old")
    open(unit = 2, file="file_01_data.txt", status="old")

    open(newunit=u, file="file_01_data.txt", status="old")
    print *, u
    ! -10 is returned by gfortran, 3 is returned by lfortran
    ! similarly for other asserts below
    if (u /= -10 .and. u /= 3) error stop
    close(unit = 1)

    open(newunit=u, file="file_01_data.txt", status="old")
    print *, u
    if (u /= -11 .and. u /= 1) error stop

    open(newunit=u, file="file_01_data.txt", status="old")
    print *, u
    if (u /= -12 .and. u /= 4) error stop

    close(unit = 0)
    open(newunit=u, file="file_01_data.txt", status="old")
    print *, u
    if (u /= -13 .and. u /= 0) error stop

end program
