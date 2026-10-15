program intrinsics_490
    ! get_command: status is 0 on success and -1 when command is too short
    implicit none
    character(len=:), allocatable :: cmd
    character(len=4) :: short
    character(len=4096) :: long
    integer :: n, st

    call get_command(length=n, status=st)
    if (st /= 0) error stop
    if (n <= 0) error stop

    allocate(character(len=n) :: cmd)
    st = 42
    call get_command(cmd, n, st)
    if (st /= 0) error stop

    st = 42
    call get_command(long, status=st)
    if (st /= 0) error stop
    if (long(1:n) /= cmd) error stop
    if (len_trim(long) /= n) error stop

    call get_command(short, status=st)
    if (n > len(short)) then
        if (st /= -1) error stop
        if (short /= cmd(1:len(short))) error stop
    else
        if (st /= 0) error stop
        if (short /= cmd) error stop
    end if

    st = 42
    call get_command(status=st)
    if (st /= 0) error stop
    print *, "PASS"
end program
