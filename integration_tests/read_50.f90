program read_50
    implicit none
    integer :: n, ios
    real :: r
    character(len=20) :: str
    character(len=100) :: msg

    ! Test 1: successful read from string, iostat should be 0
    ios = 999
    str = "42"
    read(str, *, iostat=ios) n
    if (ios /= 0) error stop
    if (n /= 42) error stop

    ! Test 2: successful read of real from string
    ios = 999
    str = "3.14"
    read(str, *, iostat=ios) r
    if (ios /= 0) error stop
    if (abs(r - 3.14) > 0.001) error stop

    ! Test 3: successful read from file
    ios = 999
    open(unit=10, file="read_50_test.txt", status="replace", action="write")
    write(10, '(I5)') 666
    close(10)
    open(unit=10, file="read_50_test.txt", status="old", action="read")
    read(10, *, iostat=ios) n
    close(10, status="delete")
    if (ios /= 0) error stop
    if (n /= 666) error stop

    ! Test 4: read error from string, iostat should be positive
    ios = 999
    str = "abc"
    read(str, *, iostat=ios) n
    if (ios <= 0) error stop

    ! Test 5: successful read with iomsg, msg should not be set
    ios = 999
    msg = "initial"
    str = "77"
    read(str, *, iostat=ios, iomsg=msg) n
    if (ios /= 0) error stop
    if (n /= 77) error stop

    ! Test 6: read error with iomsg, msg should contain error description
    ios = 999
    msg = ""
    str = "xyz"
    read(str, *, iostat=ios, iomsg=msg) n
    if (ios <= 0) error stop
    if (len_trim(msg) == 0) error stop

    ! Test 7: successful file read with iomsg
    ios = 999
    msg = ""
    open(unit=11, file="read_50_test2.txt", status="replace", action="write")
    write(11, '(I5)') 123
    close(11)
    open(unit=11, file="read_50_test2.txt", status="old", action="read")
    read(11, *, iostat=ios, iomsg=msg) n
    close(11, status="delete")
    if (ios /= 0) error stop
    if (n /= 123) error stop
end program read_50
