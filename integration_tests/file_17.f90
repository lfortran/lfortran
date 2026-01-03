program file_17

    integer :: i
    real :: arr(3)
    real :: a, b, c
    arr(1) = 100.123
    arr(2) = -2.14
    arr(3) = 528.156

    open(10, file="file_17_util.txt", status="replace")
    write(10, "((es23.16))") (arr(i), i=1, size(arr))
    close(10)

    open(10, file="file_17_util.txt")
    read(10, *) a, b, c
    close(10)

    print *, arr(1), a
    print *, arr(2), b
    print *, arr(3), c

    if (abs(arr(1) - a) > 1e-5) error stop
    if (abs(arr(2) - b) > 1e-5) error stop
    if (abs(arr(3) - c) > 1e-5) error stop

end program file_17
