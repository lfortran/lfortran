program read_17
    ! Test combined END= and ERR= labels
    implicit none
    integer :: x, u, count

    open(newunit=u, status='scratch')
    write(u, '(I5)') 42
    write(u, '(I5)') 99
    rewind(u)

    count = 0
10  continue
    read(u, *, end=30, err=20) x
    count = count + 1
    if (count == 1 .and. x /= 42) error stop "First value should be 42"
    if (count == 2 .and. x /= 99) error stop "Second value should be 99"
    goto 10

20  continue
    error stop "Should not reach ERR label with valid data"

30  continue
    close(u)
    if (count /= 2) error stop "Should have read 2 values"
    print *, "PASS: combined END=/ERR= works"
end program read_17
