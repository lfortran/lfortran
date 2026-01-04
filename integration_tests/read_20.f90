program read_20
    ! Test iostat is set correctly on integer overflow
    implicit none
    integer :: x, u, ios

    open(newunit=u, file='_test_overflow.txt', status='replace')
    write(u, '(A)') "2719"
    write(u, '(A)') "2147483648"  ! INT32_MAX + 1
    close(u)

    open(newunit=u, file='_test_overflow.txt', status='old')

    ! First read should succeed
    read(u, *, iostat=ios) x
    if (ios /= 0) error stop "First read should succeed"
    if (x /= 2719) error stop "First value should be 2719"

    ! Second read should fail with overflow, iostat should be non-zero
    read(u, *, iostat=ios) x
    if (ios == 0) error stop "Second read should fail (overflow)"

    close(u, status='delete')
    print *, "PASS: iostat correctly set on overflow"
end program read_20
