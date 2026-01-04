program read_18
    ! Test IOSTAT with END= label interaction
    implicit none
    integer :: x, u, ios

    open(newunit=u, status='scratch')
    write(u, '(I5)') 123
    rewind(u)

    ! First read should succeed
    read(u, *, iostat=ios, end=10) x
    if (ios /= 0) error stop "iostat should be 0 on success"
    if (x /= 123) error stop "Value should be 123"

    ! Second read should hit EOF and jump to label
    read(u, *, iostat=ios, end=10) x
    error stop "Should have jumped to END label"

10  continue
    close(u)
    ! iostat should be negative at EOF
    if (ios >= 0) error stop "iostat should be negative at EOF"
    print *, "PASS: IOSTAT + END= interaction works"
end program read_18
