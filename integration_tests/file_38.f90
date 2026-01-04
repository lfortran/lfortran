program file_38
    ! MRE: action='read' should not allow writes
    implicit none
    integer :: u, ios

    ! Create test file
    open(newunit=u, file="file_38.dat", status="replace")
    write(u, *) "original"
    close(u)

    ! Open with action='read' and status='old'
    open(newunit=u, file="file_38.dat", action='read', status='old')

    ! Try to write - this MUST fail with action='read'
    write(u, *, iostat=ios) "overwritten"
    close(u)

    if (ios == 0) error stop "BUG: write succeeded with action=read"
    if (ios < 0) error stop "BUG: expected positive iostat for error (not EOF)"
    print *, "PASSED: write blocked with ios=", ios
end program
