! Test: logical I/O with -fdefault-integer-8 (ILP64 mode)
! Fixes #9770
program ilp64_logical_read_01
    implicit none
    logical :: flag1, flag2

    ! Write logical values to temp file
    open(10, file="/tmp/test_ilp64_logical.txt", status="replace")
    write(10, *) .true.
    write(10, *) .false.
    close(10)

    ! Read them back
    open(10, file="/tmp/test_ilp64_logical.txt", status="old")
    read(10, *) flag1
    read(10, *) flag2
    close(10)

    if (.not. flag1) error stop "flag1 should be true"
    if (flag2) error stop "flag2 should be false"
    print *, "PASS"
end program
