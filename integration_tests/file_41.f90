program file_41
    implicit none

    integer :: unit, ios
    character(len=*), parameter :: fname = "test_stream.txt"
    character(len=*), parameter :: expected = "build"
    character(len=len(expected)) :: found

    ! ---- Write ----
    open(newunit=unit, file=fname, access="stream", action="write", iostat=ios)
    if (ios /= 0) error stop "open(write) failed"

    write(unit, iostat=ios) expected
    if (ios /= 0) error stop "write failed"

    close(unit)

    ! ---- Read back ----
    open(newunit=unit, file=fname, access="stream", action="read", iostat=ios)
    if (ios /= 0) error stop "open(read) failed"

    read(unit, iostat=ios) found
    if (ios /= 0) error stop "read failed"

    close(unit)

    ! ---- Verify ----
    if (found /= expected) then
        print *, "EXPECTED:", expected
        print *, "FOUND:   ", found
        error stop "content mismatch"
    end if

    ! ---- Delete file ----
    open(newunit=unit, file=fname, status="old", iostat=ios)
    if (ios /= 0) error stop "reopen for delete failed"
    close(unit, status="delete")

    print *, "PASS: stream write/read/delete successful"

end program file_41
