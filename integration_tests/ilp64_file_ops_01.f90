! Test: file I/O operations with -fdefault-integer-8 (ILP64 mode)
! Fixes #9772
program ilp64_file_ops_01
    implicit none
    integer :: u
    character(len=10) :: line

    u = 10
    open(u, file="/tmp/test_ilp64_file_ops.txt", status="replace")
    write(u, '(A)') "line1"
    write(u, '(A)') "line2"

    ! Test rewind
    rewind(u)
    read(u, '(A)') line
    if (trim(line) /= "line1") error stop "rewind failed"

    ! Test backspace
    backspace(u)
    read(u, '(A)') line
    if (trim(line) /= "line1") error stop "backspace failed"

    ! Test flush
    flush(u)

    close(u)
    print *, "PASS"
end program
