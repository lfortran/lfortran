program read_stdin_01
    ! Interactive standard-input reads must stop at the newline that
    ! terminates each record (see #12108). The loader selects one read kind
    ! per process so that one kind cannot mask another's buffering bug:
    ! "formatted" runs a format-directed READ, "list" a list-directed one.
    implicit none
    character(len=16) :: mode
    character(len=1) :: c
    integer :: n, ios, nargs

    nargs = command_argument_count()
    if (nargs >= 1) then
        call get_command_argument(1, mode)
    else
        mode = "formatted"
    end if

    if (trim(mode) == "list") then
        print '(a)', 'LIST?'
        read (*, *, iostat=ios) n
        if (ios /= 0) error stop "list-directed read failed"
        if (n /= 42) error stop "wrong integer read"
        print '(a,i0)', 'GOTN:', n
    else
        print '(a)', 'INPUT?'
        read (*, '(A1)', iostat=ios) c
        if (ios /= 0) error stop "formatted read failed"
        if (c /= 'A') error stop "wrong character read"
        print '(2a)', 'GOTC:', c
    end if
end program
