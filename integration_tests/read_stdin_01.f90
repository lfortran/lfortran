program read_stdin_01
    ! Interactive standard-input reads must stop at the newline that
    ! terminates each record (#12108).  The loader selects one kind of
    ! read per process so that a guarded first read cannot mask another
    ! kind's buffering bug: "formatted", "list", "unit5", "array", "real",
    ! "logical", "list2", "mixed" (list then formatted, #12656), "bare",
    ! "u5mix", "advmix" and "multi" (three formatted records).
    implicit none
    character(len=16) :: mode
    character(len=1) :: c
    integer :: n, ios, nargs, i
    integer :: arr(3)
    logical :: l
    real :: x

    mode = "formatted"
    nargs = command_argument_count()
    if (nargs >= 1) call get_command_argument(1, mode)

    if (trim(mode) == "list") then
        print '(a)', 'LIST?'
        read (*, *, iostat=ios) n
        if (ios /= 0) error stop "list-directed read failed"
        if (n /= 42) error stop "wrong list-directed value"
        print '(a,i0)', 'GOTN:', n
    else if (trim(mode) == "unit5") then
        print '(a)', 'U5?'
        read (5, *, iostat=ios) n
        if (ios /= 0) error stop "unit 5 read failed"
        if (n /= 42) error stop "wrong unit 5 value"
        print '(a,i0)', 'GOTU:', n
    else if (trim(mode) == "array") then
        print '(a)', 'ARR?'
        read (*, *, iostat=ios) arr
        if (ios /= 0) error stop "array read failed"
        if (arr(1) + arr(2) + arr(3) /= 6) error stop "wrong array values"
        print '(a,i0)', 'GOTA:', arr(1) + arr(2) + arr(3)
    else if (trim(mode) == "real") then
        print '(a)', 'REAL?'
        read (*, *, iostat=ios) x
        if (ios /= 0) error stop "real read failed"
        if (abs(x - 1.5) > 0.01) error stop "wrong real value"
        print '(a,f3.1)', 'GOTR:', x
    else if (trim(mode) == "logical") then
        print '(a)', 'LOG?'
        read (*, *, iostat=ios) l
        if (ios /= 0) error stop "logical read failed"
        if (.not. l) error stop "wrong logical value"
        print '(a,l1)', 'GOTL:', l
    else if (trim(mode) == "list2") then
        print '(a)', 'LIST?'
        read (*, *, iostat=ios) n
        if (ios /= 0 .or. n /= 42) error stop "first list read failed"
        print '(a,i0)', 'GOTN:', n
        print '(a)', 'LIST?'
        read (*, *, iostat=ios) n
        if (ios /= 0 .or. n /= 43) error stop "second list read failed"
        print '(a,i0)', 'GOTN2:', n
    else if (trim(mode) == "mixed") then
        ! Issue #12656: list-directed reads leave the record terminator in
        ! the stream.  The subsequent formatted read must not misinterpret
        ! it as an empty record.
        print '(a)', 'LIST?'
        read (*, *, iostat=ios) n
        if (ios /= 0 .or. n /= 42) error stop "mixed list read failed"
        print '(a,i0)', 'GOTN:', n
        print '(a)', 'INPUT?'
        read (*, '(A1)', iostat=ios) c
        if (ios /= 0 .or. c /= 'A') error stop "mixed formatted read failed"
        print '(2a)', 'GOTC:', c
    else if (trim(mode) == "bare") then
        read (*,*)                        ! empty READ skips one record
        read (*, *, iostat=ios) n
        if (ios /= 0 .or. n /= 42) error stop "bare read failed"
        print '(a,i0)', 'GOTN:', n
    else if (trim(mode) == "u5mix") then
        read (5, *, iostat=ios) n
        if (ios /= 0 .or. n /= 42) error stop "unit 5 list read failed"
        print '(a,i0)', 'GOTN:', n
        read (5, '(A1)', iostat=ios) c
        if (ios /= 0 .or. c /= 'A') error stop "unit 5 formatted read failed"
        print '(2a)', 'GOTC:', c
    else if (trim(mode) == "advmix") then
        read (*, '(a)', advance='no')     ! must not skip the record
        read (*, *, iostat=ios) n
        if (ios /= 0 .or. n /= 42) error stop "advance=no read failed"
        print '(a,i0)', 'GOTN:', n
    else if (trim(mode) == "multi") then
        do i = 1, 3
            print '(a)', 'LINE?'
            read (*, '(A1)', iostat=ios) c
            if (ios /= 0) error stop "multi read failed"
            if (c /= 'A') error stop "wrong multi value"
        end do
        print '(a)', 'GOT3:done'
    else
        print '(a)', 'INPUT?'
        read (*, '(A1)', iostat=ios) c
        if (ios /= 0) error stop "formatted read failed"
        if (c /= 'A') error stop "wrong character read"
        print '(2a)', 'GOTC:', c
    end if
end program
