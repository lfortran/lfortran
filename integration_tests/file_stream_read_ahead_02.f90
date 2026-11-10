program file_stream_read_ahead_02
    ! A stream file read into a big array by one statement: the data, the
    ! position after the read, a read that reaches the end of the file, and
    ! the number of read system calls (the read goes straight into the array
    ! by a few calls, not through the 8 KiB buffer of the stream).
    implicit none
    integer, parameter :: n = 12*1024*1024 + 1081
    integer, allocatable :: expected(:), got(:)
    integer :: head(4)
    integer :: unit, ios, i
    integer(8) :: state, before, after
    character(len=*), parameter :: name = "file_stream_read_ahead_02.bin"

    allocate(expected(n), got(n))
    state = 987
    do i = 1, n
        state = mod(state*1103515245_8 + 12345_8, 2147483648_8)
        expected(i) = int(state)
    end do
    open(newunit=unit, file=name, access='stream', form='unformatted', &
        action='write', status='replace', iostat=ios)
    if (ios /= 0) error stop "open for writing"
    write(unit) expected
    close(unit)

    open(newunit=unit, file=name, access='stream', form='unformatted', &
        action='read', status='old', iostat=ios)
    if (ios /= 0) error stop "open for reading"
    ! Small reads first, so that the stream holds a buffer.
    read(unit, pos=1_8, iostat=ios) head
    if (ios /= 0) error stop "head"
    before = read_calls()
    got = 0
    read(unit, pos=401_8, iostat=ios) got(:n-100)
    if (ios /= 0) error stop "big read"
    after = read_calls()
    do i = 1, n-100
        if (got(i) /= expected(i+100)) error stop "big read data"
    end do
    if (before >= 0 .and. after - before > 40) then
        print *, "read calls:", after - before
        error stop "too many read calls"
    end if
    ! The next read continues where the big one ended.
    read(unit, pos=4_8*n - 15, iostat=ios) head
    do i = 1, 4
        if (head(i) /= expected(n - 4 + i)) error stop "tail"
    end do
    ! Two big reads one after the other, without pos.
    read(unit, pos=1_8, iostat=ios) got(:(n/2))
    read(unit, iostat=ios) got((n/2)+1:n)
    if (ios /= 0) error stop "second half"
    do i = 1, n
        if (got(i) /= expected(i)) error stop "two reads data"
    end do
    ! A big read that passes the end of the file is an error.
    read(unit, pos=4_8*n - 2_8*1024*1024, iostat=ios) got(:4*1024*1024)
    if (ios == 0) error stop "read past the end"
    ! The file can still be read after that.
    read(unit, pos=5_8, iostat=ios) head(:3)
    if (ios /= 0) error stop "read after the error"
    if (any(head(:3) /= expected(2:4))) error stop "after the error"
    close(unit, status='delete')
    print *, "ok"
contains
    ! The read system calls of the process so far, or -1 when it is not known
    ! (it is read with the C library: /proc files cannot be opened for update).
    integer(8) function read_calls() result(r)
        use iso_c_binding
        interface
            integer(c_int) function c_open(path, flags) bind(c, name="open")
                import :: c_int, c_char
                character(kind=c_char), intent(in) :: path(*)
                integer(c_int), value :: flags
            end function
            integer(c_size_t) function c_read(fd, buf, count) bind(c, name="read")
                import :: c_int, c_char, c_size_t
                integer(c_int), value :: fd
                character(kind=c_char), intent(out) :: buf(*)
                integer(c_size_t), value :: count
            end function
            integer(c_int) function c_close(fd) bind(c, name="close")
                import :: c_int
                integer(c_int), value :: fd
            end function
        end interface
        character(kind=c_char) :: buf(2048)
        character(len=2048) :: text
        integer(c_int) :: fd, ignored
        integer(c_size_t) :: got
        integer :: i, p, s
        r = -1
        fd = c_open("/proc/self/io"//c_null_char, 0_c_int)
        if (fd < 0) return
        got = c_read(fd, buf, 2048_c_size_t)
        ignored = c_close(fd)
        if (got <= 0) return
        text = ""
        do i = 1, int(got)
            text(i:i) = buf(i)
        end do
        p = index(text, "syscr:")
        if (p == 0) return
        read(text(p+6:), *, iostat=s) r
        if (s /= 0) r = -1
    end function
end program
