program file_stream_read_ahead_01
    ! A stream file of many megabytes read in small pieces, in order, then
    ! with jumps back and forth, to the end and across it.
    implicit none
    integer, parameter :: n = 6*1024*1024 + 12345, piece = 65536
    character(len=piece) :: buffer
    character(len=1), allocatable :: expected(:)
    integer :: unit, ios, i, k, j
    integer(8) :: pos, state, total
    character(len=*), parameter :: name = "file_stream_read_ahead_01.bin"

    allocate(expected(n))
    state = 12345
    do i = 1, n
        state = mod(state*1103515245_8 + 12345_8, 2147483648_8)
        expected(i) = achar(int(mod(state/65536, 256_8)))
    end do
    open(newunit=unit, file=name, access='stream', form='unformatted', &
        action='write', status='replace', iostat=ios)
    if (ios /= 0) error stop "open for writing"
    write(unit) expected
    close(unit)

    open(newunit=unit, file=name, access='stream', form='unformatted', &
        action='read', status='old', iostat=ios)
    if (ios /= 0) error stop "open for reading"
    total = 0
    pos = 1
    do while (pos + piece - 1 <= n)
        read(unit, pos=pos, iostat=ios) buffer
        if (ios /= 0) error stop "sequential read"
        do j = 1, piece, 4099
            if (buffer(j:j) /= expected(pos + j - 1)) error stop "sequential data"
        end do
        total = total + piece
        pos = pos + piece
    end do
    if (total /= (n/piece)*int(piece, 8)) error stop "sequential total"

    ! Jumps: backward, far forward, and back into the middle.
    do k = 1, 40
        select case (mod(k, 4))
        case (0); pos = 1 + int(mod(k*7919_8, int(n - piece, 8)))
        case (1); pos = int(n, 8) - piece - 3*k
        case (2); pos = 1 + 1000_8*k
        case default; pos = 1 + int(mod(k*104729_8, int(n - piece, 8)))
        end select
        read(unit, pos=pos, iostat=ios) buffer
        if (ios /= 0) error stop "jump read"
        do j = 1, piece, 1021
            if (buffer(j:j) /= expected(pos + j - 1)) error stop "jump data"
        end do
    end do

    ! Pieces that straddle several windows, continuing without a pos.
    read(unit, pos=3_8, iostat=ios) buffer(:1000)
    do i = 1, 100
        read(unit, iostat=ios) buffer(:40000 + 7*i)
        if (ios /= 0) error stop "continued read"
    end do

    ! Reading past the end is an error and the end is still readable.
    read(unit, pos=int(n, 8) - 99, iostat=ios) buffer(:200)
    if (ios == 0) error stop "read past the end"
    read(unit, pos=int(n, 8) - 99, iostat=ios) buffer(:100)
    if (ios /= 0) error stop "read of the last bytes"
    do j = 1, 100
        if (buffer(j:j) /= expected(n - 100 + j)) error stop "last bytes"
    end do
    close(unit, status='delete')
    print *, "ok"
end program
