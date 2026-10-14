program read_crlf_01
    implicit none
    integer :: unit, ios, count, mode
    character(8) :: line
    character(3) :: prefix
    character(1) :: tail
    character(:), allocatable :: ending
    real :: value
    real(8) :: wide_value, values(2)

    do mode = 1, 3
        ending = achar(10)
        if (mode /= 1) ending = achar(13) // achar(10)
        open(newunit=unit, file='read_crlf_01.dat', access='stream', &
            form='unformatted', status='replace')
        write(unit) '1.5' // ending // '2.5' // ending // ending
        if (mode == 3) then
            write(unit) '3.5' // ending
        else
            write(unit) '3.5'
        end if
        close(unit)
        if (mode == 3) then
            open(newunit=unit, file='read_crlf_01.dat', status='old', access='stream', form='formatted')
        else
            open(newunit=unit, file='read_crlf_01.dat', status='old')
        end if

        ! A CRLF terminator must not become part of the character value.
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '1.5     ') error stop 1
        read(line, *, iostat=ios) value
        if (ios /= 0 .or. value /= 1.5) error stop 2
        read(line, *, iostat=ios) wide_value
        if (ios /= 0 .or. wide_value /= 1.5_8) error stop 3
        backspace(unit)
        read(unit, '(f8.1)', iostat=ios) wide_value
        if (ios /= 0 .or. wide_value /= 1.5_8) error stop 4
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '2.5     ') error stop 5
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '        ') error stop 6
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '3.5     ') error stop 7
        read(unit, '(a)', iostat=ios) line
        if (ios /= -1) error stop 8

        rewind(unit)
        ! The CR falls just outside the first field's buffer.
        read(unit, '(a3)', advance='no', iostat=ios) prefix
        if (ios /= 0 .or. prefix /= '1.5') error stop 9
        read(unit, '(a1)', advance='no', iostat=ios, size=count) tail
        if (ios /= -2 .or. count /= 0 .or. tail /= ' ') error stop 10
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '2.5     ') error stop 11

        rewind(unit)
        ! Slash and format reversion advance over the whole terminator.
        read(unit, '(f8.1,/,f8.1)', iostat=ios) values
        if (ios /= 0 .or. any(values /= [1.5_8, 2.5_8])) error stop 12
        rewind(unit)
        read(unit, '(f8.1)', iostat=ios) values
        if (ios /= 0 .or. any(values /= [1.5_8, 2.5_8])) error stop 13
        rewind(unit)
        read(unit, '(t10,a1)', iostat=ios) tail
        if (ios /= 0 .or. tail /= ' ') error stop 14
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '2.5     ') error stop 15
        rewind(unit)
        read(unit, '(3x,a1)', iostat=ios) tail
        if (ios /= 0 .or. tail /= ' ') error stop 16
        rewind(unit)
        read(unit, '(a)', pad='no', iostat=ios) line
        if (ios /= -2) error stop 17
        rewind(unit)
        read(unit, '(a)') line
        endfile(unit)
        rewind(unit)
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0 .or. line /= '1.5     ') error stop 18
        read(unit, '(a)', iostat=ios) line
        if (ios /= -1) error stop 19
        close(unit, status='delete')
    end do

    ! Internal characters are data, including CR followed by LF.
    line = 'x' // achar(13) // achar(10)
    read(line, '(a2)') prefix
    if (prefix /= 'x' // achar(13) // ' ') error stop 20
end program read_crlf_01
