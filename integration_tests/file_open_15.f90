program file_open_15
    ! CONVERT= in OPEN selects the byte order of unformatted data:
    ! every element and every sequential record marker is converted.
    implicit none
    integer :: u, ios, i
    integer(4) :: i4
    integer(8) :: i8
    integer(2) :: i2(2)
    real(4) :: r4
    real(8) :: r8, a8(3), b8(6)
    complex(4) :: c4
    complex(8) :: z8(2), w8(2)
    logical :: l
    character(len=2) :: s
    character(len=16) :: conv
    integer(1) :: bytes(14)
    character(len=*), parameter :: filename = 'file_open_15.dat'

    ! Stream access, big endian
    open(newunit=u, file=filename, access='stream', form='unformatted', &
        status='replace', convert='big_endian', iostat=ios)
    if (ios /= 0) error stop 1
    write(u) 16909060, 'ab', [258_2, 772_2]
    close(u)
    open(newunit=u, file=filename, access='stream', form='unformatted', &
        status='old')
    read(u) bytes(1:10)
    close(u)
    if (any(bytes(1:10) /= [1_1, 2_1, 3_1, 4_1, 97_1, 98_1, 1_1, 2_1, 3_1, 4_1])) &
        error stop 2

    open(newunit=u, file=filename, access='stream', form='unformatted', &
        status='old', convert='BIG_ENDIAN')
    read(u) i4, s, i2
    close(u)
    if (i4 /= 16909060) error stop 3
    if (s /= 'ab') error stop 4
    if (any(i2 /= [258_2, 772_2])) error stop 5

    ! Sequential access: the record markers are converted too
    open(newunit=u, file=filename, form='unformatted', status='replace', &
        convert='big_endian')
    write(u) 16909060, 'ab'
    write(u) 1.0d0, (1.0, 2.0), .true.
    close(u)
    open(newunit=u, file=filename, access='stream', form='unformatted', &
        status='old')
    read(u) bytes(1:14)
    if (any(bytes(1:14) /= [0_1, 0_1, 0_1, 6_1, 1_1, 2_1, 3_1, 4_1, 97_1, 98_1, &
        0_1, 0_1, 0_1, 6_1])) error stop 6
    read(u) bytes(1:12)
    if (any(bytes(1:12) /= [0_1, 0_1, 0_1, 20_1, 63_1, -16_1, 0_1, 0_1, &
        0_1, 0_1, 0_1, 0_1])) error stop 7
    read(u) bytes(1:12)
    if (any(int(bytes(1:12)) /= [63, -128, 0, 0, 64, 0, 0, 0, 0, 0, 0, 1])) &
        error stop 8
    close(u)
    open(newunit=u, file=filename, form='unformatted', status='old', &
        convert='big_endian')
    read(u) i4, s
    read(u) r8, c4, l
    close(u)
    if (i4 /= 16909060) error stop 9
    if (s /= 'ab') error stop 10
    if (r8 /= 1.0d0) error stop 11
    if (c4 /= (1.0, 2.0)) error stop 12
    if (.not. l) error stop 13

    ! Little endian, with the value given at run time
    conv = 'little_endian'
    open(newunit=u, file=filename, access='stream', form='unformatted', &
        status='replace', convert=conv)
    write(u) 16909060
    close(u)
    open(newunit=u, file=filename, access='stream', form='unformatted', &
        status='old')
    read(u) bytes(1:4)
    close(u)
    if (any(bytes(1:4) /= [4_1, 3_1, 2_1, 1_1])) error stop 14

    ! Other kinds, arrays and array sections round trip
    a8 = [1.5d0, -2.25d0, 1.0d10]
    z8 = [(1.0d0, -1.0d0), (2.5d0, 3.5d0)]
    open(newunit=u, file=filename, form='unformatted', status='replace', &
        convert='swap')
    write(u) 1234567890123_8, 3.5, a8, z8
    close(u)
    b8 = 0
    open(newunit=u, file=filename, form='unformatted', status='old', &
        convert='swap')
    read(u) i8, r4, b8(1:5:2), w8
    close(u, status='delete')
    if (i8 /= 1234567890123_8) error stop 15
    if (r4 /= 3.5) error stop 16
    do i = 1, 3
        if (b8(2*i - 1) /= a8(i)) error stop 17
        if (b8(2*i) /= 0) error stop 18
    end do
    if (any(w8 /= z8)) error stop 19

    print *, i4, i8, r4, s
end program file_open_15
