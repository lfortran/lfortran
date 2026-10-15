program formatted_read_08
    ! A short record is padded with blanks and the next READ starts on the next record.
    implicit none
    character(5) :: a, b
    character(3) :: c
    character(3) :: lines(2) = ['abc', 'XYZ']
    integer :: u, i, ios, n

    open(newunit=u, file='formatted_read_08.txt', status='replace')
    do i = 1, 5
        write(u, '(a)') 'abc'
        write(u, '(a)') 'XYZ'
    end do
    rewind u

    read(u, '(a5,a5)') a, b
    if (a /= 'abc' .or. b /= '') error stop 1
    read(u, '(a5)') a
    if (a /= 'XYZ') error stop 2

    read(u, '(a5,2x,a5)') a, b
    if (a /= 'abc' .or. b /= '') error stop 3
    read(u, '(a5)') a
    if (a /= 'XYZ') error stop 4

    read(u, '(a5,t1,a3)') a, c
    if (a /= 'abc' .or. c /= 'abc') error stop 5
    read(u, '(a5)') a
    if (a /= 'XYZ') error stop 6

    read(u, '(a5,tl3,a3)') a, c
    if (a /= 'abc' .or. c /= 'abc') error stop 7
    read(u, '(a5)') a
    if (a /= 'XYZ') error stop 8

    read(u, '(a5,a5)', advance='no', size=n, iostat=ios) a, b
    if (.not. is_iostat_eor(ios) .or. n /= 3 .or. a /= 'abc') error stop 9
    read(u, '(a5)') a
    if (a /= 'XYZ') error stop 10

    close(u, status='delete')

    read(lines, '(a5,t1,a3)') a, c
    if (a /= 'abc' .or. c /= 'abc') error stop 11
    print *, 'ok'
end program
