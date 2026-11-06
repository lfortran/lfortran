program inquire_20
    ! INQUIRE(POS=) on a formatted sequential unit in a write loop, with
    ! non-advancing writes, FILE=, REWIND, empty records and ACTION='write'.
    implicit none
    integer :: u, p, i, nbytes
    character(len=8) :: buf

    open(newunit=u, file='inquire_20_a.txt', status='replace', &
        form='formatted', access='sequential')
    write(u, '(a)') 'Line 1'
    inquire(unit=u, pos=p)
    if (p /= 6) error stop 1
    write(u, '(a)') 'Hello World!'
    inquire(unit=u, pos=p)
    if (p /= 18) error stop 2
    inquire(file='inquire_20_a.txt', pos=p)
    if (p /= 18) error stop 3

    write(u, '(a)', advance='no') 'abc'
    inquire(unit=u, pos=p)
    if (p /= 21) error stop 4
    write(u, '(a)', advance='no') 'defg'
    inquire(unit=u, pos=p)
    if (p /= 25) error stop 5
    write(u, '(a)') 'h'
    inquire(unit=u, pos=p)
    if (p /= 26) error stop 6

    ! Each record i holds its digits; the position counts no terminators.
    nbytes = 26
    do i = 1, 2000
        write(u, '(i0)') i
        write(buf, '(i0)') i
        nbytes = nbytes + len_trim(buf)
        inquire(unit=u, pos=p)
        if (p /= nbytes) error stop 7
    end do
    inquire(file='inquire_20_a.txt', pos=p)
    if (p /= nbytes) error stop 8

    rewind(u)
    inquire(unit=u, pos=p)
    if (p /= 1) error stop 9
    close(u, status='delete')

    open(newunit=u, file='inquire_20_b.txt', status='replace')
    write(u, '(a)') ''
    inquire(unit=u, pos=p)
    if (p /= 0) error stop 10
    write(u, '(a)') ''
    inquire(unit=u, pos=p)
    if (p /= 0) error stop 11
    write(u, '(a)') 'xyz'
    inquire(unit=u, pos=p)
    if (p /= 3) error stop 12
    close(u, status='delete')

    open(newunit=u, file='inquire_20_c.txt', status='replace', action='write')
    write(u, '(a)') 'abc'
    inquire(unit=u, pos=p)
    if (p /= 3) error stop 13
    write(u, '(a)') 'defgh'
    inquire(unit=u, pos=p)
    if (p /= 8) error stop 14
    close(u, status='delete')
    print *, 'ok'
end program
