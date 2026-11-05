program file_76
    implicit none

    ! POS= is allowed only for a unit connected for stream access. On a
    ! formatted sequential unit it must report an error through IOSTAT= and
    ! must not move the unit (#16828).
    integer :: u, ios, pos_before, pos_after, value
    character(8) :: s

    open(newunit=u, file='file_76.txt', status='replace', &
         access='sequential', form='formatted')
    write(u, '(a)') 'aaaa'
    write(u, '(a)') 'bbbb'

    ! WRITE with POS= on a sequential unit fails and leaves the file alone.
    inquire(unit=u, pos=pos_before)
    write(u, '(a)', pos=1, iostat=ios) 'qqqq'
    if (ios == 0) error stop 'WRITE with POS= on a sequential unit was accepted'
    inquire(unit=u, pos=pos_after)
    if (pos_after /= pos_before) then
        error stop 'failed WRITE with POS= moved the unit'
    end if

    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'READ after failed WRITE with POS='
    if (trim(s) /= 'aaaa') then
        error stop 'failed WRITE with POS= changed a record'
    end if

    ! READ with POS= on a sequential unit fails without transferring data.
    inquire(unit=u, pos=pos_before)
    s = 'sentinel'
    read(u, '(a)', pos=1, iostat=ios) s
    if (ios == 0) error stop 'READ with POS= on a sequential unit was accepted'
    inquire(unit=u, pos=pos_after)
    if (pos_after /= pos_before) then
        error stop 'failed READ with POS= moved the unit'
    end if
    if (s /= 'sentinel') then
        error stop 'failed READ with POS= transferred data'
    end if

    close(u, status='delete')

    ! POS= still works on a stream unit.
    open(newunit=u, file='file_76.dat', status='replace', &
         access='stream', form='unformatted')
    write(u, pos=1) 7
    write(u, pos=5) 9
    read(u, pos=1) value
    if (value /= 7) error stop 'stream READ with POS= failed'
    read(u, pos=5) value
    if (value /= 9) error stop 'stream READ with POS= failed'
    close(u, status='delete')
end program
