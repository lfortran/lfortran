program iomsg_deferred_char_01
    ! Deferred-length allocatable and pointer character variables
    ! are valid iomsg= variables in READ and WRITE statements.
    implicit none
    character(len=:), allocatable :: msg
    character(len=:), pointer :: pmsg
    character(len=8) :: buf
    integer :: u, ios, n, i

    msg = repeat(' ', 99)
    allocate(character(len=60) :: pmsg)

    open(newunit=u, file='iomsg_deferred_char_01.bin', access='stream', &
        form='unformatted', status='replace')
    write(u, iostat=ios, iomsg=msg) 'abcdefgh'
    if (ios /= 0) error stop
    close(u)
    open(newunit=u, file='iomsg_deferred_char_01.bin', access='stream', &
        form='unformatted', iostat=ios, iomsg=msg)
    if (ios /= 0) error stop
    read(u, iostat=ios, iomsg=msg) buf
    if (ios /= 0) error stop
    if (buf /= 'abcdefgh') error stop
    close(u, status='delete')

    open(newunit=u, file='iomsg_deferred_char_01.txt', status='replace')
    write(u, '(a)', iostat=ios, iomsg=pmsg) 'ijkl'
    if (ios /= 0) error stop
    rewind(u)
    buf = ''
    read(u, '(a)', advance='no', size=n, iostat=ios, iomsg=pmsg) buf
    if (ios /= -2) error stop
    if (n /= 4) error stop
    if (buf /= 'ijkl') error stop

    ! Error path: the message is stored in the deferred-length variables
    msg = repeat(' ', 99)
    pmsg = ''
    read(u, *, iostat=ios, iomsg=msg) i
    if (ios /= -1) error stop
    if (len_trim(msg) == 0) error stop
    rewind(u)
    read(u, '(a)') buf
    read(u, *, iostat=ios, iomsg=pmsg) i
    if (ios /= -1) error stop
    if (len_trim(pmsg) == 0) error stop
    close(u, status='delete')
    deallocate(pmsg)
    print *, 'ok'
end program
