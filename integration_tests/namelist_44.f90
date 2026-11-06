program namelist_44
    ! IOMSG= is assigned an explanatory message when a namelist READ fails,
    ! and left unchanged when it succeeds
    implicit none
    integer :: x, y(2), ios, u
    character(len=200) :: msg
    character(len=40) :: buf
    namelist /g/ x, y

    open(newunit=u, file='namelist_44.nml', status='replace')
    write(u, '(a)') '&g x=1, bogus=2 /'
    write(u, '(a)') '&g x=3, y(5)=1 /'
    write(u, '(a)') '&g x=4 /'
    close(u)

    open(newunit=u, file='namelist_44.nml', status='old')
    ! An object name that is not in the group
    msg = 'iomsg not set'
    read(u, nml=g, iostat=ios, iomsg=msg)
    if (ios <= 0) error stop 1
    if (msg == 'iomsg not set') error stop 2
    if (len_trim(msg) == 0) error stop 3

    ! A subscript out of the array bounds
    msg = 'iomsg not set'
    read(u, nml=g, iostat=ios, iomsg=msg)
    if (ios <= 0) error stop 4
    if (msg == 'iomsg not set') error stop 5
    if (len_trim(msg) == 0) error stop 6

    ! A successful read leaves IOMSG= unchanged
    msg = 'iomsg not set'
    read(u, nml=g, iostat=ios, iomsg=msg)
    if (ios /= 0) error stop 7
    if (x /= 4) error stop 8
    if (msg /= 'iomsg not set') error stop 9

    ! End of file before the group is found
    msg = 'iomsg not set'
    read(u, nml=g, iostat=ios, iomsg=msg)
    if (ios == 0) error stop 10
    if (msg == 'iomsg not set') error stop 11
    close(u, status='delete')

    ! Internal file
    buf = '&g x=5, bogus=6 /'
    msg = 'iomsg not set'
    read(buf, nml=g, iostat=ios, iomsg=msg)
    if (ios <= 0) error stop 12
    if (msg == 'iomsg not set') error stop 13
    if (len_trim(msg) == 0) error stop 14
    print '(a)', 'ok'
end program namelist_44
