program namelist_iomsg_01
    ! A failing namelist READ with IOSTAT= assigns an explanatory IOMSG=;
    ! a successful one leaves IOMSG= unchanged.
    implicit none
    integer :: x, y, ios, u
    character(len=100) :: msg
    character(len=20) :: line
    character(len=20) :: lines(2)
    namelist /g/ x, y

    open(newunit=u, file='namelist_iomsg_01.nml', status='replace')
    write(u, '(a)') '&g bogus=2 /'
    write(u, '(a)') '&g x=1 /'
    rewind(u)

    msg = 'unset'
    ios = 0
    read(u, nml=g, iostat=ios, iomsg=msg)
    print *, ios, trim(msg)
    if (ios <= 0) error stop 1
    if (msg == 'unset' .or. len_trim(msg) == 0) error stop 2

    rewind(u)
    read(u, '(a)') line
    x = 0
    msg = 'unchanged'
    read(u, nml=g, iostat=ios, iomsg=msg)
    close(u, status='delete')
    if (ios /= 0) error stop 3
    if (x /= 1) error stop 4
    if (msg /= 'unchanged') error stop 5

    line = '&g y=3, bogus=4 /'
    msg = 'unset'
    read(line, nml=g, iostat=ios, iomsg=msg)
    print *, ios, trim(msg)
    if (ios <= 0) error stop 6
    if (msg == 'unset' .or. len_trim(msg) == 0) error stop 7

    lines(1) = '&g x=5,'
    lines(2) = ' bogus=6 /'
    msg = 'unset'
    read(lines, nml=g, iostat=ios, iomsg=msg)
    print *, ios, trim(msg)
    if (ios <= 0) error stop 8
    if (msg == 'unset' .or. len_trim(msg) == 0) error stop 9

    line = '&g x=7, y=8 /'
    msg = 'unchanged'
    read(line, nml=g, iostat=ios, iomsg=msg)
    if (ios /= 0) error stop 10
    if (x /= 7 .or. y /= 8) error stop 11
    if (msg /= 'unchanged') error stop 12
end program
