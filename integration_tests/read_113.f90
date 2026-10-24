program read_113
    ! A numeric or logical edit descriptor for a character item is an
    ! error condition of a formatted READ: IOSTAT= gets a nonzero value
    implicit none
    integer :: ios, i, j, n, k
    character(len=4) :: c, cc(2,2)
    character(len=16) :: s = '  12  34  56  78'

    n = 2
    read(s, '(I4)', iostat=ios) c
    if (ios == 0) error stop 1
    read(s, '(F4.0)', iostat=ios) c
    if (ios == 0) error stop 2
    read(s, '(L4)', iostat=ios) c
    if (ios == 0) error stop 3
    k = 0
    read(s, '(2I4)', iostat=ios) k, c
    if (ios == 0) error stop 4
    if (k /= 12) error stop 5
    read(s, '(4I4)', iostat=ios) ((cc(i,j), j=1,n), i=1,n)
    if (ios == 0) error stop 6
    print *, "ok"
end program read_113
