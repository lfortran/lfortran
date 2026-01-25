program namelist_22
    character(len=1), allocatable :: a(:)
    character(len=200) :: buf
    integer :: u, ios
    logical :: found
    namelist /n/ a

    allocate(a(1))
    a(1) = "x"
    open(newunit=u, status="scratch", action="readwrite")
    write(u, nml=n)
    rewind(u)
    found = .false.
    do
        read(u, "(A)", iostat=ios) buf
        if (ios /= 0) exit
        if (index(buf, "x") /= 0) found = .true.
    end do
    close(u)
    if (.not. found) error stop
end program namelist_22
