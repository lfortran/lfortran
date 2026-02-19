program file_01
    integer :: iostat
    character(len=100) :: iomsg
    open(10, delim="xyz", iostat=iostat, iomsg=iomsg)
    print *, iostat
    print *, iomsg

    if (iostat /= 5002) error stop
    if (len(trim(iomsg)) <= 0) error stop
end program
