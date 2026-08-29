program endfile_iomsg_01
    implicit none
    integer :: ios
    integer, parameter :: u = 10
    character(len=128) :: msg = ""

    open(unit=u, file="endfile_iomsg_01.txt", status="replace", iostat=ios)
    if (ios /= 0) error stop
    if (len_trim(msg) /= 0) error stop

    write(u, *) 1

    endfile(unit=u, iostat=ios, iomsg=msg)
    if (ios /= 0) error stop
    if (len_trim(msg) /= 0) error stop

    close(unit=u, status="delete")
end program