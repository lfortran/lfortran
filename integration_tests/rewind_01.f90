program rewind_01
    implicit none
    integer :: ios
    integer, parameter :: u = 10
    character(len=128) :: msg

    open(unit=u, file="rewind_01_file.txt", status="replace", access="direct", recl=4, iostat=ios)
    if (ios /= 0) error stop

    msg = "sentinel"
    rewind(unit=u, iostat=ios, iomsg=msg)

    if (ios == 0) error stop
#if defined(__LFORTRAN__)
    if (len_trim(msg) == 0) error stop
    if (trim(msg) /= "REWIND cannot be used on UNIT 10 opened with DIRECT access.") error stop
#endif
    close(unit=u, status="delete")


end program rewind_01
