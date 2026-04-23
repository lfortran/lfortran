program rewind_01
    implicit none
    integer, parameter :: u = 10
    character(len=*) :: msg

    open(unit=u, file="rewind_01_file.txt", status="replace", access="direct", recl=4)
    msg = "sentinel"
    rewind(unit=u)

end program rewind_01
