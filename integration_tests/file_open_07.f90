program file_open_07
    integer :: u, ios
    character(len=*), parameter :: filename = "file_open_07_err.txt"

    open(newunit=u, file=filename, status="old", iostat=ios, err=100)
    print *, "opened"
    close(u)
    error stop 1

100 continue
    if (ios == 0) error stop 2
end program
