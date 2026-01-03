program expr2
    implicit none
    integer :: ios
    character(len=50) :: data
    character(len=50) :: temp

    temp = "apple"
    open(1, form="unformatted", file="file_20_tmp.dat")
    write(1, iostat=ios) temp
    close(1)
    print *, ios
    if (ios /= 0) error stop

    open(2, form="unformatted", file="file_20_tmp.dat")
    read(2) data
    close(2)
    print *, data
    if (trim(data) /= "apple") error stop
end program
