program file_22
    integer :: filesize
    inquire(file="file_22_data.txt", size=filesize)
    print *, filesize
    if (filesize /= 13) error stop
 end program file_22