program fileops
    implicit none
    integer :: j, i
    j=11
    open(j, file="../file_01_data.txt")
    read(j, *) i
    close(j)

end program fileops
