program fileops
    implicit none
    integer :: j, i
    j=11
    open(j, file="../file_01.f90")
    read(j, *) i
    close(j)

end program fileops
