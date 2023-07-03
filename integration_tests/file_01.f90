program fileops
    implicit none
    integer :: j, i
    character(len=1) :: c
    real :: z
    j=11
    open(j, file="../file_01_data.txt")
    read(j, *) i
    read(j, *) c
    read(j, *) z
    rewind(j)
    close(j)

end program fileops
