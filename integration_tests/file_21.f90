program file_21
    implicit none
    integer :: j
    character(len=1) :: c(2)
    j = 11
    open(j, file="file_21_data.txt")
    read(j, *) c
    close(j)

    print *, c(1)
    if (c(1) /= "a") error stop
    print *, c(2)
    if (c(2) /= "c") error stop

end program file_21