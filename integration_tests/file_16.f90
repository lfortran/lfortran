program file_16
    implicit none
    integer :: j = 11
    character(len=10) :: str, x
    str = "LFortran"
    open(j, file="test.txt")
    write(j, *) str
    str = ""
    rewind(j)
    read(j, *) str
    if (str /= "LFortran") error stop
    close(j)
end program file_16
