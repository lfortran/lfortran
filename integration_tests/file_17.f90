program file_17
    implicit none
    integer :: j = 11
    character(len=33) :: str(2)
    character(8) :: s
    str(1) = "LFortran can finally run dftatom!"
    str(2) = "This is the last bug to be fixed!"
    open(j, file="test_file_17.txt")
    write(j, *) str
    str = ""
    rewind(j)
    read(j, *) s
    if (s /= "LFortran") error stop
    close(j)
end program file_17
