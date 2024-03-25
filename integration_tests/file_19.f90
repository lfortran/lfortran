program file_19
    implicit none

    character(len=25) :: buffer
    integer :: chunk, iostat

    open(11, file="file_19_data.txt")
    read(11, '(a)', iostat=iostat, size=chunk, advance='no') buffer

    print *, buffer
    if (buffer /= " Hi this is 1st line!") error stop

    print *, chunk
    if (chunk /= 21) error stop

end program
