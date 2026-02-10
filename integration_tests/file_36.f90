program file_36
    implicit none
    integer :: unit_num, iostat
    character(len=100) :: result

    open(file="file_36_test.txt", newunit=unit_num, delim="quote",iostat=iostat)
    write(unit_num,*) "hello world"

    rewind(unit_num)

    read(unit_num,*) result
    close(unit_num)

    print *, result
    print *, iostat

    if (result /= "hello world") error stop
    if(iostat/=0) error stop 
end program file_36
