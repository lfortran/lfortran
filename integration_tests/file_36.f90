program file_36
    implicit none
    integer :: unit_num
    character(len=100) :: result

    open(file="test.txt", newunit=unit_num)
    write(unit_num,'(A)') "hello world"

    rewind(unit_num)

    read(unit_num,'(A)') result
    close(unit_num)

    print *, result

    if (result /= "hello world") error stop
end program file_36
