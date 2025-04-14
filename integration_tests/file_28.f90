program file_28
    implicit none
    integer :: unit_num
    integer :: stat
    integer, dimension(5) :: data_out = [10, 20, 30, 40, 50]
    integer, dimension(5) :: data_in
    ! Choose a unit number
    unit_num = 20
    ! Write data to file
    open(unit=unit_num, file='file_28.txt', status='replace')
    write(unit_num, *) data_out
    rewind(unit_num)
    open(unit=unit_num)
    read(unit_num, *) data_in
    close(unit_num)
    if(any(data_in /= data_out)) error stop
end program file_28