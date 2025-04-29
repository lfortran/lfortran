program file_28
    implicit none
    integer :: unit_num, unit_num2
    integer :: stat, a, b, c
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
    close(unit_num, status='delete')
    open(newunit=unit_num2, file='file_28.txt', status='replace', access='stream', form='unformatted')
    write(unit_num2) 1, 2, 3
    rewind(unit_num2)
    read(unit_num2) a, b, c
    close(unit_num2, status='delete')
    if (a /= 1 .or. b /= 2 .or. c /= 3) error stop
end program file_28