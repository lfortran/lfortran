program read_from_file
    implicit none
    integer :: x, ios, unit_no
    character(len=100) :: filename

    filename = './tests/overflow_test.txt'

    ! Open the file for reading
    open(newunit=unit_no, file=filename, status='unknown', iostat=ios)
    if (ios /= 0) then
        print *, "Failed to open file:", trim(filename)
        stop
    end if

    ! Attempt to read integers from the file
    do
        read(unit_no, *, iostat=ios) x
        if (ios /= 0) exit
        print *, "Read integer:", x
    end do

    close(unit_no)
end program read_from_file
