program read_from_file
    implicit none
    integer :: x, unit_no

    open(newunit=unit_no, file='./tests/overflow_test.txt', status='old')

    read(unit_no, *) x
    print *, "Read integer:", x

    read(unit_no, *) x
    print *, "Read integer:", x

    close(unit_no)
end program read_from_file
