program read_10
    implicit none
    integer :: vals(3)
    integer :: i

    open(10, status='scratch')
    write(10, *) 10, 20, 30
    rewind(10)

    read(10, *) (vals(i), i=1, 3)
    close(10)

    if (vals(1) /= 10) error stop "vals(1) should be 10"
    if (vals(2) /= 20) error stop "vals(2) should be 20"
    if (vals(3) /= 30) error stop "vals(3) should be 30"
    print *, "PASS"
end program
