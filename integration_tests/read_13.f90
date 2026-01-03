program read_13
    implicit none
    real :: arr(5)
    integer :: i, j

    open(10, file="read_13_data.txt", status="replace")
    write(10, *) 1.0, 2.0, 3.0, 4.0, 5.0
    close(10)

    open(10, file="read_13_data.txt", status="old")
    read(10, *) (arr(j), j=1,5)
    close(10, status="delete")

    do i = 1, 5
        if (abs(arr(i) - real(i)) > 1e-6) error stop
    end do
    print *, "PASS"
end program
