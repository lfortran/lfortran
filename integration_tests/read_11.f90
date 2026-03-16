program read_11
    implicit none
    integer :: i, j, n, unit_no
    real :: a(3, 4)

    n = 4
    open(newunit=unit_no, status='scratch')
    write(unit_no, *) 1.0, 2.0, 3.0, 4.0
    write(unit_no, *) 5.0, 6.0, 7.0, 8.0
    rewind(unit_no)

    read(unit_no, *) (a(1,j), j = 1, n)
    read(unit_no, *) (a(2,j), j = 1, n)
    close(unit_no)

    if (abs(a(1,1) - 1.0) > 1e-6) error stop "a(1,1) should be 1.0"
    if (abs(a(1,2) - 2.0) > 1e-6) error stop "a(1,2) should be 2.0"
    if (abs(a(1,3) - 3.0) > 1e-6) error stop "a(1,3) should be 3.0"
    if (abs(a(1,4) - 4.0) > 1e-6) error stop "a(1,4) should be 4.0"
    if (abs(a(2,1) - 5.0) > 1e-6) error stop "a(2,1) should be 5.0"
    if (abs(a(2,2) - 6.0) > 1e-6) error stop "a(2,2) should be 6.0"
    if (abs(a(2,3) - 7.0) > 1e-6) error stop "a(2,3) should be 7.0"
    if (abs(a(2,4) - 8.0) > 1e-6) error stop "a(2,4) should be 8.0"
    print *, "PASS"
end program
