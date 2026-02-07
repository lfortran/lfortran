! Test for https://github.com/lfortran/lfortran/issues/6811
! Reading from character variable into integer array
! Exact MRE from issue body
program read_35
    implicit none
    character(23) :: cinput = '42 666 -42 -666 10 9 0 '
    integer :: input(7)
    read(cinput, *) input
    print "(A,7(1X,I0))", 'input was ', input
    if (input(1) /= 42) error stop
    if (input(2) /= 666) error stop
    if (input(3) /= -42) error stop
    if (input(4) /= -666) error stop
    if (input(5) /= 10) error stop
    if (input(6) /= 9) error stop
    if (input(7) /= 0) error stop
end program read_35
