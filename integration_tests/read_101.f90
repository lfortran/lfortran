program read_101
    implicit none
    character(len=32) :: line
    real :: value, a(3)
    integer :: i, j, n, ierr, ncol, iv
    character(len=3) :: cv

    line = "1.0 2.0 3.0"
    n = 2
    read(line, *) (value, j=1, n)
    if (abs(value - 2.0) > 1e-6) error stop

    ncol = 0
    do i = 1, 5
        read(line, *, iostat=ierr) (value, j=1, i)
        if (ierr /= 0) then
            ncol = i - 1
            exit
        end if
    end do
    if (ncol /= 3) error stop

    line = "4 5 6"
    n = 3
    read(line, *) (iv, j=1, n)
    if (iv /= 6) error stop

    line = "abc def"
    n = 2
    read(line, *) (cv, j=1, n)
    if (cv /= "def") error stop

    line = "1 10 2 20 3 30"
    n = 3
    read(line, *) (value, a(j), j=1, n)
    if (abs(value - 3.0) > 1e-6) error stop
    if (any(abs(a - [10.0, 20.0, 30.0]) > 1e-6)) error stop
end program
