program implied_do_loops13
    implicit none
    character(:), allocatable :: tmp_line
    character(len=5) :: values(3)
    integer :: i

    tmp_line = "Hello"
    values = [(tmp_line, i = 1, 3)]

    do i = 1, 3
        if (values(i) /= "Hello") error stop 1
    end do
end program implied_do_loops13
