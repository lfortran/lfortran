program arrays_op_16
    implicit none
    integer:: x(10) = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/), i
    integer, allocatable :: y(:)

    allocate(y(10))
    y(:) = x(:)

    print *, x, y
    do i = 1, 10
        if( y(i) /= i ) error stop
    end do
end program arrays_op_16
