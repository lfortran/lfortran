program arrays_op_17
    implicit none
    integer, allocatable :: g(:), y(:, :)
    integer :: b(20), i

    allocate(y(20, 20), g(20))
    b = 2
    y = 4
    g = 8
    do i = 1, 20
        y(i, :) = g(:) * y(i, :) + b(:)
    end do

    print *, y
    if( any(y /= 34) ) error stop

end program arrays_op_17
