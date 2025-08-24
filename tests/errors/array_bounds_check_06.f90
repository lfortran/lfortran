program array_bounds_check_06
    implicit none

    integer :: x(3)
    integer, allocatable :: y(:)
    allocate(y(4)) ! works with 3, must fail with 2 or 4

    x = 2
    y = 3
    x = y

end program
