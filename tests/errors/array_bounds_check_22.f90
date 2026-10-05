program array_bounds_check_22
    implicit none
    real :: b(2, 3)
    integer :: n
    n = 3
    b = 1.0
    print *, b(n, 1:2)
end program
