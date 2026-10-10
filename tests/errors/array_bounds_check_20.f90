program array_bounds_check_20
    implicit none
    real, allocatable :: a(:)
    allocate(a(3))
    a = 1.0
    print *, a(1:4)
end program
