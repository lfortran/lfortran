program array_bounds_check_18
    implicit none
    real, allocatable :: a(:)
    allocate(a(0))
    print *, a(1:4)
end program
