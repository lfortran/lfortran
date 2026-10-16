program array_bounds_check_19
    implicit none
    real, allocatable :: a(:, :)
    integer :: k
    allocate(a(4, 3))
    k = 0
    ! The section a(0:4, 2) starts outside a
    a(k:, 2) = 1
    print *, a(1, 1)
end program
