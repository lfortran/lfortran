program array_bounds_check_18
    implicit none
    real :: a(4, 3)
    integer :: i
    i = 4
    ! The section a(:, 4) lies outside a
    a(:, i) = 1
    print *, a(1, 1)
end program
