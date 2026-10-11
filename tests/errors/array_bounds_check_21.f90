program array_bounds_check_21
    implicit none
    real :: a(3)
    integer :: n
    n = 5
    a = 1.0
    ! the last element selected by 2:n:2 is a(4)
    print *, a(2:n:2)
end program
