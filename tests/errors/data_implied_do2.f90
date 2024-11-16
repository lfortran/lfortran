program data_implied_do2
    implicit none
    integer :: k = 1
    integer :: i, iarx(3)
    data(iarx(i), i=1, 3, k) / 1, 2, 3 /
end program data_implied_do2

