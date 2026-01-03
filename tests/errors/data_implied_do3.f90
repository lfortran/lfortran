program data_implied_do3
    implicit none
    integer :: k = 3
    integer :: i, iarx(3)
    data(iarx(i), i=k, 3) / 1, 2, 3 /
end program data_implied_do3

