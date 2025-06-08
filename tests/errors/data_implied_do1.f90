program data_implied_do1
    implicit none
    integer :: k = 3
    integer :: i, iarx(3)
    data(iarx(i), i=1, k) / 1, 2, 3 /
end program data_implied_do1

