program data_stmt_parameter_02
    implicit none
    integer, parameter :: k(3) = [1, 2, 3]
    integer :: i
    data (k(i), i=1,3) / 4, 5, 6 /
end program data_stmt_parameter_02
