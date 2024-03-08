program incompatible_dimension_assignment_arr2
    implicit none
    ! integer array of rank 3 and shape 2, 2, 2
    integer :: x(1:2,1:2,1:2)
    ! integer array of rank 3 and shape 2, 2, 1
    integer :: y(1:2,1:2,1:1)
    x = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
    y = reshape([1, 2, 3, 4], [2, 2, 1])
    ! invalid assignment, as shapes are different
    y = x
end program
