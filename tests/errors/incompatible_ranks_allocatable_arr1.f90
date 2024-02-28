program incompatible_ranks_allocatable_arr1
    implicit none
    ! allocatable array of rank 2
    integer, allocatable :: arr1(:, :)

    ! RHS is a constant array of rank 1
    ! incompatible assignment
    arr1 = [1, 2, 3]
end program
