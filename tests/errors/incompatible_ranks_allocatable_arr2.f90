program incompatible_ranks_allocatable_arr2
    implicit none
    ! allocatable array of rank 3
    integer, allocatable :: arr1(:, :, :)
    ! allocatable array of rank 1
    integer, allocatable :: arr3(:)

    ! RHS is an allocatable array of rank 3
    ! incompatible assignment
    arr3 = arr1
end program
