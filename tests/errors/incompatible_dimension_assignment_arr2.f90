program incompatible_ranks_allocatable_arr2
    implicit none
    ! constant array of rank 1 size 3
    integer :: arr1(3)
    ! an allocatable array of rank 1
    integer, allocatable :: arr2(:)
    ! size of allocatable array is 4
    allocate(arr2(4))
    arr2 = (/1, 2, 3, 4/)

    ! incompatible assignment of size 3 to an array of size 4
    arr1 = arr2
end program
