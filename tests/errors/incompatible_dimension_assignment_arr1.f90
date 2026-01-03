program incompatible_ranks_allocatable_arr2
    implicit none
    ! constant array of size 1
    integer :: arr1(1)
    ! arr1 (which is of size 1) is being assigned
    ! an array of size 3
    arr1 = [1, 2, 3]
end program
