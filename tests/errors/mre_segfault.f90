program mre_segfault
    implicit none
    integer, allocatable :: arr4(:)

    ! checking source in allocate with reshape
    allocate(arr4(3), source=reshape([1, 2, 3, 4, 5, 6], [2, 3]))
end program
