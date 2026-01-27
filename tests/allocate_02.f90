program allocate_02
    implicit none
    integer, allocatable :: arr(:)
    integer :: i
    allocate(arr(1))
    do i = 1, 1000000
        deallocate(arr)
        allocate(arr(1))
    end do
end program
