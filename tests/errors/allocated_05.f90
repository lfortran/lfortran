program allocated_05
    implicit none
    integer, allocatable :: arr(:)

    allocate(arr(10))
    allocate(arr(20))  ! Error: double allocation of array
    print *, "Should not reach here"
end program allocated_05
