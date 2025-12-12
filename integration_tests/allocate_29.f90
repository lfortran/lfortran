program allocate_29
    implicit none
    integer, allocatable :: x
    integer, allocatable :: arr(:)

    allocate(x)
    allocate(x)
    print *, "Should not reach here - scalar"

    allocate(arr(10))
    allocate(arr(20))
    print *, "Should not reach here - array"
end program allocate_29
