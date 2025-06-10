program intrinsics_384
    implicit none
    integer, allocatable :: arr(:)
    allocate(arr(3))
    arr = [1, 2, 3]
    print *, eoshift(arr, 1, 0)
    if (any(eoshift(arr, 1, 0) /= [2, 3, 0])) error stop
end program intrinsics_384