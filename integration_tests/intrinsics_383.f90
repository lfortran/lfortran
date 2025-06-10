program intrinsics_383
    implicit none
    integer, allocatable :: arr1(:)
    integer, allocatable :: arr2(:, :)

    allocate(arr1(3))
    arr1 = [1, 2, 3]
    print *, pack(arr1, .true.)
    if (any(pack(arr1, .true.) /= [1, 2, 3])) error stop

    allocate(arr2(2, 3))
    arr2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    print *, pack(arr2, .true.)
    if (any(pack(arr2, .true.) /= [1, 2, 3, 4, 5, 6])) error stop
end program