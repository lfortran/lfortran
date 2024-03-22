!> the below test cases test the broadcasting of "min" intrinsic
!> procedure during compile time
program intrinsics_194
    integer :: arr3(3), arr2(2)
    arr3 = min([-1, 2, 3], 2, 5, [4, 4, 5], [5, -8, 7])
    if (arr3(1) /= -1) error stop
    if (arr3(2) /= -8) error stop
    if (arr3(3) /= 2) error stop
    ! not necessary to test the dimensions though
    ! as the initial assignment itself would raise an
    ! error if the assignment was incompatible
    if (size(arr3) /= 3) error stop

    arr3 = min(1, [-1, 2, 20])
    if (arr3(1) /= -1) error stop
    if (arr3(2) /= 1) error stop
    if (arr3(3) /= 1) error stop

    arr3 = min([1, 2, 3], [1, 1, [2]])
    if (arr3(1) /= 1) error stop
    if (arr3(2) /= 1) error stop
    if (arr3(3) /= 2) error stop

    arr2 = min([1, 2], -1, -4)
    if (arr2(1) /= -4) error stop
    if (arr2(2) /= -4) error stop
end program
