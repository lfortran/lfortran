program intrinsics_446
    implicit none
    integer, dimension(3, 2) :: result
    result = spread([1, 4, 5], 2, 2)
    if (result(1, 1) /= 1 .or. result(2, 1) /= 4 .or. result(3, 1) /= 5) error stop
    if (result(1, 2) /= 1 .or. result(2, 2) /= 4 .or. result(3, 2) /= 5) error stop
    print *, result
end program intrinsics_446
