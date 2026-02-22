module arrays_reshape_32_mod
    implicit none

    integer(4), dimension(1,1), parameter :: &
        arr = reshape([1, 1], shape(arr), order=[2,1])
    integer(4), dimension(2,2), parameter :: &
        arr2 = reshape([10, 20, 30, 40], shape(arr2))
    integer(4), dimension(2,3), parameter :: &
        arr3 = reshape([1, 2, 3, 4, 5, 6], shape(arr3), order=[2,1])

end module arrays_reshape_32_mod

program arrays_reshape_32
    use arrays_reshape_32_mod
    implicit none
    if (arr(1,1) /= 1) error stop

    if (arr2(1,1) /= 10) error stop
    if (arr2(2,1) /= 20) error stop
    if (arr2(1,2) /= 30) error stop
    if (arr2(2,2) /= 40) error stop

    if (arr3(1,1) /= 1) error stop
    if (arr3(1,2) /= 2) error stop
    if (arr3(1,3) /= 3) error stop
    if (arr3(2,1) /= 4) error stop
    if (arr3(2,2) /= 5) error stop
    if (arr3(2,3) /= 6) error stop
end program arrays_reshape_32
