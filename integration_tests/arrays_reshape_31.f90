program arrays_reshape_31
    implicit none
    character(len=10) :: arr1d(6)
    character(len=10) :: arr2d(2, 3)

    arr1d = ["one  ", "two  ", "three", "four ", "five ", "six  "]
    arr2d = reshape(arr1d, [2, 3])

    if (trim(arr2d(1, 1)) /= "one") error stop "arr2d(1,1) wrong"
    if (trim(arr2d(2, 1)) /= "two") error stop "arr2d(2,1) wrong"
    if (trim(arr2d(1, 2)) /= "three") error stop "arr2d(1,2) wrong"

    print *, "PASS"
end program
