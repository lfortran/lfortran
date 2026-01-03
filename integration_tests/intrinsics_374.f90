program intrinsics_370
    implicit none

    integer :: input(6, 9)
    character(len=2) :: str_arr(2, 2)
    integer, dimension(2) :: result

    input = reshape([&
        1,  2,  3,  4,  5,  7,  8,  9, 10, &
        11, 12, 13, 14, 7,  16, 17, 18, 19, &
        21, 22, 7,  24, 25, 26, 27, 28, 29, &
        31, 32, 33, 34, 35, 36, 37, 38, 39, &
        41, 42, 43, 44, 45, 46, 7,  48, 49, &
        51, 52, 53, 54, 55, 56, 57, 58, 7], [6, 9]) 

    str_arr = reshape(["aa", "bb", "cc", "aa"], [2, 2])

    result = findloc(input, 7)
    print *, result
    if (any(result /= [6, 1])) error stop

    result = findloc(input, 34)
    print *, result
    if (any(result /= [1, 6])) error stop

    result = findloc(input, 7, back=.true.)
    print *, result
    if (any(result /= [6, 9])) error stop

    result = findloc(str_arr, "cc")
    print *, result
    if (any(result /= [1, 2])) error stop

end program
