program intrinsics_370
    implicit none

    ! Test cases for 2D arrays
    integer :: input(6, 9)
    character(len=2) :: str_arr(2, 2)
    integer, dimension(2) :: result

    ! Initialize test arrays - 6x9 array with multiple occurrences of some numbers
    input = reshape([&
        1,  2,  3,  4,  5,  7,  8,  9, 10, &
        11, 12, 13, 14, 7,  16, 17, 18, 19, &
        21, 22, 7,  24, 25, 26, 27, 28, 29, &
        31, 32, 33, 34, 35, 36, 37, 38, 39, &
        41, 42, 43, 44, 45, 46, 7,  48, 49, &
        51, 52, 53, 54, 55, 56, 57, 58, 7], [6, 9])  ! 7 appears at (1,6), (2,5), (3,3), (5,7), (6,9)

    str_arr = reshape(["aa", "bb", "cc", "aa"], [2, 2])

    ! Test 1: Find first occurrence of repeated value (7)
    print *, "Test 1: findloc on 6x9 array, find first 7"
    result = findloc(input, 7)
    print *, result
    if (any(result /= [6, 1])) error stop

    ! Test 2: Find element in middle of array (34)
    print *, "Test 2: findloc on 6x9 array, find 34"
    result = findloc(input, 34)
    print *, result
    if (any(result /= [1, 6])) error stop

    ! Test 3: Find last occurrence of repeated value (7)
    print *, "Test 3: findloc on 6x9 array, find last 7"
    result = findloc(input, 7, back=.true.)
    print *, result
    if (any(result /= [6, 9])) error stop

    ! Test 4: Find string in array
    print *, "Test 4: findloc on string array, find first cc"
    result = findloc(str_arr, "cc")
    print *, result
    if (any(result /= [1, 2])) error stop

    print *, "All tests passed!"

end program
