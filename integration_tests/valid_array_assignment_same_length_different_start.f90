program valid_array_assignment_same_length_different_start
    ! array with size 3 and start index 2
    integer :: arr1(2:4)
    ! array with size 3 and start index 1
    integer :: arr2(1:3)
    arr1 = [1, 2, 3]
    ! valid assignment as only "size" matters for array
    ! assignment, not "start index"
    arr2 = arr1

    if (size(arr2) /= 3) error stop
    if (arr2(1) /= 1) error stop
end program
