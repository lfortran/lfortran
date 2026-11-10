program arrays_138
    ! An array with huge(0) elements: size() must not overflow (#6508)
    implicit none
    integer, dimension(2147483647) :: arr1
    print *, size(arr1)
    if (size(arr1) /= 2147483647) error stop
    if (size(arr1, kind=8) /= 2147483647_8) error stop
    if (ubound(arr1, 1) /= huge(0)) error stop
end program arrays_138
