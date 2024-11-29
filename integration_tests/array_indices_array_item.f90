program array_indices_array_item

    implicit none

    integer :: arr_idx(2)
    real ::  arr_1(2, 2), arr_2(2, 1), arr_3(2, 2, 2), arr_4(1, 2, 2)
    real :: arr_2_reshape(2, 2), arr_4_reshape(2, 2, 2)

    ! Initialize the matrices and arrays
    arr_1 = reshape([1.0, 2.0, 3.0, 4.0], shape(arr_1))
    arr_3 = reshape([1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0, 4.0, 5.0], shape(arr_3))
    arr_idx = [2, 1]

    ! Slicing
    arr_2 = arr_1(arr_idx, 1)
    arr_2_reshape = reshape([3.0, 1.0], shape(arr_2_reshape));
    print *, rank(arr_2)
    if (rank(arr_2) /= 1) error stop
    if (all(arr_2 /= arr_2_reshape)) error stop

    arr_4 = arr_3(1, arr_idx, arr_idx)
    arr_4_reshape = reshape([4.0, 2.0, 3.0, 1.0], shape(arr_4_reshape));
    print *, rank(arr_4)
    if (rank(arr_4) /= 2) error stop
    if (all(arr_4 /= arr_4_reshape)) error stop

    print *, rank(arr_3(1, arr_idx, 2))
    if (rank(arr_3(1, arr_idx, 2)) /= 1) error stop

end program array_indices_array_item
