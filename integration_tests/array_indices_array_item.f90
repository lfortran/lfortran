program array_indices_array_item

    implicit none

    integer :: arr_idx(2)
    integer, allocatable :: arr_idx2(:)
    integer, allocatable ::  arr(:, :)
    real ::  arr_1(2, 2), arr_2(2), arr_3(2, 2, 2), arr_4(2, 2)
    real :: arr_2_reshape(2), arr_4_reshape(2, 2)
    integer :: rank_val

    ! Initialize the matrices and arrays
    arr_1 = reshape([1.0, 2.0, 3.0, 4.0], shape(arr_1))
    arr_3 = reshape([1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0, 4.0], shape(arr_3))
    arr_idx = [2, 1]
    allocate(arr_idx2(2))
    arr_idx2 = [2, 1]

     !Slicing
    arr_2 = arr_1(arr_idx, 1)
    arr_2_reshape = reshape([2.0, 1.0], shape(arr_2_reshape));
    print *, rank(arr_2)
    if (rank(arr_2) /= 1) error stop
    if (any(arr_2 /= arr_2_reshape)) error stop

    arr_4 = arr_3(1, arr_idx, arr_idx)
    arr_4_reshape = reshape([3.0, 1.0, 3.0, 1.0], shape(arr_4_reshape));
    if (rank(arr_4) /= 2) error stop
    if (any(arr_4 /= arr_4_reshape)) error stop

    rank_val = rank(arr_3(1, arr_idx, 2))
    print *, rank_val
    if (rank_val /= 1) error stop

    rank_val = rank(arr_3(1, [2, 1], 2))
    print *, rank_val
    if (rank_val /= 1) error stop

    arr_2 = arr_1(arr_idx2, 1)
    arr_2_reshape = reshape([2.0, 1.0], shape(arr_2_reshape));
    print *, rank(arr_2)
    if (rank(arr_2) /= 1) error stop
    if (any(arr_2 /= arr_2_reshape)) error stop

    arr_4 = arr_3(1, arr_idx2, arr_idx2)
    arr_4_reshape = reshape([3.0, 1.0, 3.0, 1.0], shape(arr_4_reshape));
    print *, rank(arr_4)
    if (rank(arr_4) /= 2) error stop
    if (any(arr_4 /= arr_4_reshape)) error stop

    rank_val = rank(arr_3(1, arr_idx2, 2))
    print *, rank_val
    if (rank_val /= 1) error stop

    allocate(arr(1, 3))
    arr = reshape([1, 2, 3], shape(arr))
    rank_val = rank(arr(arr_idx, 1))
    print *, rank_val
    if (rank_val /= 1) error stop
    if (any(arr(arr_idx, 1) /= [2, 1])) error stop

end program array_indices_array_item
