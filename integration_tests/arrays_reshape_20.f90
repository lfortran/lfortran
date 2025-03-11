program arrays_reshape_20
    integer, dimension(2, 3) :: x1 = reshape([1, 4, 2, 5, 3, 6], [2, 3])
    integer, dimension(2, 3) :: x2 = reshape([1, 2, 3, 4, 0, 0], [2, 3])
    integer, dimension(2, 3) :: x3 =reshape([1, 4, 2, 0, 3, 0], [2, 3])

    print *, x1
    print *, x2
    print *, x3

    if (any(x1 /= reshape([1, 2, 3, 4, 5, 6], [2, 3], order = [2, 1]))) error stop
    if (any(x2 /= reshape([1, 2, 3, 4], [2, 3], pad = [0]))) error stop
    if (any(x3 /= reshape([1, 2, 3, 4], [2, 3], order = [2, 1], pad = [0]))) error stop
end program arrays_reshape_20
