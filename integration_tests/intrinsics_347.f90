program intrinsics_347
    integer :: x(2, 2), y(4, 2, 2)
    x = reshape([1, 2, 3, 4], [2, 2])
    y = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16], [4, 2, 2])
    print *, reshape([1, 2, 3, 4],[2, 2])
    if (any(x /= reshape([1, 2, 3, 4], [2, 2]))) error stop
    print *, reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16], [4, 2, 2])
    if (any(y /= reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16], [4, 2, 2]))) error stop
end program