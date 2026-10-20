program where_22
    implicit none
    integer :: arr(3), brr(3), idx(3)
    integer :: a2(2, 2), b2(2, 2), jdx(2)

    arr = [1, 2, 3]
    brr = [1, 2, 3]
    idx = [3, 2, 1]
    where (brr > 1) arr = brr(idx)
    print *, arr
    if (any(arr /= [1, 2, 1])) error stop

    arr = [1, 2, 3]
    where (brr > 1) arr(idx) = brr
    print *, arr
    if (any(arr /= [3, 2, 3])) error stop

    a2 = 0
    b2 = reshape([1, 2, 3, 4], [2, 2])
    jdx = [2, 1]
    where (b2 > 1) a2 = b2(jdx, :)
    print *, a2
    if (any(a2 /= reshape([0, 1, 4, 3], [2, 2]))) error stop
end program
