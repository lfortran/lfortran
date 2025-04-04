program intrinsics_343
    integer, allocatable :: b(:)
    integer, dimension(3, 4) :: a
    integer, dimension(3, 3, 2) :: c
    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], shape=[3, 4])
    c = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], shape=[3, 3, 2])
    allocate(b(3))

    b = reshape([1, 2, 3], shape=[3])
    print *, cshift(b, 2)
    b = cshift(b, 2)
    print *, b

    print *, cshift(a, 2)
    a = cshift(a, 2)
    print *, a

    print *, cshift(c, 2)
    c = cshift(c, 2)
    print *, c

    if( any(b /= [3, 1, 2] ) ) error stop
    if( any(a /= reshape([3, 1, 2, 6, 4, 5, 9, 7, 8, 12 ,10, 11], shape=[3, 4]) )) error stop
    if ( any(c /= reshape([3, 1, 2, 6, 4, 5, 9, 7, 8, 12 ,10, 11, 15, 13, 14, 18, 16, 17], shape=[3, 3, 2]) ) ) error stop
end program
