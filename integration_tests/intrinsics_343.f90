program intrinsics_343
    integer, allocatable :: b(:)
    integer, dimension(3, 4) :: a
    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], shape=[3, 4])
    allocate(b(3))

    b = reshape([1, 2, 3], shape=[3])
    print *, cshift(b, 2)
    b = cshift(b, 2)
    print *, b

    print *, cshift(a, 2)
    a = cshift(a, 2)
    print *, a

    if( any(b /= [3, 1, 2] ) ) error stop
    if( any(a /= reshape([3, 1, 2, 6, 4, 5, 9, 7, 8, 12 ,10, 11], shape=[3, 4]) )) error stop
end program
