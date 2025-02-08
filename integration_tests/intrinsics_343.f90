program intrinsics_343
    integer, allocatable :: b(:)
    allocate(b(3))

    b = reshape([1, 2, 3], shape=[3])
    print *, cshift(b, 2)
    b = cshift(b, 2)
    print *, b

    if( any(b /= [3, 1, 2] ) ) error stop
end program
