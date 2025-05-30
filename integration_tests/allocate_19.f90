program allocate_19

    integer, allocatable :: a(:), b(:), c(:), d(:)

    a = [1, 2, 3, 4, 5, 6]
    allocate(b(6), source = a)
    print *, b
    print *, size(b)
    if( any( b /= [1, 2, 3, 4, 5, 6] ) ) error stop
    if( size(b) /= 6 ) error stop

    c = [6, 5, 4]
    allocate(d(3), source = c)
    print *, d
    print *, size(d)
    if( any( d /= [6, 5, 4] ) ) error stop
    if( size(d) /= 3 ) error stop

end program
