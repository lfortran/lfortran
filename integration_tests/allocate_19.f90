program allocate_19

    integer, allocatable :: a(:), b(:)

    a = [1, 2, 3, 4, 5, 6]
    allocate(b(10), source = a)
    print *, b
    print *, size(b)
    if( any( b(1:6) /= [1, 2, 3, 4, 5, 6] ) ) error stop
    if( size(b) /= 10 ) error stop

    deallocate(b)

    a = [6, 5, 4]
    allocate(b(3), source = a)
    print *, b
    print *, size(b)
    if( any( b /= [6, 5, 4] ) ) error stop
    if( size(b) /= 3 ) error stop

end program
