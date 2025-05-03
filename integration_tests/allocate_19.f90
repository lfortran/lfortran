program expr2
    integer, allocatable :: a(:), b(:)
    allocate(a(5))
    a = [1, 2, 3, 4, 5]
    ! When target size is greater than source

    allocate(b(10), source = a)
    print *, b
    if ( any( b(1:5) /= [1, 2, 3, 4, 5])) error stop
    deallocate(b)

    ! When target size is less than source
    allocate(b(2), source = a)
    print *, b
    if ( any( b /= [1, 2])) error stop
    deallocate(b)

    ! When target size is same as source
    allocate(b(5), source = a)
    print *, b
    if ( any(b /= [1, 2, 3, 4, 5])) error stop
    deallocate(b)

    ! When target size is not specified
    allocate(b, source = a)
    print *, b
    if ( any(b /= [1, 2, 3, 4, 5])) error stop
    deallocate(b)
end program
