program expr2
    integer, allocatable :: a(:), b(:)
    a = [1, 2, 3, 4, 5]
    allocate(b(10), source = a)
    print *, b
    if ( any( b /= [1, 2, 3, 4, 5, 0, 0, 0, 0, 0])) error stop
end program
