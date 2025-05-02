program incorrect_allocate_for_array
    integer, allocatable :: a(:)
    allocate(a)
    print *, size(a)
end program
