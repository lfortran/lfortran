program realloc_lhs_disabled_01
    ! With --no-realloc-lhs-arrays an allocated array is not reallocated to
    ! the shape of the value assigned to it, and the runtime error must say
    ! how to get the reallocation back.
    integer, allocatable :: x(:)
    allocate(x(2))
    print *, size(x)
    x = [1, 2, 3]
    print *, size(x)
end program realloc_lhs_disabled_01
