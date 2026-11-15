program realloc_lhs_disabled_02
    ! With --disable-realloc-lhs-arrays, the older spelling of
    ! --no-realloc-lhs-arrays, an unallocated array is not allocated by an
    ! assignment, and the runtime error must say how to get that back.
    integer, allocatable :: x(:)
    x = [1, 2, 3]
    print *, size(x)
end program realloc_lhs_disabled_02
