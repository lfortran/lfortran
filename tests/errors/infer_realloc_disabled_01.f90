program infer_realloc_disabled_01
    integer, allocatable :: x(:)
    x = [1, 2, 3]
    x = [4, 5]
    print *, size(x)
end program infer_realloc_disabled_01
