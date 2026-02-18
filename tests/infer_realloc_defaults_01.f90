program infer_realloc_defaults_01
    integer, allocatable :: x(:)
    x = [1, 2, 3]
    x = [4, 5]
    if (size(x) /= 2) error stop
end program infer_realloc_defaults_01
