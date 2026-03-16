program realloc_lhs_01
    integer, allocatable :: x(:)
    x = [1, 2, 3]
    x = [4, 5]
    if (size(x) /= 2) error stop
end program realloc_lhs_01
