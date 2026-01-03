program intrinsics_219
    integer :: x = sum([1, 2, 3])
    integer, parameter :: y = sum([1, 2, 3])
    if (x /= 6) error stop
    if (y /= 6) error stop
end