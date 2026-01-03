program intrinsics_106
    integer :: x(2, 3)
    print *, merge(size(x, 1), size(x, 2), mask = 1<0)
    if ( merge(size(x, 1), size(x, 2), mask = 1<0) /= 3 ) error stop
end