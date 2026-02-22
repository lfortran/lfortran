program infer_walrus_array_02
    implicit none
    x := reshape([1, 2, 3, 4, 5, 6], [2, 3])
    if (size(x, 1) /= 2) error stop
    if (size(x, 2) /= 3) error stop
    if (x(1, 1) /= 1) error stop
    if (x(2, 1) /= 2) error stop
    if (x(1, 2) /= 3) error stop
    print *, "PASSED"
end program
