program infer_walrus_array_01
    implicit none
    x := [1, 2, 3]
    if (size(x) /= 3) error stop
    if (x(1) /= 1) error stop
    if (x(2) /= 2) error stop
    if (x(3) /= 3) error stop
    print *, "PASSED"
end program
