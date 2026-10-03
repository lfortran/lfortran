program infer_walrus_array_03
    implicit none
    x = [10, 20, 30]
    if (size(x) /= 3) error stop
    if (x(1) /= 10) error stop
    if (x(3) /= 30) error stop
    y = [1.0d0, 2.0d0, 3.0d0]
    if (size(y) /= 3) error stop
    if (abs(y(2) - 2.0d0) > 1.0d-10) error stop
    print *, "PASSED"
end program
