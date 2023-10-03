program data_10
    integer :: i, j, x(5), y(5)
    real :: k, l
    data x, j / 1, 2, 3, 4, 5, 3 /
    print *, x, j
    if (x(1) /= 1) error stop
    if (x(2) /= 2) error stop
    if (x(3) /= 3) error stop
    if (x(4) /= 4) error stop
    if (x(5) /= 5) error stop
    if (j /= 3) error stop
    data k, (y(i), i = 1, 5), l / 10.0, 1, 2, 3, 4, 5, 12.0 /
    print *, k, l, y
    if (y(1) /= 1) error stop
    if (y(2) /= 2) error stop
    if (y(3) /= 3) error stop
    if (y(4) /= 4) error stop
    if (y(5) /= 5) error stop
    if (abs(k - 10.0) > 1e-8) error stop
end program
