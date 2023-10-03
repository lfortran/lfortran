program intrinsics_64
    real :: x, y
    x = 1.0
    y = -0.0

    x = sign(x, y)
    print *, x
    if (abs(x - (-1.0)) > 1e-9) error stop
end program
