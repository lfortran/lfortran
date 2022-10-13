program intrinsics_42
    implicit none
    real :: x = 3.0, y = 4.0, z = 5.0
    real :: a = -6, b = 8, c = 10
    real :: p = 1.2, q = 0.5, r = 1.3

    if (abs(hypot(x, y) - (z * z)) > 1e5) error stop
    if (abs(hypot(a, b) - (c * c)) > 1e5) error stop
    if (abs(hypot(p, q) - (r * r)) > 1e5) error stop

    print *, x, y, z
    print *, a, b, c
    print *, p, q, r
end program
