program intrinsics_483
    implicit none
    integer(4) :: a4(2)
    integer(8) :: b8(2)
    real(4) :: r4(2)
    real(8) :: r8(2)
    complex(4) :: c4(2)
    complex(8) :: c8(2)
    a4 = [100000, 100000]
    b8 = [100000_8, 100000_8]
    r4 = [1.0, 1.0]
    r8 = [1.0d-10, 1.0d0]
    c4 = [(1.0, 0.0), (1.0, 0.0)]
    c8 = [(1.0d-10, 0.0d0), (1.0d0, 0.0d0)]
    if (kind(dot_product(a4, b8)) /= 8) error stop
    if (kind(dot_product(b8, a4)) /= 8) error stop
    if (dot_product(a4, b8) /= 20000000000_8) error stop
    if (dot_product(b8, a4) /= 20000000000_8) error stop
    if (kind(dot_product(r4, r8)) /= 8) error stop
    if (kind(dot_product(r8, r4)) /= 8) error stop
    if (abs(dot_product(r4, r8) - 1.0000000001d0) > 1.0d-12) error stop
    if (abs(dot_product(r8, r4) - 1.0000000001d0) > 1.0d-12) error stop
    if (kind(dot_product(c4, c8)) /= 8) error stop
    if (kind(dot_product(c8, c4)) /= 8) error stop
    if (abs(dot_product(c4, c8) - (1.0000000001d0, 0.0d0)) > 1.0d-12) error stop
    if (abs(dot_product(c8, c4) - (1.0000000001d0, 0.0d0)) > 1.0d-12) error stop
end program
