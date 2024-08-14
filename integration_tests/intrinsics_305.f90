program intrinsics_305
    implicit none
    real, parameter :: x1 = realpart((1.0, 2.0))
    real(8), parameter :: x2 = realpart((1.0_8, 2.0_8))
    real, parameter :: ar1(3) = realpart([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)])
    real(8), parameter :: ar2(3) = realpart([(1.0_8, 2.0_8), (3.0_8, 4.0_8), (5.0_8, 6.0_8)])
    complex :: x = (1.0, 2.0)
    complex(8) :: y = (1.0_8, 2.0_8)
    complex :: z(3) = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]

    print *, x1
    if (x1 /= 1.0) error stop
    print *, x2
    if (x2 /= 1.0_8) error stop
    print *, realpart(x)
    if (realpart(x) /= 1.0) error stop
    print *, realpart(y)
    if (realpart(y) /= 1.0_8) error stop
    print *, ar1
    if (any(ar1 /= [1.0, 3.0, 5.0])) error stop
    print *, ar2
    if (any(ar2 /= [1.0_8, 3.0_8, 5.0_8])) error stop
    print *, realpart(x)
    if (realpart(x) /= 1.0) error stop
    print *, realpart(y)
    if (realpart(y) /= 1.0_8) error stop
    print *, realpart(z)
    if (any(realpart(z) /= [1.0, 3.0, 5.0])) error stop
end program