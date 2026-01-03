program intrinsics_138
    implicit none

    real :: x = 3.143
    complex(4) :: y = (2.33, 4.1)
    real(kind=4) :: x1(2)
    complex(kind=8) :: y1

    integer(4), parameter :: r1 = precision(1._4)
    integer(8), parameter :: r2 = precision(67._8)
    integer, parameter :: r3 = precision((1, 2))
    integer(4), parameter :: ar1 = precision([11.3_4, 1.7_4, 0.0_4])
    integer(8), parameter :: ar2 = precision([11.3_8, 1.7_8, 0.0_8])
    integer, parameter :: ar3 = precision([(1, 2), (3, 4), (11, 22)])

    real(4) :: arr1(3) = [11.3_4, 1.7_4, 0.0_4]
    complex(8) :: arr2(3) = [(11.3_8, 1.7_8), (0.0_8, 1.1_8), (5.0_8, 0.1_8)]

    print *, r1
    if (r1 /= 6) error stop
    print *, r2
    if (r2 /= 15) error stop

    print *, ar1
    if (ar1 /= 6) error stop
    print *, ar2
    if (ar2 /= 15) error stop

    print *, precision(x)
    if (precision(x) /= 6) error stop
    print *, precision(y)
    if (precision(y) /= 6) error stop
    print *, precision(1._8) ** 0.5
    if (abs(precision(1._8) ** 0.5_8 - 3.87298346) > 1e-6) error stop

    print*, precision(1.0)
    if (precision(1.0) /= 6) error stop
    print *, precision(x1)
    if (precision(x1) /= 6) error stop
    print *, precision(1.0d0)
    if (precision(1.0d0) /= 15) error stop
    print *, precision(y1)
    if (precision(y1) /= 15) error stop

    print *, precision(arr1)
    if (precision(arr1) /= 6) error stop
    print *, precision(arr2)
    if (precision(arr2) /= 15) error stop

end program