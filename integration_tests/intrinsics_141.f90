program intrinsic_141
    implicit none
    real :: x = 178.1387e-4
    integer :: i = 5
    real :: y = -186.3245e-4
    integer :: j = 3
    real, parameter :: x1 = scale(11.0, 2)
    real(8), parameter :: y1 = scale(-11.0_8, 2)
    real(8), parameter :: z1 = scale(11.0, 2_8)
    real, parameter ::ar1(3) = scale([1.0, 21.0, 13.0], 2)
    real(8), parameter :: ar2(3) = scale([1.0_8, 2.0_8, 3.0_8], 2)
    real(8), parameter :: ar3(3) = scale([1.0, 2.0, 3.0], 2_8)
    real :: arr1(3) = [1.0, 21.0, 13.0]
    real(8) :: arr2(3) = [1.0_8, 2.0_8, 3.0_8]
    integer :: arr3(3) = [1, 2, 3]

    print *, scale(arr1, arr3)
    if (any(abs(scale(arr1, arr3) - [2.00000000e+00, 84.0000000, 104.000000]) > 1e-7)) error stop

    print *, scale(arr2, 2)
    if (any(abs(scale(arr2, 2) - [4.00000000e+00, 8.00000000, 12.0000000]) > 1e-7)) error stop
    print *, x1
    if (abs(x1 - 4.40000000e+01) > 1e-7) error stop
    print *, y1
    if (abs(y1 - (-4.40000000000000000e+01)) > 1e-7) error stop
    print *, z1
    if (abs(z1 - 4.40000000000000000e+01) > 1e-7) error stop
    print *, ar1
    if (any(abs(ar1 - [4.00000000e+00, 8.40000000e+01, 5.20000000e+01]) > 1e-7)) error stop
    print *, ar2
    if (any(abs(ar2 - [4.00000000000000000e+00, 8.00000000000000000e+00, 1.20000000000000000e+01]) > 1e-7)) error stop
    print *, ar3
    if (any(abs(ar3 - [4.00000000000000000e+00, 8.00000000000000000e+00, 1.20000000000000000e+01]) > 1e-7)) error stop

    print *, scale(y,j)
    if (abs(scale(y, j) - (-1.49059594e-01)) > 1e-7) error stop

    print *, scale(-186.3245e-4,3)
    if (abs(scale(-186.3245e-4,3) - (-1.49059594e-01)) > 1e-7) error stop

    print *, scale(x,i)
    if (abs(scale(x, i) - 5.70043862e-01) > 1e-7) error stop

    print *, scale(178.1387e-4,5)
    if (abs(scale(178.1387e-4, 5) - 5.70043862e-01) > 1e-7) error stop

  end program 
