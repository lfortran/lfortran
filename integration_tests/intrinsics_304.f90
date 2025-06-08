program intrinsics_304
    implicit none
    integer, parameter :: a1 = sign(1, 1)
    integer(8), parameter :: a2 = sign(1, -1)
    integer, parameter :: a3 = sign(-13, 0)
    integer(8), parameter :: a4 = sign(0, 42)
    real, parameter :: a5 = sign(1.425, 3.0)
    real(8), parameter :: a6 = sign(1.425, -3.0)
    real, parameter :: a7 = sign(1.425, 0.0)
    real(8), parameter :: a8 = sign(0.0, -3.0)

    integer, parameter :: ar1(3) = sign([1, 2, 3], [1, 2, 3])
    integer(8), parameter :: ar2(3) = sign([1, -2, 3], -3)
    real, parameter :: ar3(3) = sign([1.425, -2.425, 3.425], 3.0)
    real(8), parameter :: ar4(3) = sign([1.425, -2.425, 3.425], 3.0)
    
    integer :: x1 = 123, x2 = -90
    integer(8) :: x3 = 123, x4 = 423
    real :: x5 = 123.0, x6 = -47981
    real(8) :: x7 = 123.0, x8 = 123.0

    integer :: arr1(3) = [-311, -241, 331], arr2(3) = [1, 2, 3]
    real :: arr3(3) = [1.425, -2.425, 3.425], arr4(3) = [1.425, -2.425, 3.425]

    print *, a1
    if (a1 /= 1) error stop
    print *, a2
    if (a2 /= -1) error stop
    print *, a3
    if (a3 /= 13) error stop
    print *, a4
    if (a4 /= 0) error stop
    print *, a5
    if (abs(a5 - 1.425) > 1e-7) error stop
    print *, a6
    if (abs(a6 - (-1.425)) > 1e-7) error stop
    print *, a7
    if (abs(a7 - 1.425) > 1e-7) error stop
    print *, a8
    if (abs(a8 - (-0.0)) > 1e-7) error stop

    print *, ar1
    if (any(ar1 /= [1, 2, 3])) error stop
    print *, ar2
    if (any(ar2 /= [-1, -2, -3])) error stop
    print *, ar3
    if (any(abs(ar3 - [1.42499995e+00, 2.42499995e+00, 3.42499995e+00]) > 1e-7)) error stop
    print *, ar4
    if (any(abs(ar4 - [1.42499995231628418e+00, 2.42499995231628418e+00, 3.42499995231628418e+00]) >1e-7)) error stop

    print *, sign(x1, x2)
    if (sign(x1, x2) /= -123) error stop
    print *, sign(x3, x4)
    if (sign(x3, x4) /= 123) error stop
    print *, sign(x5, x6)
    if (abs(sign(x5, x6) - (-123.0)) > 1e-7) error stop
    print *, sign(x7, x8)
    if (abs(sign(x7, x8) - 123.0) > 1e-7) error stop

    print *, sign(arr1, arr2)
    if (any(sign(arr1, arr2) /= [311, 241, 331])) error stop
    print *, sign(arr3, arr4)
    if (any(abs(sign(arr3, arr4) - [1.425, -2.425, 3.425]) > 1e-7)) error stop
    
    print *, sign(a=-12.,b=0.)
    print *, sign(a=-12.,b=1.)
    print *, sign(a=-12.,b=-1.)
  
    if ( .not. sign( a = -12, b = 1 ) == 12 ) error stop
    if ( .not. sign( -12, b = 0 ) == 12 ) error stop
    if ( .not. sign( -12, -1 ) == -12 ) error stop
end program
