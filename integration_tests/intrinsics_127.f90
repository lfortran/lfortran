program intrinsics_127
    implicit none
    integer, parameter :: x1 = ifix(4.23)
    integer, parameter :: y1 = ifix(123.41)
    integer, parameter :: ar1(3) = ifix([1.0, 2.11, -31.0])
    real :: arr1(3) = [1.0, 2.11, -31.0]
    real :: x
    x = 4.23
    
    print *, x1
    if (x1 /= 4) error stop
    print *, y1
    if (y1 /= 123) error stop
    print *, ar1
    if (any(ar1 /= [1, 2, -31])) error stop
    print *, ifix(arr1)
    if (any(ifix(arr1) /= [1, 2, -31])) error stop

    print *, ifix(x)
    if( .not. ifix(x) == 4 ) error stop

    print *, ifix(4.23)
    if (ifix(123.41) /= 123 ) error stop

end program
