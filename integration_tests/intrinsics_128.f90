program intrinsics_128
    implicit none
    double precision :: x
    integer, parameter :: x1 = idint(4.23_8)
    integer, parameter :: y1 = idint(123.41_8)
    integer, parameter :: ar1(3) = idint([1.0_8, 2.11_8, -31.0_8])
    real(8) :: arr1(3) = [1.0_8, 2.11_8, -31.0_8]

    print *, x1
    if (x1 /= 4) error stop
    print *, y1
    if (y1 /= 123) error stop
    print *, ar1
    if (any(ar1 /= [1, 2, -31])) error stop
    print *, idint(arr1)
    if (any(idint(arr1) /= [1, 2, -31])) error stop

    x = 4.23

    print *, idint(x)
    if( .not. idint(x) == 4 ) error stop

    print *, idint(4.23D0)
    if( .not. idint(4.23D0) == 4 ) error stop

end program
