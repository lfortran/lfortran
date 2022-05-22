program intrinsics_31
    real :: x = 63.29
    real :: y = -63.59
    real(kind = 8) :: z = 0.000001
    real(kind = 8) :: w = -1.000001
    real :: x_ceil, y_ceil
    integer :: z_ceil, w_ceil
    
    x_ceil = ceiling(x)
    y_ceil = ceiling(y)
    z_ceil = ceiling(z)
    w_ceil = ceiling(w)

    if((abs(x_ceil - 64) > 1e-5) .or. (ceiling(x_ceil) /= 64)) error stop
    if(abs(y_ceil - (-63)) > 1e-5 .or. (ceiling(y_ceil) /= -63)) error stop
    if(z_ceil /= 1) error stop
    if(w_ceil /= -1) error stop

    print *, x_ceil, ceiling(x_ceil)
    print *, y_ceil, ceiling(y_ceil)
    print *, z_ceil
    print *, w_ceil
end program intrinsics_31
