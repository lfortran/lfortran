program sign_intrinsic
    integer :: x, signval
    real :: y
    x = 5
    y = -10.0
    signval = sign(x, y)
    print *, signval
end program 
