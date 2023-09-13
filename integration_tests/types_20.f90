program main
    real(8) :: y
    real ::  x
    y = 5.2d0
    x = sngl(y)
    if(x /= 5.19999981) error stop
end program