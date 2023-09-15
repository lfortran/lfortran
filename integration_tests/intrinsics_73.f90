program main
    real(8) :: y
    real ::  x
    y = 5.2d0
    x = sngl(y)
    if(abs(x - 5.19999981)>1e-8) error stop
end program