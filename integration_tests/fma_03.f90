program fma_03

    implicit none
    integer ::b(10)
    integer ::bb(10)
    real :: r
    bb = 0
    bb(2) =  100
    b = 1
    r = 2   
    b(1) = bb(int(r*r - r,4)) ! Test fma in nested expression
    print *, b(1)
    if(b(1) /= 100) error stop

end program fma_03