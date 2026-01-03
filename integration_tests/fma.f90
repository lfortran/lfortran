program fma

    implicit none
    real :: x = 4.0
    real, parameter :: pi = 3.14
    real(8) :: S1 = 1.0, S2 = 2.0
    real(8) :: z = 1.0

    integer ::b(10)
    real :: r

    
    x = (x * 4 + 0.5_4*sign(1._4, x)) - x * pi
    z = S1+z*S2
    if( abs(x - 3.94) > 1e-6 ) error stop
    if( abs(z - 3.0) > 1e-6 ) error stop



    b = 1
    r = 1
    b(1) = nint(sign(1.0,r +r*r), 4) ! Test FMA opt. with `--fast` (functioncall with fma nested)
    print *, b(1)   
    if(b(1) /= 1) error stop
    b(1) = r -r*r ! Test FMA opt. with `--fast` (sub op)
    print *, b(1)
    if(b(1) /= 0) error stop

end program

