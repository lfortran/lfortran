program main
    implicit none
    complex(8) :: k
    real::re,im
    re=3
    im=4
    k = cmplx(re, im)
    print *, k
    if (real(k) /= 3) error stop
    if (aimag(k) /= 4) error stop
end program
