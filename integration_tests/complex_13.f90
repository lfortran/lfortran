program main
    implicit none
    complex(8) :: l
    complex(4) :: z
    l=cmplx(3,-4)
    z=l
    print *,z
    if (real(z) /= 3) error stop
    if (aimag(z) /= -4) error stop
end program
