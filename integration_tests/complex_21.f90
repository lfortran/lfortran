program complex_21
    implicit none
    complex :: a = (1.0, 2.0)
    complex(8) :: b = (1.0_8, 2.0_8)

    if (cmplx(a, kind=8) /= b) error stop
    if (cmplx(b, kind=4) /= a) error stop
end program