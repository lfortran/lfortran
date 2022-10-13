program complex_09
    integer :: i = 42
    real :: x = 3.14
    complex :: z1, z2
    z1 = cmplx(i, x)
    print *, z1, cmplx(x, kind=8)
    z2 = cmplx(z2, kind=8)
end program complex_09