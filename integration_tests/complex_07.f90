program complex_07
    implicit none
    integer, parameter :: real_part = 42
    real, parameter :: img_part = 3.14
    complex :: x = cmplx(real_part)
    complex :: y = cmplx(img_part)
    complex :: z = cmplx(real_part, img_part)
    double complex :: c1, x2, y2, z2
    c1 = (12, 24)
    print *, c1
    x2 = dcmplx(real_part)
    y2 = dcmplx(img_part)
    z2 = dcmplx(real_part, img_part)
    if (abs(x - (real_part, 0)) > 1e-5) error stop
    if (abs(y - (img_part, 0)) > 1e-5) error stop
    if (abs(z - (real_part, img_part)) > 1e-5) error stop
    if (abs(x2 - (real_part, 0)) > 1e-5) error stop
    if (abs(y2 - (img_part, 0)) > 1e-5) error stop
    if (abs(z2 - (real_part, img_part)) > 1e-5) error stop

    print *, x, y, z
end program
