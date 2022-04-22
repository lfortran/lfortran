program complex_07
    implicit none
    integer, parameter :: real_part = 42
    real, parameter :: img_part = 3.14
    complex :: x = cmplx(real_part)
    complex :: y = cmplx(img_part)
    complex :: z = cmplx(real_part, img_part)

    if (abs(x - (real_part, 0)) > 1e-5) error stop
    if (abs(y - (img_part, 0)) > 1e-5) error stop
    if (abs(z - (real_part, img_part)) > 1e-5) error stop

    print *, x, y, z
end program
