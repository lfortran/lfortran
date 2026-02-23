program complex_33
    implicit none
    complex, allocatable :: z(:)
    complex :: w(3)
    real :: re_vals(3), im_vals(3)
    real :: zero = 0.0
    integer :: i
    character(len=9) :: buf

    allocate(z(3))
    z = cmplx(1.0/zero, 1.0/zero)
    do i = 1, 3
        write(buf, "(F9.0)") z(i)%im
        if (adjustl(buf) /= "Infinity") error stop
    end do

    w = (1.0, 2.0)
    re_vals = w%re
    im_vals = w%im
    do i = 1, 3
        if (abs(re_vals(i) - 1.0) > 1e-6) error stop
        if (abs(im_vals(i) - 2.0) > 1e-6) error stop
    end do

    z(1) = cmplx(1.0, 4.0)
    z(2) = cmplx(2.0, 5.0)
    z(3) = cmplx(3.0, 6.0)

    re_vals = z%re
    im_vals = z%im

    do i = 1, 3
        if (abs(re_vals(i) - real(i)) > 1e-6) error stop
        if (abs(im_vals(i) - real(i + 3)) > 1e-6) error stop
    end do

    do i = 1, 3
        if (abs(z(i)%re - real(i)) > 1e-6) error stop
        if (abs(z(i)%im - real(i + 3)) > 1e-6) error stop
    end do

end program complex_33
