program intrinsics_49
    ! A test of complex and real kinds 4 and 8 sin intrinsic
    implicit none
    block
        real(kind=4) :: x, y
        complex :: z, r
        x = 1.0
        y = 2.0
        z = cmplx(x, y)
        r = sin(z)
        if (abs(real(r) - 3.16577864) > 1e-6) error stop
        if (abs(imag(r) - 1.95960093) > 1e-6) error stop
    end block
    block
        real(kind=8) :: x, y
        complex :: z, r
        x = 1.0
        y = 2.0
        z = cmplx(x, y)
        r = sin(z)
        if (abs(real(r) - 3.16577864) > 1e-6) error stop
        if (abs(imag(r) - 1.95960093) > 1e-6) error stop
    end block
    block
        real(kind=4) :: x, r
        x = 1.0
        r = sin(x)
        if (abs(r - 0.841470957) > 1e-6) error stop
    end block
    block
        real(kind=8) :: x, r
        x = 1.0
        r = sin(x)
        if (abs(r - 0.841470957) > 1e-6) error stop
    end block
end program