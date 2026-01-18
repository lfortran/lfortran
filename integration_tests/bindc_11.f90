module bindc_11_mod
use, intrinsic :: iso_c_binding, only: c_float
implicit none
contains

    complex(c_float) function twice(z) result(r) bind(c)
        complex(c_float), value, intent(in) :: z
        r = z + z
    end function twice

end module bindc_11_mod

program bindc_11
use, intrinsic :: iso_c_binding, only: c_float
use bindc_11_mod, only: twice
implicit none

    complex(c_float) :: z, r
    real(c_float) :: re, im
    integer :: i

    z = cmplx(5.0_c_float, 7.0_c_float, kind=c_float)

    r = twice(z)
    re = real(r, kind=c_float)
    im = aimag(r)
    if (abs(re - 10.0_c_float) > 1.0e-6_c_float) error stop
    if (abs(im - 14.0_c_float) > 1.0e-6_c_float) error stop

    r = cmplx(0.0_c_float, 0.0_c_float, kind=c_float)
    do i = 1, 700000
        r = r + twice(z)
    end do

    re = real(r, kind=c_float)
    im = aimag(r)
    if (abs(re - 7000000.0_c_float) > 1.0_c_float) error stop
    if (abs(im - 9800000.0_c_float) > 1.0_c_float) error stop
    print *, "PASSED"

end program bindc_11
