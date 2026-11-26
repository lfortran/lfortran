! bind(c) procedures with complex VALUE arguments, called from Fortran. The
! C ABI of WebAssembly passes them in memory.
module bindc_complex_value_01_m
    use, intrinsic :: iso_c_binding, only: c_int, c_float_complex, c_double_complex
    implicit none
contains
    integer(c_int) function scale_z(z, f, r) result(code) bind(c, name="bindc_complex_value_01_z")
        complex(c_double_complex), value :: z, f
        complex(c_double_complex), intent(out) :: r
        r = f*z
        code = 0
    end function

    subroutine scale_c(z, r) bind(c, name="bindc_complex_value_01_c")
        complex(c_float_complex), value :: z
        complex(c_float_complex), intent(out) :: r
        r = 2*z
    end subroutine
end module

program bindc_complex_value_01
    use bindc_complex_value_01_m
    implicit none
    complex(c_double_complex) :: r
    complex(c_float_complex) :: s, zc
    if (scale_z((1.5d0, -2d0), (0d0, 2d0), r) /= 0) error stop
    print *, r
    if (r /= (4d0, 3d0)) error stop
    zc = (0.5, 0.25)
    call scale_c(zc, s)
    print *, s
    if (s /= (1.0, 0.5)) error stop
end program
