! c_long_double_complex is the kind of C long double complex, not -1.
program c_long_double_complex_01
    use iso_c_binding, only: c_long_double, c_long_double_complex
    implicit none
    complex(c_long_double_complex) :: z
    if (c_long_double_complex < 0) error stop 1
    if (c_long_double_complex /= c_long_double) error stop 2
    z = (1.0_c_long_double, 2.0_c_long_double)
    if (real(z, kind=c_long_double) /= 1.0_c_long_double) error stop 3
    if (aimag(z) /= 2.0_c_long_double) error stop 4
end program
