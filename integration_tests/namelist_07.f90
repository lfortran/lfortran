program namelist_complex
    implicit none

    ! Define complex variables
    complex(4) :: c4_scalar
    complex(8) :: c8_scalar
    complex(4) :: c4_array(3)
    complex(8) :: c8_array(2)

    ! Define namelist
    namelist /complex_test/ c4_scalar, c8_scalar, c4_array, c8_array

    ! Initialize complex scalars
    c4_scalar = (1.5, -2.3)
    c8_scalar = (3.14159, 2.71828)

    ! Initialize complex arrays
    c4_array(1) = (1.0, 1.0)
    c4_array(2) = (2.0, -2.0)
    c4_array(3) = (3.0, 3.0)

    c8_array(1) = (10.5, -20.3)
    c8_array(2) = (-15.7, 30.2)

    ! Write namelist to file
    open(unit=10, file='namelist_complex.dat', status='replace', form='formatted')
    write(10, nml=complex_test)
    close(10)

    ! Reset all values to zero
    c4_scalar = (0.0, 0.0)
    c8_scalar = (0.0d0, 0.0d0)
    c4_array = (0.0, 0.0)
    c8_array = (0.0d0, 0.0d0)

    ! Read namelist from file
    open(unit=10, file='namelist_complex.dat', status='old', form='formatted')
    read(10, nml=complex_test)
    close(10)

    ! Verify complex scalars (with tolerance for floating point)
    if (abs(real(c4_scalar) - 1.5) > 1.0e-5) error stop "c4_scalar real part mismatch"
    if (abs(aimag(c4_scalar) - (-2.3)) > 1.0e-5) error stop "c4_scalar imag part mismatch"
    if (abs(real(c8_scalar) - 3.14159d0) > 1.0d-5) error stop "c8_scalar real part mismatch"
    if (abs(aimag(c8_scalar) - 2.71828d0) > 1.0d-5) error stop "c8_scalar imag part mismatch"

    ! Verify complex arrays
    if (abs(real(c4_array(1)) - 1.0) > 1.0e-5) error stop "c4_array(1) real mismatch"
    if (abs(aimag(c4_array(1)) - 1.0) > 1.0e-5) error stop "c4_array(1) imag mismatch"
    if (abs(real(c4_array(2)) - 2.0) > 1.0e-5) error stop "c4_array(2) real mismatch"
    if (abs(aimag(c4_array(2)) - (-2.0)) > 1.0e-5) error stop "c4_array(2) imag mismatch"
    if (abs(real(c4_array(3)) - 3.0) > 1.0e-5) error stop "c4_array(3) real mismatch"
    if (abs(aimag(c4_array(3)) - 3.0) > 1.0e-5) error stop "c4_array(3) imag mismatch"

    if (abs(real(c8_array(1)) - 10.5d0) > 1.0d-5) error stop "c8_array(1) real mismatch"
    if (abs(aimag(c8_array(1)) - (-20.3d0)) > 1.0d-5) error stop "c8_array(1) imag mismatch"
    if (abs(real(c8_array(2)) - (-15.7d0)) > 1.0d-5) error stop "c8_array(2) real mismatch"
    if (abs(aimag(c8_array(2)) - 30.2d0) > 1.0d-5) error stop "c8_array(2) imag mismatch"

    print *, "Complex number namelist test passed!"

end program namelist_complex
