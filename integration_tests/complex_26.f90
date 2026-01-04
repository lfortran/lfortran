! Test: Complex literal assigned to Real PARAMETER
! LAPACK legacy pattern where REAL parameters use complex literal syntax
program complex_26
    implicit none
    real :: zero, one
    parameter (zero = (0.0E+0, 0.0E+0), one = (1.0E+0, 0.0E+0))

    ! GFortran accepts this as an extension, extracting the real part
    if (abs(zero) > 1e-6) error stop
    if (abs(one - 1.0) > 1e-6) error stop

    print *, "PASS"
end program
