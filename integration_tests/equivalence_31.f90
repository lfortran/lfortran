program equivalence_31
    implicit none
    ! Test: reading a complex variable that is equivalenced with a real array
    complex :: z
    real :: r(2)
    equivalence (r, z)
    character(len=10) :: buf

    buf = '  1.0  2.0'
    read(buf, '(F5.1,F5.1)') z

    if (abs(real(z) - 1.0) > 1.0e-6) error stop
    if (abs(aimag(z) - 2.0) > 1.0e-6) error stop

    print *, real(z), aimag(z)
end program
