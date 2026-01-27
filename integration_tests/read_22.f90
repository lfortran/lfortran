! Test complex number input parsing with whitespace inside parentheses.
! Fortran list-directed format allows: ( 0.1000E+01, 0.2000E+01)
program read_22
    implicit none
    complex :: c1, c2
    complex(8) :: c3
    complex :: arr(2)

    ! Test single-precision complex with spaces inside parentheses
    open(10, file='read_22_data.txt', status='replace')
    write(10, '(A)') '( 0.1000E+01, 0.2000E+01)'
    write(10, '(A)') '(3.0, 4.0)'
    write(10, '(A)') '( 0.5000D+01, 0.6000D+01)'
    write(10, '(A)') '( 0.7000E+01, 0.8000E+01) ( 0.9000E+01, 0.1000E+02)'
    close(10)

    open(10, file='read_22_data.txt', status='old')
    read(10, *) c1
    read(10, *) c2
    read(10, *) c3
    read(10, *) arr
    close(10, status='delete')

    ! Verify single-precision with spaces
    if (abs(real(c1) - 1.0) > 1e-5) error stop 1
    if (abs(aimag(c1) - 2.0) > 1e-5) error stop 2

    ! Verify without spaces
    if (abs(real(c2) - 3.0) > 1e-5) error stop 3
    if (abs(aimag(c2) - 4.0) > 1e-5) error stop 4

    ! Verify double-precision with D exponent
    if (abs(real(c3) - 5.0d0) > 1d-10) error stop 5
    if (abs(aimag(c3) - 6.0d0) > 1d-10) error stop 6

    ! Verify array reading
    if (abs(real(arr(1)) - 7.0) > 1e-5) error stop 7
    if (abs(aimag(arr(1)) - 8.0) > 1e-5) error stop 8
    if (abs(real(arr(2)) - 9.0) > 1e-5) error stop 9
    if (abs(aimag(arr(2)) - 10.0) > 1e-5) error stop 10

    print *, "PASS"
end program
