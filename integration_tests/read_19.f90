program read_19
    ! Test D exponent notation in READ
    use iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: x
    integer :: u

    open(newunit=u, status='scratch')

    ! Test D+xx notation
    write(u, '(A)') "1.5D+02"
    rewind(u)
    read(u, *) x
    if (abs(x - 150.0_dp) > 1.0e-10_dp) error stop "D+02 failed"

    ! Test D-xx notation
    rewind(u)
    write(u, '(A)') "2.5D-03"
    rewind(u)
    read(u, *) x
    if (abs(x - 0.0025_dp) > 1.0e-15_dp) error stop "D-03 failed"

    ! Test d (lowercase) notation
    rewind(u)
    write(u, '(A)') "3.0d+01"
    rewind(u)
    read(u, *) x
    if (abs(x - 30.0_dp) > 1.0e-10_dp) error stop "d+01 failed"

    close(u)
    print *, "PASS: D exponent notation works"
end program read_19
