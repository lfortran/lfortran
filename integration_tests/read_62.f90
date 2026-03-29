program read_62
    use iso_fortran_env, only: dp => real64
    implicit none

    character(10) :: s
    real(dp) :: x

    s = '12345D+01'
    read(s, '(f10.5)') x

    if (abs(x - 1.2345_dp) > 1.0e-12_dp) error stop 'f10.5 + D exponent parsing failed'
end program read_62