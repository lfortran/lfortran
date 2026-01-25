program format8
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: val

    ! Test E format exponent for values near powers of 10
    ! E format normalizes mantissa to [0.1, 1.0)
    ! e.g., 1.0e-10 becomes 0.1e-9

    val = 1.0d-10
    print '(E25.17)', val

    val = 1.0d-100
    print '(E25.17)', val

    val = 1.0d0
    print '(E25.17)', val

    val = 1.0d+10
    print '(E25.17)', val

    val = 1.0d+100
    print '(E25.17)', val
end program format8
