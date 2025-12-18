! MRE from sblat1.f: down_cast assert with EQUIVALENCE + multi-value DATA
! EQUIVALENCE aliases array Y to part of X, then DATA initializes Y
! with multiple values. This triggers LCOMPILERS_ASSERT in down_cast.
program lapack_07
    real :: x(4), y(2)
    equivalence (x(1), y(1))
    data y/1.0, 2.0/
    if (x(1) /= 1.0) error stop
    if (x(2) /= 2.0) error stop
    print *, 'PASS'
end program
