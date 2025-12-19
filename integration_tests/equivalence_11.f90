! MRE: EQUIVALENCE + DATA with double precision arrays
! Reduced from LAPACK BLAS testing (dblat1.f DATA statements)
program equivalence_11
    implicit none
    double precision :: a(4), b(4)
    equivalence (a(1), b(1))
    data b /1.0d0, 2.0d0, 3.0d0, 4.0d0/
    if (abs(a(1) - 1.0d0) > 1.0d-10) error stop
    if (abs(a(2) - 2.0d0) > 1.0d-10) error stop
    if (abs(a(4) - 4.0d0) > 1.0d-10) error stop
    print *, "PASS"
end program
