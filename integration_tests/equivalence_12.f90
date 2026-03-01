program equivalence_12
    implicit none
    double precision :: dt19x(2,2,4), dt19xa(2,2,2)
    equivalence (dt19x(1,1,1), dt19xa(1,1,1))
    dt19xa = 0.0d0
    dt19xa(1,1,1) = 1.0d0
    if (abs(dt19x(1,1,1) - 1.0d0) > 1.0d-10) error stop
    print *, "PASS"
end program
