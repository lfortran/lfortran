program equivalence_10
    REAL A, A1
    DIMENSION A(26,4), A1(26,4)
    EQUIVALENCE (A(1,1),A1(1,1))
    DATA A1(1,1) /42.0/
    if (any(shape(A1) /= [26,4])) error stop
    if (A(1,1) /= 42.0 .or. A1(1,1) /= 42.0) error stop
end program
