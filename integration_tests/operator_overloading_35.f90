program operator_overloading_35
    use operator_overloading_35_reexport, only: vec_t, operator(-)
    implicit none
    type(vec_t) :: a, b, c
    a%x = 5.0; b%x = 3.0
    c = a - b
    if (abs(c%x - 2.0) > 1.0e-5) error stop
    c = -a
    if (abs(c%x + 5.0) > 1.0e-5) error stop
    c = 10.0 - b
    if (abs(c%x - 7.0) > 1.0e-5) error stop
    print *, "ok"
end program
