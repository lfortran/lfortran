program operator_overloading_36
    use operator_overloading_36_mod_base
    implicit none
    type(array_type) :: a, b, c
    a%val = 3.0
    b%val = 5.0
    c = a - b
    if (c%val < -2.01 .or. c%val > -1.99) error stop
    c = -a
    if (c%val < -3.01 .or. c%val > -2.99) error stop
    print *, "PASS"
end program
