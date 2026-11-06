program associate_62
    implicit none
    real :: b, c1, c2, s1, s2, l1, l2, a1, a2, e1, e2
    b = 1.0
    ! An intrinsic applied to an associate name must see the selector's value
    ! on every evaluation, not the previous call's result.
    associate (z => b)
        c1 = cos(z)
        c2 = cos(z)
        s1 = sin(z)
        s2 = sin(z)
        l1 = log(z)
        l2 = log(z)
        a1 = atan(z)
        a2 = atan(z)
        e1 = erf(z)
        e2 = erf(z)
    end associate
    if (abs(c1 - cos(1.0)) > 1e-6) error stop 1
    if (abs(c2 - cos(1.0)) > 1e-6) error stop 2
    if (abs(s1 - sin(1.0)) > 1e-6) error stop 3
    if (abs(s2 - sin(1.0)) > 1e-6) error stop 4
    if (abs(l1 - log(1.0)) > 1e-6) error stop 5
    if (abs(l2 - log(1.0)) > 1e-6) error stop 6
    if (abs(a1 - atan(1.0)) > 1e-6) error stop 7
    if (abs(a2 - atan(1.0)) > 1e-6) error stop 8
    if (abs(e1 - erf(1.0)) > 1e-6) error stop 9
    if (abs(e2 - erf(1.0)) > 1e-6) error stop 10
    print *, "All tests passed"
end program
