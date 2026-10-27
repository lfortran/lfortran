module complex_44_mod
    implicit none
    complex :: z, za(2)
contains
    real function hr()
        z%re = 4.0
        hr = 3.0
    end function

    function har() result(r)
        real :: r(2)
        za%re = 4.0
        r = 3.0
    end function
end module

program complex_44
    use complex_44_mod
    implicit none
    z = (1.0, 1.0)
    z%im = hr()
    print *, z
    if (z /= (4.0, 3.0)) error stop

    za = (1.0, 1.0)
    za%im = har()
    print *, za
    if (any(za /= (4.0, 3.0))) error stop
end program
