program arrays_intrin_09
    implicit none
    real :: x(10, 10)
    x = 6.0
    if (count(abs(x - 6.0) > 1e-6) /= 0) error stop
    if (any(abs(x - 6.0) > 1e-6)) error stop
    print *, "ok"
end program
