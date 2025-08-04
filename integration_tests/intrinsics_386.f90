program main
    complex :: x(2) = [(1.0, 2.0), (3.0, 4.0)]
    print *, product(x)
    if (product(x) /= (-5.0, 10.0)) error stop
end program