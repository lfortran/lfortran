program complex_23
    complex, parameter :: arr(1) = [(1,1)]
    complex :: d(1)
    d =  arr  + 1
    print *, d
    if (any(d /= [(2.0,1.0)])) error stop
end program complex_23
