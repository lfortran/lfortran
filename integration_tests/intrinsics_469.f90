program mre
    integer, parameter :: r = dot_product(shape(1), shape(1))
    if (r /= 0) error stop "dot_product of empty arrays should be 0"
    print *, "PASS"
end program mre
