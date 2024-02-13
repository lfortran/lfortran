program intrinsics_138
    real :: x
    real :: y
    x = 178.1387e-4
    y = 1.00

    print *, fraction(x), x * radix(x)**(-exp(x))
end program 
