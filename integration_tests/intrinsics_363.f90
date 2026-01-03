program intrinsics_363
    real :: a(2) = cshift([1.0,2.0], 1)
    print *, a
    if (any(a /= [2.0, 1.0])) error stop
end program
