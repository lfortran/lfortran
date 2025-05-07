program intrinsics_372
    real, allocatable :: a(:), b(:) 
    complex :: c(2)
     allocate(a(2))
     allocate(b(2))
     a = 1
     b = 2
    c = cmplx(a,b)
    print *, c
    if (c(1) /= (1.0, 2.0)) error stop 
    if (c(2) /= (1.0, 2.0)) error stop
end program