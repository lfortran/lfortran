program intrinsics_245
    implicit none
    complex :: complex_arg, result

    complex_arg = cmplx(real(1.0, kind=4), 0.0)
    result = cmplx(complex_arg, kind = 8)
    print *, result
    if (result /= (1.0, 0.0)) error stop

    print *, cmplx(real(1, kind=4), 0.00000000, kind=8)
    if (cmplx(real(1, kind=4), 0.00000000, kind=8) /= (1.0, 0.0)) error stop

    print *, cmplx(1.00000000, real(0, kind=4), kind=8)
    if (cmplx(1.00000000, real(0, kind=4), kind=8) /= (1.0, 0.0)) error stop
    
end program 