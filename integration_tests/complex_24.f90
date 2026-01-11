program complex_24
    complex(4) :: d(2,1)
    real (4) :: r(2,1)
    d = cmplx(1, 1) * reshape([1, 2], [2, 1])
    print *, d
    if (abs(d(1, 1) - (1.00000000e+00, 1.00000000e+00)) > 1.0e-6) error stop
    if (abs(d(2, 1) - (2.00000000e+00, 2.00000000e+00)) > 1.0e-6) error stop
end program complex_24
