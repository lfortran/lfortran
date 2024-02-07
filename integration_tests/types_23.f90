program types_23
    complex(8) :: B(3) = [1, 2, 3]
    print*, B

    if ( abs(real(B(1)) - 1.00) > 10e-5 ) error stop
    if ( abs(imag(B(2)) - 0.00) > 10e-5 ) error stop
end