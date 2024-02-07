program types_25
    complex(8) :: A(3) = [(1, 1), (2, 2), (3, 3)]
    print*, A

    if ( abs(real(A(1)) - 1.00) > 10e-5 ) error stop
    if ( abs(imag(A(2)) - 2.00) > 10e-5 ) error stop
end