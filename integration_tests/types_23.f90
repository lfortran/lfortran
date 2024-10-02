program types_23
    complex(8) :: A(3) = [(1, 1), (2, 2), (3, 3)]
    integer(8) :: B(3) = [(1, 1), (2, 2), (3, 3)]
    real(8) :: C(3) = [(1, 1), (2, 2), (3, 3)]
    complex(8) :: D(3) = [1, 2, 3]

    print*, A
    if ( abs(real(A(1)) - 1.00) > 10e-5 ) error stop
    if ( abs(imag(A(2)) - 2.00) > 10e-5 ) error stop

    print*, B
    if ( abs(B(1) - 1.00) > 10e-5 ) error stop

    print*, C
    if ( abs(C(1) - 1.00) > 10e-5 ) error stop

    print*, D
    if ( abs(real(D(1)) - 1.00) > 10e-5 ) error stop
    if ( abs(imag(D(2)) - 0.00) > 10e-5 ) error stop
end
