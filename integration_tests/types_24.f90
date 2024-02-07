program types_24
    integer(8) :: B(3) = [(1, 1), (2, 2), (3, 3)]
    print*, B

    if ( abs(B(1) - 1.00) > 10e-5 ) error stop
end