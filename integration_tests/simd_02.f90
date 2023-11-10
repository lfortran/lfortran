program simd_02
    !LF$ attributes simd :: A, B, res
    real :: A(4), B(4), C(8), res(4)
    C = 43
    A = C(:4)
    B = C(5:)
    res = A + B
    C(:4) = res
    res = A * B
    C(5:) = res
    print *, C
    print *, sum(C)
    if ((sum(C)) /= 7740.00) error stop
end program simd_02
