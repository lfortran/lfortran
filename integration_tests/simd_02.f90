program simd_02
    !LF$ attributes simd :: A, B, res
    real :: A(4), B(4), C(8), res(4)
    C = [3.14, 7.25, 0.98, 5.67, 2.35, 9.42, 6.11, 1.99]
    A = C(:4)
    B = C(5:)
    res = A + B * A
    C(:4) = res
    res = A * B
    C(5:) = res
    print *, C
    print *, sum(C)
    if (abs(sum(C) - 202.930176) > 1e-5 ) error stop
end program simd_02
