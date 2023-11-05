program simd_01
    !LF$ attributes simd :: A, B, C
    real :: A(8), B(8), C(8)
    A = 1
    B = 2
    C = A + B
end program
