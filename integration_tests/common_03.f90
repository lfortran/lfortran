subroutine pass()
    real A(2), B
    integer i(5), e
    common /SaMpLe/ A, B, i 
    if (abs(A(1) - 10.0) > 1e-7) error stop
    if (abs(B - 20.0) > 1e-7) error stop
    print *, A, B, e
end subroutine

program common3
    real A, B, z1, z2
    integer c, d, e, f, g, h, i(5), j
    common z1, /c/ c, /b/ d /Sample/ A(2), /b/ e, f, //z2 /c/ g, h, j /sample/ B, i
    A = [10.0, 11.0]
    B = 20.0
    e = 123
    call pass()
end program
