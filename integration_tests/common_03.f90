subroutine pass()
    real A(2), B
    integer i(5), e
    common /sample/ A, B, i
    if (abs(A(1) - 10.0) > 1e-7) error stop
    if (abs(B - 20.0) > 1e-7) error stop
    print *, A, B, e
end subroutine

program common1
    real A, B
    integer c, d, e, f, g, h, i(5), j
    common /c/ c, /b/ d /sample/ A(2), /b/ e, f, /c/ g, h, j /sample/ B, i
    A = [10.0, 11.0]
    B = 20.0
    call pass()
end program
