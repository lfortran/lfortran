program nested_vars_06
    real xt

    xt = 5.0
    call b1(xt)

    if (abs(xt - 10.0) > 1e-5) error stop 1
end program

subroutine b1(xt)
    real xt
    real bvalu
    real x, h
    real g8

    g8(x, h) = bvalu(xt)

    xt = g8(1.2, 3.4)
end subroutine

real function bvalu(x)
    real x

    bvalu = x * 2.0
end function
