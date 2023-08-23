program intrinsics_66
    double precision :: x
    x = 1.0D0
    print *, dcosh(x)
    print *, dsinh(x)
    print *, dcosh(1.2D0)
    print *, dsinh(1.2D0)
    print *, dtanh(x)
    print *, dtanh(1.2D0)

    if (abs(dcosh(x) - 1.5430806348152437D0) > 1d-14) error stop
    if (abs(dsinh(x) - 1.1752011936438014D0) > 1d-14) error stop
    if (abs(dcosh(1.2D0) - 1.8106555673243747D0) > 1d-14) error stop
    if (abs(dsinh(1.2D0) - 1.5094613554121725D0) > 1d-12) error stop
    if (abs(dtanh(x) - 0.76159415595576485D0) > 1d-14) error stop
    if (abs(dtanh(1.2D0) - 0.83365460701215521D0) > 1d-14) error stop

    x = 4.2D0
    print *, dcosh(x)
    print *, dsinh(x)
    print *, dcosh(4.2D0)
    print *, dsinh(4.2D0)
    print *, dtanh(x)
    print *, dtanh(4.2D0)
    if (abs(dcosh(x) - 33.350663308872818D0) > 1d-14) error stop
    if (abs(dsinh(x) - 33.335667732052336D0) > 1d-14) error stop
    if (abs(dcosh(4.2D0) - 33.350663308872818D0) > 1d-14) error stop
    if (abs(dsinh(4.2D0) - 33.335667732052336D0) > 1d-14) error stop
    if (abs(dtanh(x) - 0.99955036645953343D0) > 1d-14) error stop
    if (abs(dtanh(4.2D0) - 0.99955036645953343D0) > 1d-14) error stop
end program
    