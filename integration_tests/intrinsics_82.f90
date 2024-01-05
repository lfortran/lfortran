program intrinsics_82
    double precision :: x(5)
    x(1) = 1.0d0
    x(2) = 2.0d0
    x(3) = 3.0d0
    x(4) = 4.0d0
    x(5) = 5.0d0

    print *, tan(x)
    if( abs(tan(x(1)) - 1.5574077246549023) > 1e-5) error stop
    if( abs(tan(x(2)) + 2.1850398632615189) > 1e-5) error stop
    if( abs(tan(x(3)) + 0.14254654307427780) > 1e-5) error stop
    if( abs(tan(x(4)) - 1.1578212823495775) > 1e-5) error stop
    if( abs(tan(x(5)) + 3.3805150062465859) > 1e-5) error stop

    print *, cosh(x)
    if( abs(cosh(x(1)) - 1.5430806348152437) > 1e-5) error stop
    if( abs(cosh(x(2)) - 3.7621956910836314) > 1e-5) error stop
    if( abs(cosh(x(3)) - 10.067661995777765) > 1e-5) error stop
    if( abs(cosh(x(4)) - 27.308232836016487) > 1e-5) error stop
    if( abs(cosh(x(5)) - 74.209948524787848) > 1e-5) error stop

    print *, sinh(x)
    if( abs(sinh(x(1)) - 1.1752011936438014) > 1e-5) error stop
    if( abs(sinh(x(2)) - 3.6268604078470190) > 1e-5) error stop
    if( abs(sinh(x(3)) - 10.017874927409903) > 1e-5) error stop
    if( abs(sinh(x(4)) - 27.289917197127750) > 1e-5) error stop
    if( abs(sinh(x(5)) - 74.203210577788752) > 1e-5) error stop
    
    print *, tanh(x)
    if( abs(tanh(x(1)) - 0.76159415595576485) > 1e-5) error stop
    if( abs(tanh(x(2)) - 0.96402758007581690) > 1e-5) error stop
    if( abs(tanh(x(3)) - 0.99505475368673046) > 1e-5) error stop
    if( abs(tanh(x(4)) - 0.99932929973906703) > 1e-5) error stop
    if( abs(tanh(x(5)) - 0.99990920426259511) > 1e-5) error stop

end program