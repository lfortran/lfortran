program intrinsics_458
    implicit none
    
    real :: h, ha, res
    double precision :: dh, dha, dres

    COMMON /c1/  h, ha, res
    COMMON /c2/  dh, dha, dres
    
    h = -3.0
    ha = 5.0
    res = sign(ha, h)
    print *, "res: ", res
    if (abs(res - (-5.0)) > 1e-6) error stop

    h = 7.0
    ha = 5.0
    res = sign(ha, h)
    print *, "res: ", res
    if (abs(res - 5.0) > 1e-6) error stop

    h = 4.0
    ha = -2.0
    res = sign(h, ha)
    print *, "res: ", res
    if (abs(res - (-4.0)) > 1e-6) error stop

    h = -9.0
    res = sign(h, h)
    print *, "res: ", res
    if (abs(res - (-9.0)) > 1e-6) error stop

    h = 6.0
    ha = -1.0
    h = sign(ha, h)
    print *, "h: ", h
    if (abs(h - 1.0) > 1e-6) error stop

    dh = -3.0d0
    dha = 5.0d0
    dres = sign(dha, dh)
    print *, "dres: ", dres
    if (abs(dres - (-5.0d0)) > 1d-12) error stop

    dh = 7.0d0
    dha = 5.0d0
    dres = sign(dha, dh)
    print *, "dres: ", dres
    if (abs(dres - 5.0d0) > 1d-12) error stop

    print *, "All tests passed."

end program
