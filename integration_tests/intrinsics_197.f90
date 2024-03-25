program intrinsic_197
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: x, y, z, w
    real :: i, j, k, l

    x = 1036.462826483388272_dp
    y = 1.7197292882018389_dp
    z = 10.2368267382872828_dp
    w = 0.17197292882018389_dp

    i = 152.67283628_sp
    j = 632.92729728_sp
    k = 1.2728272919_sp
    l = 5.1727272919_sp

    print *, bessel_y0(x)
    if(bessel_y0(x) - (-2.1476418943648812E-002_dp) > 1e-12) error stop

    print *, bessel_y0(y)
    if(bessel_y0(y) - (0.45752410314844527_dp) > 1e-12) error stop

    print *, bessel_y0(z)
    if(bessel_y0(z) - (-3.6134118144921369e-3_dp) > 1e-12) error stop

    print *, bessel_y0(w)
    if(bessel_y0(w) - (-1.1810118244863275_dp) > 1e-12) error stop

    print *, bessel_j0(1036.462826483388272_dp)
    if(bessel_j0(1036.462826483388272_dp) - (1.2368783577999469E-002_dp) > 1e-12) error stop

    print *, bessel_j0(1.7197292882018389_dp)
    if(bessel_j0(1.7197292882018389_dp) - (0.38657517808346376_dp) > 1e-12) error stop

    print *, bessel_j0(10.2368267382872828_dp)
    if(bessel_j0(10.2368267382872828_dp) - (-0.24920488318544790_dp) > 1e-12) error stop

    print *, bessel_j0(0.17197292882018389_dp)
    if(bessel_j0(0.17197292882018389_dp) - (0.99261998331270740_dp) > 1e-12) error stop

    print *, bessel_y0(i)
    if(bessel_y0(i) - (5.72582334E-02) > 1e-5) error stop

    print *, bessel_y0(j)
    if(bessel_y0(j) - (-1.99812334E-02) > 1e-5) error stop

    print *, bessel_y0(k)
    if(bessel_y0(k) - (0.271367937) > 1e-5) error stop

    print *, bessel_y0(l)
    if(bessel_y0(l) - (-0.328962386) > 1e-12) error stop

    print *, bessel_j0(152.67283628_sp)
    if(bessel_j0(152.67283628_sp) - (2.98547018E-02) > 1e-12) error stop

    print *, bessel_j0(632.92729728_sp)
    if(bessel_j0(632.92729728_sp) - (-2.46289242E-02) > 1e-12) error stop

    print *, bessel_j0(1.2728272919_sp)
    if(bessel_j0(1.2728272919_sp) - (0.634188831) > 1e-12) error stop

    print *, bessel_j0(0.1727272919_sp)
    if(bessel_j0(0.1727272919_sp) - (0.992555201) > 1e-12) error stop


end program
