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

    print *, bessel_y0(3.229995504589554d-153)
    if(bessel_y0(3.229995504589554d-153) - (-223.60567312692984) > 1e-12) error stop

    print *, bessel_y0(1.9201212555874555d-153)
    if(bessel_y0(1.9201212555874555d-153) - (-223.93677423698244) > 1e-12) error stop

    print *, bessel_y0(6.809537363878625d-155)
    if(bessel_y0(6.809537363878625d-155) - (-226.06259684300085) > 1e-12) error stop

    print *, bessel_y0(5.690181373063991d-154)
    if(bessel_y0(5.690181373063991d-154) - (-224.71105113517498) > 1e-12) error stop

    print *, bessel_y0(4.9984915359820346d-154)
    if(bessel_y0(4.9984915359820346d-154) - (-224.79356084510076_dp) > 1e-12) error stop

    print *, bessel_y0(29.82407037185126)
    if(bessel_y0(29.82407037185126) - (-0.100661635) > 1e-12) error stop

    print *, bessel_y0(35.2459016393)
    if(bessel_y0(35.2459016393) - (1.34913139E-02) > 1e-12) error stop

    print *, bessel_y0(46.2634946)
    if(bessel_y0(46.2634946) - (0.116948858) > 1e-12) error stop

    print *, bessel_y0(24.7740903638)
    if(bessel_y0(24.7740903638) - (-0.146244213) > 1e-12) error stop

    print *, bessel_y0(15.8978408636372)
    if(bessel_y0(15.8978408636372) - (0.113518834) > 1e-12) error stop

    print *, bessel_y0(8.801468212714914)
    if(bessel_y0(8.801468212714914) - (0.265794873) > 1e-12) error stop

    print *, bessel_y0(8.798080767692923)
    if(bessel_y0(8.798080767692923) - (0.265978813) > 1e-12) error stop

    print *, bessel_y0(8.791287884846061575)
    if(bessel_y0(8.791287884846061575) - (0.266338646) > 1e-12) error stop

    print *, bessel_y0(0.6635324560331525)
    if(bessel_y0(0.6635324560331525) - (-0.231845424) > 1e-12) error stop

    print *, bessel_y0(0.6633115808685429)
    if(bessel_y0(0.6633115808685429) - (-0.232100740) > 1e-12) error stop

    print *, bessel_y0(0.6630906573122884)
    if(bessel_y0(0.6630906573122884) - (-0.232356280) > 1e-12) error stop

    print *, bessel_y0(0.6628696853957159)
    if(bessel_y0(0.6628696853957159) - (-0.232611880) > 1e-12) error stop

    print *, bessel_y0(0.5042349934053336)
    if(bessel_y0(0.5042349934053336) - (-0.438309371) > 1e-12) error stop

    print *, bessel_y0(0.5041796815747646)
    if(bessel_y0(0.5041796815747646) - (-0.438390195) > 1e-12) error stop

    print *, bessel_y0(0.5041242821300232)
    if(bessel_y0(0.5041242821300232) - (-0.438471109 ) > 1e-12) error stop

    print *, bessel_y0(0.50401322049655)
    if(bessel_y0(0.50401322049655) - (-0.438633382) > 1e-12) error stop

    print *, bessel_y0(1.3994502099160337d-303)
    if(bessel_y0(1.3994502099160337d-303) - (-444.01882239162842_dp) > 1e-12) error stop

    print *, bessel_y0(1.199530179928029d-303)
    if(bessel_y0(1.199530179928029d-303) - (-444.11695700415243_dp) > 1e-12) error stop

    print *, bessel_y0(9.99610149940024d-304)
    if(bessel_y0(9.99610149940024d-304) - (-444.23302545068861_dp) > 1e-12) error stop

    print *, bessel_y0(5.9977008996401444d-304)
    if(bessel_y0(5.9977008996401444d-304) - (-444.55822289723716_dp) > 1e-12) error stop

    print *, bessel_y0(499.19032387045183)
    if(bessel_y0(499.19032387045183) - (3.19634713E-02) > 1e-12) error stop

    print *, bessel_y0(499.2802878848461)
    if(bessel_y0(499.2802878848461) - (3.04011665E-02) > 1e-12) error stop

    print *, bessel_y0(499.3702518992403)
    if(bessel_y0(499.3702518992403) - (2.85927802E-02) > 1e-12) error stop

    print *, bessel_y0(499.5501799280288)
    if(bessel_y0(499.5501799280288) - (2.42997743E-02) > 1e-12) error stop


end program
