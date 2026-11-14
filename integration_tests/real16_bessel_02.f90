program real16_bessel_02
implicit none
! bessel_y0, bessel_y1 and bessel_yn of non-constant real(16) arguments (#15736, #17463).
! Reference values are from GFortran/libquadmath; the accuracy checked is a
! relative error below 1e-30 (the achieved error is about 1e-32).
integer, parameter :: qp = 16
real(qp), parameter :: tol = 1.0e-30_qp
real(qp), parameter :: x_ref(9) = [0.25_qp, 1.0_qp, 2.5_qp, 8.0_qp, 25.0_qp, 39.9_qp, 40.0_qp, 40.5_qp, 120.0_qp]
real(qp), parameter :: y0_ref(9) = [ &
    -9.315730249300586869497260101900970453e-1_qp, &
    8.825696421567695798292676602351516214e-2_qp, &
    4.980703596152318878274723503620897896e-1_qp, &
    2.235214893875662205273234004986204205e-1_qp, &
    -1.272494322680061378343286586991723009e-1_qp, &
    1.247276246682603046821962457103645011e-1_qp, &
    1.259364170582609292531519661126710359e-1_qp, &
    1.133433143742016849489177080467306781e-1_qp, &
    -1.210436541001620293459910176104036310e-2_qp]
real(qp), parameter :: y1_ref(9) = [ &
    -2.704105229315282444107762069256958068e0_qp, &
    -7.812128213002887165471500000479647898e-1_qp, &
    1.459181379667857988787599405358776046e-1_qp, &
    -1.580604617312474942555552661874835779e-1_qp, &
    -9.882996478323741005333030837794576623e-2_qp, &
    -1.836745215821859298575397235238778150e-2_qp, &
    -5.793505821549632941193851787935048552e-3_qp, &
    5.498584250054305298930672609887861456e-2_qp, &
    -7.187447320914953355512286740916989694e-2_qp]
real(qp), parameter :: y3_ref(9) = [ &
    -3.285161957241599314104901666325921123e2_qp, &
    -5.821517605964728847761757064429814322e0_qp, &
    -7.560554967536709968379029772292979283e-1_qp, &
    2.654215932105844720994915766473792344e-2_qp, &
    1.179248503968929532581402658225756273e-1_qp, &
    5.771131460283219859225525549796543563e-3_qp, &
    -6.829103413384208148827314082271731022e-3_qp, &
    -6.591206117977873568268286686691967157e-2_qp, &
    7.223802179325610168985665809708836968e-2_qp]
real(qp) :: x(9), r, one
integer :: i

x = x_ref
one = x(2)
do i = 1, 9
    r = bessel_y0(x(i))
    if (.not. close(r, y0_ref(i))) error stop
    r = bessel_y1(x(i))
    if (.not. close(r, y1_ref(i))) error stop
    r = bessel_yn(3, x(i))
    if (.not. close(r, y3_ref(i))) error stop
end do
if (bessel_yn(-3, x(4)) /= -bessel_yn(3, x(4))) error stop
if (bessel_yn(0, x(1)) /= bessel_y0(x(1))) error stop
if (bessel_yn(1, x(1)) /= bessel_y1(x(1))) error stop
! tiny argument, order 4 and a long recurrence
r = bessel_y1(1.0e-10_qp*one)
if (.not. close(r, (-6.366197723675813431507891842844625884e9_qp))) error stop
r = bessel_yn(4, 0.5_qp*one)
if (.not. close(r, (-4.992725608195123265323012906674820134e2_qp))) error stop
r = bessel_yn(40, 100.0_qp*one)
if (.not. close(r, 4.074685216880344160192262888875152949e-2_qp)) error stop
! parameters are folded at compile time and agree with the run-time values
block
    real(qp), parameter :: c_y1 = bessel_y1(2.5_qp), c_y3 = bessel_yn(3, 8.0_qp), c_y4 = bessel_yn(4, 0.5_qp)
    if (.not. close(c_y1, y1_ref(3))) error stop
    if (.not. close(c_y3, y3_ref(4))) error stop
    if (.not. close(c_y4, (-4.992725608195123265323012906674820134e2_qp))) error stop
end block
print *, "ok"

contains

    logical function close(a, b)
        real(qp), intent(in) :: a, b
        close = abs(a - b) <= tol*abs(b)
    end function
end program
