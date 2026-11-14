program real16_bessel_01
implicit none
! bessel_j0, bessel_j1 and bessel_jn of non-constant real(16) arguments (#15736, #17463).
! Reference values are from GFortran/libquadmath; the accuracy checked is a
! relative error below 1e-30 (the achieved error is about 1e-32).
integer, parameter :: qp = 16
real(qp), parameter :: tol = 1.0e-30_qp
real(qp), parameter :: x_ref(9) = [0.25_qp, 1.0_qp, 2.5_qp, 8.0_qp, 25.0_qp, 39.9_qp, 40.0_qp, 40.5_qp, 120.0_qp]
real(qp), parameter :: j0_ref(9) = [ &
    9.844359292958527049236911404547618262e-1_qp, &
    7.651976865579665514497175261026632278e-1_qp, &
    -4.838377646819799632728777885120344897e-2_qp, &
    1.716508071375539060908694078519720368e-1_qp, &
    9.626678327595811617350334075402446651e-2_qp, &
    1.992864681846520402163778865185591163e-2_qp, &
    7.366890584237289553531735691438072796e-3_qp, &
    -5.358267563226294688182981080654374681e-2_qp, &
    7.182341582915612757621875528720560405e-2_qp]
real(qp), parameter :: j1_ref(9) = [ &
    1.240259773227269227315749766638174003e-1_qp, &
    4.400505857449335159596822037189148882e-1_qp, &
    4.970941024642740380108162762644223005e-1_qp, &
    2.346363468539146243812766515904547039e-1_qp, &
    -1.253502495802899046518092710572512027e-1_qp, &
    1.249871016188417262593398071217700346e-1_qp, &
    1.260383180375849992056027218391718554e-1_qp, &
    1.126905299405943127052042786702928849e-1_qp, &
    -1.180521143300189111653384092776328974e-2_qp]
real(qp), parameter :: j3_ref(9) = [ &
    3.242512526759081309637890286201309513e-4_qp, &
    1.956335398266840591890532162175150781e-2_qp, &
    2.166003910391135247666890035159637423e-1_qp, &
    -2.911322070659522493790517740676338663e-1_qp, &
    1.083430810615088952845055778670744678e-1_qp, &
    -1.263568890066573258798061814610405473e-1_qp, &
    -1.261448155058208031649278817991198087e-1_qp, &
    -1.068487877110857915063028529242039188e-1_qp, &
    9.404539121233908035595141395452122588e-3_qp]
real(qp) :: x(9), r, one
integer :: i

x = x_ref
one = x(2)
do i = 1, 9
    r = bessel_j0(x(i))
    if (.not. close(r, j0_ref(i))) error stop
    r = bessel_j1(x(i))
    if (.not. close(r, j1_ref(i))) error stop
    r = bessel_jn(3, x(i))
    if (.not. close(r, j3_ref(i))) error stop
end do
if (bessel_jn(-3, x(4)) /= -bessel_jn(3, x(4))) error stop
if (bessel_j0(0.0_qp*one) /= 1.0_qp) error stop
print *, "ok"

contains

    logical function close(a, b)
        real(qp), intent(in) :: a, b
        close = abs(a - b) <= tol*abs(b)
    end function
end program
