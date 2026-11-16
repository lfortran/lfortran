program complex16_tanh_overflow_01
! complex(16) tanh and tan must neither overflow nor cancel for large
! arguments, at run time and in constant folding (#17850)
implicit none
integer, parameter :: qp = selected_real_kind(33)
integer, parameter :: n = 20
complex(qp), parameter :: p(n) = [ &
    tanh((20000.0_qp, 0.5_qp)), & ! issue: no overflow to NaN
    tan((0.5_qp, 100.0_qp)), & ! issue: tiny real part is not cancelled
    tanh((-20000.0_qp, 0.5_qp)), &
    tan((0.5_qp, -100.0_qp)), &
    tanh((40.0_qp, 1.0_qp)), & ! tiny imaginary part
    tanh((20.0_qp, 1.0_qp)), &
    tanh((50.0_qp, -2.0_qp)), &
    tan((1.0_qp, 40.0_qp)), &
    tanh((0.5_qp, 0.3_qp)), & ! moderate arguments
    tanh((3.0_qp, 1.5_qp)), &
    tanh((20.0_qp, 1.5707963267948966_qp)), & ! near a pole of tan(y)
    tan((1.5_qp, 3.0_qp)), &
    tanh((-30.0_qp, 2.5_qp)), &
    tanh((0.0_qp, 0.5_qp)), & ! signed zeros
    tanh((-0.0_qp, 0.5_qp)), &
    tan((0.5_qp, 0.0_qp)), &
    tan((0.5_qp, -0.0_qp)), &
    tanh((20000.0_qp, -0.0_qp)), &
    tanh((-3.0_qp, 0.0_qp)), &
    tan((-0.0_qp, 2.0_qp))]
complex(qp), parameter :: ref(n) = [ &
    (1.00000000000000000000000000000000000e0_qp, 0.0_qp), &
    (2.32901754645078001676763534669742022e-87_qp, 1.00000000000000000000000000000000000e0_qp), &
    (-1.00000000000000000000000000000000000e0_qp, 0.0_qp), &
    (2.32901754645078001676763534669742022e-87_qp, -1.00000000000000000000000000000000000e0_qp), &
    (1.00000000000000000000000000000000002e0_qp, 3.28229344554119291187765317195710251e-35_qp), &
    (1.00000000000000000353587836774237139e0_qp, 7.72603518516115422142802830769975614e-18_qp), &
    (1.00000000000000000000000000000000000e0_qp, 5.63072556277529063804617013931266333e-44_qp), &
    (3.28229344554119291187765317195710251e-35_qp, 1.00000000000000000000000000000000002e0_qp), &
    (4.96197065773507640715499888445071066e-1_qp, 2.38405083338123242763458083322123345e-1_qp), &
    (1.00491971890878310058326588843936821e0_qp, 7.03049224357747027383303165143358408e-4_qp), &
    (1.00000000000000000849670851058317803e0_qp, 3.26805869374236712974316919405044159e-34_qp), &
    (7.03049224357747027383303165143358408e-4_qp, 1.00491971890878310058326588843936821e0_qp), &
    (-9.99999999999999999999999995032218040e-1_qp, -1.67936614633974523758650810657324148e-26_qp), &
    (0.0_qp, 5.46302489843790513255179465780285383e-1_qp), &
    (-0.0_qp, 5.46302489843790513255179465780285383e-1_qp), &
    (5.46302489843790513255179465780285383e-1_qp, 0.0_qp), &
    (5.46302489843790513255179465780285383e-1_qp, -0.0_qp), &
    (1.00000000000000000000000000000000000e0_qp, -0.0_qp), &
    (-9.95054753686730451331880185255488475e-1_qp, 0.0_qp), &
    (-0.0_qp, 9.64027580075816883946413724100923150e-1_qp)]
complex(qp), volatile :: v(n)
complex(qp) :: r(n)
integer :: i
v = [ &
    (20000.0_qp, 0.5_qp), &
    (0.5_qp, 100.0_qp), &
    (-20000.0_qp, 0.5_qp), &
    (0.5_qp, -100.0_qp), &
    (40.0_qp, 1.0_qp), &
    (20.0_qp, 1.0_qp), &
    (50.0_qp, -2.0_qp), &
    (1.0_qp, 40.0_qp), &
    (0.5_qp, 0.3_qp), &
    (3.0_qp, 1.5_qp), &
    (20.0_qp, 1.5707963267948966_qp), &
    (1.5_qp, 3.0_qp), &
    (-30.0_qp, 2.5_qp), &
    (0.0_qp, 0.5_qp), &
    (-0.0_qp, 0.5_qp), &
    (0.5_qp, 0.0_qp), &
    (0.5_qp, -0.0_qp), &
    (20000.0_qp, -0.0_qp), &
    (-3.0_qp, 0.0_qp), &
    (-0.0_qp, 2.0_qp)]
r(1) = tanh(v(1))
r(2) = tan(v(2))
r(3) = tanh(v(3))
r(4) = tan(v(4))
r(5) = tanh(v(5))
r(6) = tanh(v(6))
r(7) = tanh(v(7))
r(8) = tan(v(8))
r(9) = tanh(v(9))
r(10) = tanh(v(10))
r(11) = tanh(v(11))
r(12) = tan(v(12))
r(13) = tanh(v(13))
r(14) = tanh(v(14))
r(15) = tanh(v(15))
r(16) = tan(v(16))
r(17) = tan(v(17))
r(18) = tanh(v(18))
r(19) = tanh(v(19))
r(20) = tan(v(20))
do i = 1, n
    print '(i3,2es46.36e4)', i, r(i)
    call check(r(i), ref(i), i)
    call check(p(i), ref(i), 100 + i)
end do

contains

    subroutine check(z, zref, code)
    complex(qp), intent(in) :: z, zref
    integer, intent(in) :: code
    call check_part(z%re, zref%re, code)
    call check_part(z%im, zref%im, code)
    end subroutine

    subroutine check_part(x, xref, code)
    real(qp), intent(in) :: x, xref
    integer, intent(in) :: code
    real(qp), parameter :: tol = 1e-32_qp
    if (xref == 0) then
        if (x /= 0 .or. sign(1.0_qp, x) /= sign(1.0_qp, xref)) then
            print *, 'wrong zero:', code, x
            error stop 1
        end if
    else if (.not. abs(x - xref) <= tol * abs(xref)) then
        print *, 'wrong value:', code, x, xref
        error stop 2
    end if
    end subroutine

end program
