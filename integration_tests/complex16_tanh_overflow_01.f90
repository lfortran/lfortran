program complex16_tanh_overflow_01
! complex(16) tanh and tan must neither overflow nor cancel for large
! arguments, at run time and in constant folding (#17850)
implicit none
integer, parameter :: qp = selected_real_kind(33)
integer, parameter :: nh = 17, n = 26
! tanh is applied to z(1:nh) and tan to z(nh+1:n)
complex(qp), parameter :: z(n) = [ &
    (20000.0_qp, 0.5_qp), & ! issue: no overflow to NaN
    (-20000.0_qp, 0.5_qp), &
    (40.0_qp, 1.0_qp), & ! tiny imaginary part
    (20.0_qp, 1.0_qp), &
    (50.0_qp, -2.0_qp), &
    (0.5_qp, 0.3_qp), & ! moderate arguments
    (3.0_qp, 1.5_qp), &
    (20.0_qp, 1.5707963267948966_qp), & ! near a pole of tan(y)
    (-30.0_qp, 2.5_qp), &
    (0.0_qp, 0.5_qp), & ! signed zeros
    (-0.0_qp, 0.5_qp), &
    (20000.0_qp, -0.0_qp), &
    (-3.0_qp, 0.0_qp), &
    (5677.5_qp, 0.5_qp), & ! 5677 < |x| <= 11354, normal imaginary part
    (-5677.5_qp, -0.5_qp), &
    (5677.5_qp, -0.5_qp), &
    (5677.0_qp, 0.5_qp), & ! largest |x| of the sinh(x)**2 formula
    (0.5_qp, 100.0_qp), & ! issue: tiny real part is not cancelled
    (0.5_qp, -100.0_qp), &
    (1.0_qp, 40.0_qp), &
    (1.5_qp, 3.0_qp), &
    (0.5_qp, 0.0_qp), &
    (0.5_qp, -0.0_qp), &
    (-0.0_qp, 2.0_qp), &
    (0.5_qp, 5677.5_qp), &
    (-0.5_qp, -5677.5_qp)]
complex(qp), parameter :: p(n) = [tanh(z(1:nh)), tan(z(nh+1:n))]
complex(qp), parameter :: ref(n) = [ &
    (1.00000000000000000000000000000000000e0_qp, 0.0_qp), &
    (-1.00000000000000000000000000000000000e0_qp, 0.0_qp), &
    (1.00000000000000000000000000000000002e0_qp, 3.28229344554119291187765317195710251e-35_qp), &
    (1.00000000000000000353587836774237139e0_qp, 7.72603518516115422142802830769975614e-18_qp), &
    (1.00000000000000000000000000000000000e0_qp, 5.63072556277529063804617013931266333e-44_qp), &
    (4.96197065773507640715499888445071066e-1_qp, 2.38405083338123242763458083322123345e-1_qp), &
    (1.00491971890878310058326588843936821e0_qp, 7.03049224357747027383303165143358408e-4_qp), &
    (1.00000000000000000849670851058317803e0_qp, 3.26805869374236712974316919405044159e-34_qp), &
    (-9.99999999999999999999999995032218040e-1_qp, -1.67936614633974523758650810657324148e-26_qp), &
    (0.0_qp, 5.46302489843790513255179465780285383e-1_qp), &
    (-0.0_qp, 5.46302489843790513255179465780285383e-1_qp), &
    (1.00000000000000000000000000000000000e0_qp, -0.0_qp), &
    (-9.95054753686730451331880185255488475e-1_qp, 0.0_qp), &
    (1.00000000000000000000000000000000000e0_qp, 6.48973748616036496716197000524742373e-4932_qp), &
    (-1.00000000000000000000000000000000000e0_qp, -6.48973748616036496716197000524742373e-4932_qp), &
    (1.00000000000000000000000000000000000e0_qp, -6.48973748616036496716197000524742373e-4932_qp), &
    (1.00000000000000000000000000000000000e0_qp, 1.76409354800992046558451648906130728e-4931_qp), &
    (2.32901754645078001676763534669742022e-87_qp, 1.00000000000000000000000000000000000e0_qp), &
    (2.32901754645078001676763534669742022e-87_qp, -1.00000000000000000000000000000000000e0_qp), &
    (3.28229344554119291187765317195710251e-35_qp, 1.00000000000000000000000000000000002e0_qp), &
    (7.03049224357747027383303165143358408e-4_qp, 1.00491971890878310058326588843936821e0_qp), &
    (5.46302489843790513255179465780285383e-1_qp, 0.0_qp), &
    (5.46302489843790513255179465780285383e-1_qp, -0.0_qp), &
    (-0.0_qp, 9.64027580075816883946413724100923150e-1_qp), &
    (6.48973748616036496716197000524742373e-4932_qp, 1.00000000000000000000000000000000000e0_qp), &
    (-6.48973748616036496716197000524742373e-4932_qp, -1.00000000000000000000000000000000000e0_qp)]
complex(qp), volatile :: v(n)
real(qp), volatile :: inf
complex(qp) :: r(n)
integer :: i
v = z
r(1:nh) = tanh(v(1:nh))
r(nh+1:n) = tan(v(nh+1:n))
do i = 1, n
    print '(i3,2es46.36e4)', i, r(i)
    call check(r(i), ref(i), i)
    call check(p(i), ref(i), 100 + i)
end do

! infinite arguments (run time only)
inf = huge(1.0_qp)
inf = 2 * inf
call check(tanh(cmplx(inf, 0.5_qp, qp)), (1.0_qp, 0.0_qp), 201)
call check(tanh(cmplx(-inf, 2.0_qp, qp)), (-1.0_qp, -0.0_qp), 202)
call check(tanh(cmplx(-inf, -0.0_qp, qp)), (-1.0_qp, -0.0_qp), 203)
call check(tan(cmplx(0.5_qp, inf, qp)), (0.0_qp, 1.0_qp), 204)
call check(tan(cmplx(-0.5_qp, -inf, qp)), (-0.0_qp, -1.0_qp), 205)

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
