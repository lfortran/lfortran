program real16_intrinsics_rounding_01
! exp, sinh, cosh, tanh, asinh, acosh, atanh and ** of real(16) arguments
! are correctly rounded when folded (as GFortran folds them with MPFR) and
! within 1 ulp at run time (#17825). The references are the correctly
! rounded binary128 values, given as their (low, high) 64-bit words.
implicit none
integer, parameter :: qp = 16, n = 13
real(qp), parameter :: c(n) = [exp(2.5_qp), sinh(0.25_qp), sinh(30.0_qp), &
    cosh(1.0_qp), cosh(4.5_qp), tanh(1.0_qp), tanh(-0.3_qp), &
    asinh(1.0_qp), asinh(80.5_qp), acosh(1.5_qp), atanh(0.75_qp), &
    2.0_qp**0.5_qp, 3.0_qp**1.25_qp]
integer(8), parameter :: w(2*n) = [ &
    -5632632303526589959_8, 4612396126804581899_8, &
    2190566696672857771_8, 4610844534704506896_8, &
    -3897728862169305491_8, 4623286984682422926_8, &
    343044501398460069_8, 4611557407059713877_8, &
    -8869037199747637402_8, 4612926391745659189_8, &
    4996852826703643687_8, 4611270332891877177_8, &
    -6840687279389754781_8, -4612483929491009201_8, &
    7836740484861764771_8, 4611337762717015362_8, &
    -8598262205952025647_8, 4612043593185792467_8, &
    -7400574988502603577_8, 4611383389846262112_8, &
    8450712868853065244_8, 4611389318511142309_8, &
    -3960719080840303979_8, 4611521134203499452_8, &
    -7425791141567649885_8, 4611960206303895201_8]
character(len=60) :: s
real(qp) :: x(15), r(n), ref
integer :: i, nfail

s = '2.5 0.25 30 1 4.5 1 -0.3 1 80.5 1.5 0.75 2 0.5 3 1.25'
read(s, *) x
r = [exp(x(1)), sinh(x(2)), sinh(x(3)), cosh(x(4)), cosh(x(5)), &
    tanh(x(6)), tanh(x(7)), asinh(x(8)), asinh(x(9)), acosh(x(10)), &
    atanh(x(11)), x(12)**x(13), x(14)**x(15)]
nfail = 0
do i = 1, n
    ref = transfer([w(2*i - 1), w(2*i)], 1.0_qp)
    call check(c(i), ref, 0, i, 'folded')
    call check(r(i), ref, 1, i, 'run time')
end do
if (nfail > 0) error stop
print *, "ok"

contains

    subroutine check(v, ref, tol, i, name)
        real(qp), intent(in) :: v, ref
        integer, intent(in) :: tol, i
        character(len=*), intent(in) :: name
        if (abs(v - ref) > tol*spacing(ref)) then
            print *, name, i, (v - ref)/spacing(ref), " ulp"
            nfail = nfail + 1
        end if
    end subroutine
end program
