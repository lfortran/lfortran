program complex16_inverse_trig_signed_zero_01
! Signed zeros and branch cuts of the complex(16) inverse trigonometric and
! hyperbolic functions, at run time and in constant expressions (#17848),
! plus off-axis values, NaN parts and parts near the overflow and underflow
! thresholds
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
implicit none
integer, parameter :: qp = selected_real_kind(33), n = 16, m = 5
real(qp), parameter :: z0 = 0.0_qp, nz0 = -0.0_qp
real(qp), parameter :: hp = 1.57079632679489661923132169163975140_qp
real(qp), parameter :: pi = 3.14159265358979323846264338327950280_qp
real(qp), parameter :: as5 = 5.23598775598298873077107230546583832e-1_qp
real(qp), parameter :: ac5 = 1.04719755119659774615421446109316766_qp
real(qp), parameter :: ac5m = 2.09439510239319549230842892218633533_qp
real(qp), parameter :: ach2 = 1.31695789692481670862504634730796848_qp
real(qp), parameter :: ash5 = 4.81211825059603447497758913424368471e-1_qp
real(qp), parameter :: ash2 = 1.44363547517881034249327674027310522_qp
real(qp), parameter :: ath5 = 5.49306144334054845697622618461262902e-1_qp
real(qp), parameter :: at5 = 4.63647609000806116214256231461214397e-1_qp
real(qp), parameter :: at2 = 1.10714871779409050301706546017853705_qp
! Inputs on the real and imaginary axes, with both signs of zero
complex(qp), parameter :: c(n) = [ &
    cmplx(0.5_qp, z0, qp), cmplx(0.5_qp, nz0, qp), &
    cmplx(-0.5_qp, z0, qp), cmplx(-0.5_qp, nz0, qp), &
    cmplx(2.0_qp, z0, qp), cmplx(2.0_qp, nz0, qp), &
    cmplx(-2.0_qp, z0, qp), cmplx(-2.0_qp, nz0, qp), &
    cmplx(z0, 0.5_qp, qp), cmplx(nz0, 0.5_qp, qp), &
    cmplx(z0, 2.0_qp, qp), cmplx(nz0, 2.0_qp, qp), &
    cmplx(z0, -2.0_qp, qp), cmplx(nz0, -2.0_qp, qp), &
    cmplx(z0, z0, qp), cmplx(nz0, nz0, qp)]
! Expected values (C99 Annex G)
complex(qp), parameter :: e_asin(n) = [ &
    cmplx(as5, z0, qp), cmplx(as5, nz0, qp), cmplx(-as5, z0, qp), cmplx(-as5, nz0, qp), &
    cmplx(hp, ach2, qp), cmplx(hp, -ach2, qp), cmplx(-hp, ach2, qp), cmplx(-hp, -ach2, qp), &
    cmplx(z0, ash5, qp), cmplx(nz0, ash5, qp), cmplx(z0, ash2, qp), cmplx(nz0, ash2, qp), &
    cmplx(z0, -ash2, qp), cmplx(nz0, -ash2, qp), cmplx(z0, z0, qp), cmplx(nz0, nz0, qp)]
complex(qp), parameter :: e_acos(n) = [ &
    cmplx(ac5, nz0, qp), cmplx(ac5, z0, qp), cmplx(ac5m, nz0, qp), cmplx(ac5m, z0, qp), &
    cmplx(z0, -ach2, qp), cmplx(z0, ach2, qp), cmplx(pi, -ach2, qp), cmplx(pi, ach2, qp), &
    cmplx(hp, -ash5, qp), cmplx(hp, -ash5, qp), cmplx(hp, -ash2, qp), cmplx(hp, -ash2, qp), &
    cmplx(hp, ash2, qp), cmplx(hp, ash2, qp), cmplx(hp, nz0, qp), cmplx(hp, z0, qp)]
complex(qp), parameter :: e_atan(n) = [ &
    cmplx(at5, z0, qp), cmplx(at5, nz0, qp), cmplx(-at5, z0, qp), cmplx(-at5, nz0, qp), &
    cmplx(at2, z0, qp), cmplx(at2, nz0, qp), cmplx(-at2, z0, qp), cmplx(-at2, nz0, qp), &
    cmplx(z0, ath5, qp), cmplx(nz0, ath5, qp), cmplx(hp, ath5, qp), cmplx(-hp, ath5, qp), &
    cmplx(hp, -ath5, qp), cmplx(-hp, -ath5, qp), cmplx(z0, z0, qp), cmplx(nz0, nz0, qp)]
complex(qp), parameter :: e_asinh(n) = [ &
    cmplx(ash5, z0, qp), cmplx(ash5, nz0, qp), cmplx(-ash5, z0, qp), cmplx(-ash5, nz0, qp), &
    cmplx(ash2, z0, qp), cmplx(ash2, nz0, qp), cmplx(-ash2, z0, qp), cmplx(-ash2, nz0, qp), &
    cmplx(z0, as5, qp), cmplx(nz0, as5, qp), cmplx(ach2, hp, qp), cmplx(-ach2, hp, qp), &
    cmplx(ach2, -hp, qp), cmplx(-ach2, -hp, qp), cmplx(z0, z0, qp), cmplx(nz0, nz0, qp)]
complex(qp), parameter :: e_acosh(n) = [ &
    cmplx(z0, ac5, qp), cmplx(z0, -ac5, qp), cmplx(z0, ac5m, qp), cmplx(z0, -ac5m, qp), &
    cmplx(ach2, z0, qp), cmplx(ach2, nz0, qp), cmplx(ach2, pi, qp), cmplx(ach2, -pi, qp), &
    cmplx(ash5, hp, qp), cmplx(ash5, hp, qp), cmplx(ash2, hp, qp), cmplx(ash2, hp, qp), &
    cmplx(ash2, -hp, qp), cmplx(ash2, -hp, qp), cmplx(z0, hp, qp), cmplx(z0, -hp, qp)]
complex(qp), parameter :: e_atanh(n) = [ &
    cmplx(ath5, z0, qp), cmplx(ath5, nz0, qp), cmplx(-ath5, z0, qp), cmplx(-ath5, nz0, qp), &
    cmplx(ath5, hp, qp), cmplx(ath5, -hp, qp), cmplx(-ath5, hp, qp), cmplx(-ath5, -hp, qp), &
    cmplx(z0, at5, qp), cmplx(nz0, at5, qp), cmplx(z0, at2, qp), cmplx(nz0, at2, qp), &
    cmplx(z0, -at2, qp), cmplx(nz0, -at2, qp), cmplx(z0, z0, qp), cmplx(nz0, nz0, qp)]
! The same functions folded at compile time
complex(qp), parameter :: f_asin(n) = asin(c), f_acos(n) = acos(c), &
    f_atan(n) = atan(c), f_asinh(n) = asinh(c), f_acosh(n) = acosh(c), &
    f_atanh(n) = atanh(c)
! The cases of the issue
complex(qp), parameter :: p_asin = asin((0.5_qp, 0.0_qp))
complex(qp), parameter :: p_acos = acos((0.5_qp, 0.0_qp))
! Off-axis inputs in all four quadrants, with the values of GFortran
complex(qp), parameter :: d(m) = [(-1.5_qp, 2.5_qp), (3.0_qp, -0.2_qp), &
    (-0.3_qp, -0.7_qp), (0.3_qp, 0.7_qp), (-0.8_qp, 0.25_qp)]
complex(qp), parameter :: g_asin(m) = [ &
    cmplx(-5.155484357349067133741234245133223111e-1_qp, 1.777662377506644796357567721745535336_qp, qp), &
    cmplx(1.500225001599510632816006126941486295_qp, -1.765390167618382661253807549191827933_qp, qp), &
    cmplx(-2.457320061010797201068675894670284228e-1_qp, -6.703367429877897400855015594175764091e-1_qp, qp), &
    cmplx(2.457320061010797201068675894670284228e-1_qp, 6.703367429877897400855015594175764091e-1_qp, qp), &
    cmplx(-8.458891699600045706160081203006711936e-1_qp, 3.686307094502810230344590884310906339e-1_qp, qp)]
complex(qp), parameter :: g_acos(m) = [ &
    cmplx(2.086344762529803332605445116153073614_qp, -1.777662377506644796357567721745535336_qp, qp), &
    cmplx(7.057132519538598641531556469826511530e-2_qp, 1.765390167618382661253807549191827933_qp, qp), &
    cmplx(1.816528332895976339338189281106779725_qp, 6.703367429877897400855015594175764091e-1_qp, qp), &
    cmplx(1.325064320693816899124454102172723072_qp, -6.703367429877897400855015594175764091e-1_qp, qp), &
    cmplx(2.416685496754901189847329811940422689_qp, -3.686307094502810230344590884310906339e-1_qp, qp)]
complex(qp), parameter :: g_atan(m) = [ &
    cmplx(-1.380543138238714176079527733234534840_qp, 2.925178131625636610981953896292150582e-1_qp, qp), &
    cmplx(1.250241942365043683592085732000611812_qp, -1.993086845800960362683779687403682157e-2_qp, qp), &
    cmplx(-4.800351812028440013437814075747576810e-1_qp, -7.016804321523099395504844503147672238e-1_qp, qp), &
    cmplx(4.800351812028440013437814075747576810e-1_qp, 7.016804321523099395504844503147672238e-1_qp, qp), &
    cmplx(-6.934791184666820580097872088545183072e-1_qp, 1.512975889576045819856674295218881526e-1_qp, qp)]
complex(qp), parameter :: g_asinh(m) = [ &
    cmplx(-1.750148761317514976836080901568210618_qp, 1.003362711733250757819458160949288479_qp, qp), &
    cmplx(1.820340983611094398147185074566014134_qp, -6.317398485736474300540633955483570939e-2_qp, qp), &
    cmplx(-3.855909873181749085530214009120120062e-1_qp, -7.089034409655052743641089714470979338e-1_qp, qp), &
    cmplx(3.855909873181749085530214009120120062e-1_qp, 7.089034409655052743641089714470979338e-1_qp, qp), &
    cmplx(-7.446886438283553109473025173671399647e-1_qp, 1.949814117771695444562564053485906543e-1_qp, qp)]
complex(qp), parameter :: g_acosh(m) = [ &
    cmplx(1.777662377506644796357567721745535336_qp, 2.086344762529803332605445116153073614_qp, qp), &
    cmplx(1.765390167618382661253807549191827933_qp, -7.057132519538598641531556469826511530e-2_qp, qp), &
    cmplx(6.703367429877897400855015594175764091e-1_qp, -1.816528332895976339338189281106779725_qp, qp), &
    cmplx(6.703367429877897400855015594175764091e-1_qp, 1.325064320693816899124454102172723072_qp, qp), &
    cmplx(3.686307094502810230344590884310906339e-1_qp, 2.416685496754901189847329811940422689_qp, qp)]
complex(qp), parameter :: g_atanh(m) = [ &
    cmplx(-1.634816168516660032870078062217641620e-1_qp, 1.276795025021112843608516151327208718_qp, qp), &
    cmplx(3.447102276163274337416202051090901599e-1_qp, -1.545941198410286986247101775218163495_qp, qp), &
    cmplx(-1.998818960296292725416527413307105973e-1_qp, -6.396697661585147636178585351034301842e-1_qp, qp), &
    cmplx(1.998818960296292725416527413307105250e-1_qp, 6.396697661585147636178585351034301842e-1_qp, qp), &
    cmplx(-8.681430594545043787397098332818091023e-1_qp, 5.170307041144317016985186893810960673e-1_qp, qp)]
complex(qp), parameter :: h_asin(m) = asin(d), h_acos(m) = acos(d), &
    h_atan(m) = atan(d), h_asinh(m) = asinh(d), h_acosh(m) = acosh(d), &
    h_atanh(m) = atanh(d)
! x = 1 with a tiny y, where 4 / y**2 overflows
real(qp), parameter :: tny = 1.0e-2465_qp
real(qp), parameter :: q4 = 7.853981633974483096156608458198756994e-1_qp
real(qp), parameter :: r1 = 2.838282700705441278206883083959208066e3_qp
complex(qp), parameter :: p_atanh1 = atanh(cmplx(1.0_qp, tny, qp))
complex(qp), parameter :: p_atan1 = atan(cmplx(tny, 1.0_qp, qp))
complex(qp), volatile :: v(n), w(m)
real(qp), volatile :: h, zr, one, big, t
real(qp) :: nan, inf
integer :: i

if (sign(1.0_qp, nz0) > 0 .or. sign(1.0_qp, aimag(c(2))) > 0) error stop 1
h = 0.5_qp
zr = 0
if (sign(1.0_qp, aimag(asin(cmplx(h, zr, qp)))) < 0) error stop 2
if (sign(1.0_qp, aimag(acos(cmplx(h, zr, qp)))) > 0) error stop 3
call check(100, p_asin, e_asin(1))
call check(200, p_acos, e_acos(1))
v = c
do i = 1, n
    ! Run time
    call check(1000 + i, asin(v(i)), e_asin(i))
    call check(2000 + i, acos(v(i)), e_acos(i))
    call check(3000 + i, atan(v(i)), e_atan(i))
    call check(4000 + i, asinh(v(i)), e_asinh(i))
    call check(5000 + i, acosh(v(i)), e_acosh(i))
    call check(6000 + i, atanh(v(i)), e_atanh(i))
    ! Compile time
    call check(7000 + i, f_asin(i), e_asin(i))
    call check(8000 + i, f_acos(i), e_acos(i))
    call check(9000 + i, f_atan(i), e_atan(i))
    call check(10000 + i, f_asinh(i), e_asinh(i))
    call check(11000 + i, f_acosh(i), e_acosh(i))
    call check(12000 + i, f_atanh(i), e_atanh(i))
end do

w = d
do i = 1, m
    call check(13000 + i, asin(w(i)), g_asin(i))
    call check(14000 + i, acos(w(i)), g_acos(i))
    call check(15000 + i, atan(w(i)), g_atan(i))
    call check(16000 + i, asinh(w(i)), g_asinh(i))
    call check(17000 + i, acosh(w(i)), g_acosh(i))
    call check(18000 + i, atanh(w(i)), g_atanh(i))
    call check(19000 + i, h_asin(i), g_asin(i))
    call check(20000 + i, h_acos(i), g_acos(i))
    call check(21000 + i, h_atan(i), g_atan(i))
    call check(22000 + i, h_asinh(i), g_asinh(i))
    call check(23000 + i, h_acosh(i), g_acosh(i))
    call check(24000 + i, h_atanh(i), g_atanh(i))
end do

! A NaN with a zero partner is not on an axis: both parts are NaN
nan = zr / zr
call check_nan(25001, asin(cmplx(nan, zr, qp)))
call check_nan(25002, acos(cmplx(nan, zr, qp)))
call check_nan(25003, acosh(cmplx(nan, zr, qp)))
call check_nan(25004, asinh(cmplx(zr, nan, qp)))

! Parts beyond huge / 2 or infinite: atan -> pi/2, atanh -> i pi/2
one = 1
big = huge(1.0_qp)
inf = big * 2
t = 0.5_qp
call check(26001, atan(cmplx(inf, one, qp)), cmplx(hp, z0, qp))
call check(26002, atan(cmplx(big, t, qp)), cmplx(hp, z0, qp))
call check(26003, atanh(cmplx(t, big, qp)), cmplx(z0, hp, qp))
call check(26004, atanh(cmplx(-t, -inf, qp)), cmplx(nz0, -hp, qp))

! x = 1 with a tiny y: a finite real part
t = tny
call check(27001, atanh(cmplx(one, t, qp)), cmplx(r1, q4, qp))
call check(27002, atan(cmplx(t, one, qp)), cmplx(q4, r1, qp))
call check(27003, p_atanh1, cmplx(r1, q4, qp))
call check(27004, p_atan1, cmplx(q4, r1, qp))
print *, "ok"

contains

subroutine check(id, got, want)
    integer, intent(in) :: id
    complex(qp), intent(in) :: got, want
    call check_part(id, real(got), real(want))
    call check_part(id, aimag(got), aimag(want))
end subroutine

subroutine check_nan(id, got)
    integer, intent(in) :: id
    complex(qp), intent(in) :: got
    if (.not. (ieee_is_nan(real(got)) .and. ieee_is_nan(aimag(got)))) then
        print *, id, got
        error stop "expected NaN parts"
    end if
end subroutine

subroutine check_part(id, got, want)
    integer, intent(in) :: id
    real(qp), intent(in) :: got, want
    real(qp), parameter :: tol = 1e-32_qp
    if (sign(1.0_qp, got) /= sign(1.0_qp, want)) then
        print *, id, got, want
        error stop "wrong sign"
    end if
    if (want == 0) then
        if (got /= 0) then
            print *, id, got, want
            error stop "nonzero part"
        end if
    else if (abs(got - want) > tol * abs(want)) then
        print *, id, got, want
        error stop "wrong value"
    end if
end subroutine

end program
