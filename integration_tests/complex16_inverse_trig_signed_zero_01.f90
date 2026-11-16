program complex16_inverse_trig_signed_zero_01
! Signed zeros and branch cuts of the complex(16) inverse trigonometric and
! hyperbolic functions, at run time and in constant expressions (#17848)
implicit none
integer, parameter :: qp = selected_real_kind(33), n = 16
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
complex(qp), volatile :: v(n)
real(qp), volatile :: h, zr
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
print *, "ok"

contains

subroutine check(id, got, want)
    integer, intent(in) :: id
    complex(qp), intent(in) :: got, want
    call check_part(id, real(got), real(want))
    call check_part(id, aimag(got), aimag(want))
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
