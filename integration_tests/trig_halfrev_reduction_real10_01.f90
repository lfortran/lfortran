program trig_halfrev_reduction_real10_01
use iso_c_binding, only: c_long_double
implicit none
! sinpi, cospi and tanpi of x87 real(10) constants reduce their argument
! exactly when folded (#3912). That kind exists only where the C long double
! kind is 10 (Linux x86-64), so this checks folded constants only, as in
! trig_degree_reduction_real10_01.
integer, parameter :: kp = c_long_double
if (kp /= 10) stop 0
call exact(sinpi(1.0_kp), 0.0_kp, 1)
call exact(sinpi(0.5_kp), 1.0_kp, 2)
call exact(sinpi(9223372036854775809.0_kp), 0.0_kp, 3)
call exact(sinpi(4611686018427387903.5_kp), -1.0_kp, 4)
call exact(cospi(0.5_kp), 0.0_kp, 5)
call exact(cospi(1000000.5_kp), 0.0_kp, 6)
call exact(cospi(9223372036854775809.0_kp), -1.0_kp, 7)
call exact(tanpi(0.25_kp), 1.0_kp, 8)
call exact(tanpi(1000000.25_kp), 1.0_kp, 9)
call exact(tanpi(-0.75_kp), 1.0_kp, 10)
call pole(tanpi(4611686018427387904.5_kp), 11)
! Within one ulp (spacing of the reference) of the correctly rounded value
call near(sinpi(0.1_kp), 3.090169943749474241036e-1_kp, 2.0_kp**(-65), 21)
call near(sinpi(1048576.25_kp), 7.071067811865475243819e-1_kp, &
    2.0_kp**(-64), 22)
call near(cospi(0.3_kp), 5.877852522924731291346e-1_kp, 2.0_kp**(-64), 23)
call near(tanpi(0.3_kp), 1.376381920471173538360_kp, 2.0_kp**(-63), 24)
print *, "ok"

contains

    subroutine exact(got, want, code)
        real(kp), intent(in) :: got, want
        integer, intent(in) :: code
        if (got /= want) error stop code
    end subroutine

    subroutine pole(got, code)
        real(kp), intent(in) :: got
        integer, intent(in) :: code
        if (got <= huge(got)) error stop code
    end subroutine

    subroutine near(got, want, tol, code)
        real(kp), intent(in) :: got, want, tol
        integer, intent(in) :: code
        if (abs(got - want) > tol) error stop code
    end subroutine

end program
