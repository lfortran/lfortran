program trig_degree_reduction_real10_01
use iso_c_binding, only: c_long_double
implicit none
! sind, cosd and tand of x87 real(10) constants reduce their argument
! exactly when folded (#3912). That kind exists only where the C long double
! kind is 10 (Linux x86-64), and kind-10 sind, cosd and tand do not link at
! run time yet, so this checks folded constants only. Each one is checked as
! a scalar: real(10) array constructors are not used here.
integer, parameter :: kp = c_long_double
if (kp /= 10) stop 0
call exact(sind(30.0_kp), 0.5_kp, 1)
call exact(sind(360000030.0_kp), 0.5_kp, 2)
call exact(sind(-360000030.0_kp), -0.5_kp, 3)
call exact(sind(180.0_kp), 0.0_kp, 4)
call exact(cosd(90.0_kp), 0.0_kp, 5)
call exact(cosd(360000090.0_kp), 0.0_kp, 6)
call exact(cosd(60.0_kp), 0.5_kp, 7)
call exact(tand(45.0_kp), 1.0_kp, 8)
call exact(tand(360000045.0_kp), 1.0_kp, 9)
call exact(tand(-135.0_kp), 1.0_kp, 10)
! Within one ulp (spacing of the reference) of the correctly rounded value
call near(sind(10.0_kp), 1.736481776669303488456e-1_kp, 2.0_kp**(-66), 21)
call near(sind(360000010.0_kp), 1.736481776669303488456e-1_kp, &
    2.0_kp**(-66), 22)
call near(cosd(-3600000010.0_kp), 9.848077530122080593469e-1_kp, &
    2.0_kp**(-64), 23)
call near(tand(10.0_kp), 1.763269807084649734759e-1_kp, 2.0_kp**(-66), 24)
call near(sind(360000045.0_kp), 7.071067811865475243819e-1_kp, &
    2.0_kp**(-64), 25)
call near(cosd(1000000000000000090.0_kp), 9.848077530122080593469e-1_kp, &
    2.0_kp**(-64), 26)
print *, "ok"

contains

    subroutine exact(got, want, code)
        real(kp), intent(in) :: got, want
        integer, intent(in) :: code
        if (got /= want) error stop code
    end subroutine

    subroutine near(got, want, tol, code)
        real(kp), intent(in) :: got, want, tol
        integer, intent(in) :: code
        if (abs(got - want) > tol) error stop code
    end subroutine

end program
