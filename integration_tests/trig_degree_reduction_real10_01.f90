program trig_degree_reduction_real10_01
use iso_c_binding, only: c_long_double
implicit none
! sind, cosd and tand of x87 real(10) constants reduce their argument
! exactly when folded (#3912). That kind exists only where the C long double
! kind is 10 (Linux x86-64), and kind-10 sind, cosd and tand do not link at
! run time yet, so this checks parameters only.
integer, parameter :: kp = c_long_double
real(kp), parameter :: exact(10) = [sind(30.0_kp), sind(360000030.0_kp), &
    sind(-360000030.0_kp), sind(180.0_kp), cosd(90.0_kp), &
    cosd(360000090.0_kp), cosd(60.0_kp), tand(45.0_kp), &
    tand(360000045.0_kp), tand(-135.0_kp)]
real(kp), parameter :: expected(10) = [0.5_kp, 0.5_kp, -0.5_kp, 0.0_kp, &
    0.0_kp, 0.0_kp, 0.5_kp, 1.0_kp, 1.0_kp, 1.0_kp]
real(kp), parameter :: approx(6) = [sind(10.0_kp), sind(360000010.0_kp), &
    cosd(-3600000010.0_kp), tand(10.0_kp), sind(360000045.0_kp), &
    cosd(1000000000000000090.0_kp)]
real(kp), parameter :: ref(6) = [1.736481776669303488456e-1_kp, &
    1.736481776669303488456e-1_kp, 9.848077530122080593469e-1_kp, &
    1.763269807084649734759e-1_kp, 7.071067811865475243819e-1_kp, &
    9.848077530122080593469e-1_kp]
! spacing(ref): the ulps of the reference values
real(kp), parameter :: tol(6) = [2.0_kp**(-66), 2.0_kp**(-66), &
    2.0_kp**(-64), 2.0_kp**(-66), 2.0_kp**(-64), 2.0_kp**(-64)]
integer :: i
if (kp /= 10) stop 0
do i = 1, size(exact)
    if (exact(i) /= expected(i)) error stop i
end do
! Within one ulp of the correctly rounded value
do i = 1, size(approx)
    if (abs(approx(i) - ref(i)) > tol(i)) error stop 20 + i
end do
print *, "ok"
end program
