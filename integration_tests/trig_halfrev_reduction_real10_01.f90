program trig_halfrev_reduction_real10_01
use iso_c_binding, only: c_long_double
implicit none
! sinpi, cospi and tanpi of x87 real(10) constants reduce their argument
! exactly when folded (#3912). That kind exists only where the C long double
! kind is 10 (Linux x86-64), so this checks parameters only, as in
! trig_degree_reduction_real10_01.
integer, parameter :: kp = c_long_double
real(kp), parameter :: exact(10) = [sinpi(1.0_kp), sinpi(0.5_kp), &
    sinpi(9223372036854775809.0_kp), sinpi(4611686018427387903.5_kp), &
    cospi(0.5_kp), cospi(1000000.5_kp), cospi(9223372036854775809.0_kp), &
    tanpi(0.25_kp), tanpi(1000000.25_kp), tanpi(-0.75_kp)]
real(kp), parameter :: expected(10) = [0.0_kp, 1.0_kp, 0.0_kp, -1.0_kp, &
    0.0_kp, 0.0_kp, -1.0_kp, 1.0_kp, 1.0_kp, 1.0_kp]
real(kp), parameter :: approx(4) = [sinpi(0.1_kp), sinpi(1048576.25_kp), &
    cospi(0.3_kp), tanpi(0.3_kp)]
real(kp), parameter :: ref(4) = [3.090169943749474241036e-1_kp, &
    7.071067811865475243819e-1_kp, 5.877852522924731291346e-1_kp, &
    1.376381920471173538360_kp]
real(kp), parameter :: pole = tanpi(4611686018427387904.5_kp)
! spacing(ref): the ulps of the reference values
real(kp), parameter :: tol(4) = [2.0_kp**(-65), 2.0_kp**(-64), &
    2.0_kp**(-64), 2.0_kp**(-63)]
integer :: i
if (kp /= 10) stop 0
do i = 1, size(exact)
    if (exact(i) /= expected(i)) error stop i
end do
if (pole <= huge(pole)) error stop 11
! Within one ulp of the correctly rounded value
do i = 1, size(approx)
    if (abs(approx(i) - ref(i)) > tol(i)) error stop 20 + i
end do
print *, "ok"
end program
