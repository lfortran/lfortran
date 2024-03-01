program binop_02
use iso_fortran_env, sp=>real32, dp=>real64
real :: a(2)
double precision :: b(4)
integer :: c(3)
complex :: d(2)
a = [2.0, 3.0] + 2.0
print *, sum(a)
if (abs(sum(a) - 9.0_sp) > 1e-8_sp) error stop
b = [2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp] + [6.0_dp, 7.0_dp, 8.0_dp, 9.0_dp]
if (abs(sum(b) - 44.0_dp) > 1e-12_dp) error stop
print *, sum(b)
c = [2, 3, 4] + 2
if (sum(c) /= 15) error stop
print *, sum(c)
d = [(2.0, 3.0), (4.0, 5.0)] + (6.0, 7.0)
print *, abs(sum(d))
if (abs(abs(sum(d)) - 28.4253407_sp) > 1e-8_sp) error stop
end program
