program intrinsics_437
! Test norm2 with dim argument
implicit none

integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)
real(sp) :: a(3, 4), b(3), c(4)
real(dp) :: da(2, 3), db(2), dc(3)

! Test single precision, dim=2 (reduce columns)
a = 1.0_sp
b = norm2(a, dim=2)
print *, b
if (any(abs(b - 2.0_sp) > 1.0e-6_sp)) error stop

! Test single precision, dim=1 (reduce rows)
c = norm2(a, dim=1)
print *, c
if (any(abs(c - sqrt(3.0_sp)) > 1.0e-6_sp)) error stop

! Test double precision, dim=2
da(1, :) = [1.0_dp, 2.0_dp, 3.0_dp]
da(2, :) = [4.0_dp, 5.0_dp, 6.0_dp]
db = norm2(da, dim=2)
print *, db
if (abs(db(1) - sqrt(14.0_dp)) > 1.0e-12_dp) error stop
if (abs(db(2) - sqrt(77.0_dp)) > 1.0e-12_dp) error stop

! Test double precision, dim=1
dc = norm2(da, dim=1)
print *, dc
if (abs(dc(1) - sqrt(17.0_dp)) > 1.0e-12_dp) error stop
if (abs(dc(2) - sqrt(29.0_dp)) > 1.0e-12_dp) error stop
if (abs(dc(3) - sqrt(45.0_dp)) > 1.0e-12_dp) error stop

print *, "All norm2 dim tests passed."
end program
