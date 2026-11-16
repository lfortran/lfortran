program intrinsics_520
! Keyword argument x= for the degree trigonometric intrinsics (#17843)
implicit none
real(4), parameter :: s4 = sind(x=30.0)
real(8), parameter :: s8 = sind(x=30d0)
real(4), parameter :: t4 = atand(x=1.0)
real(8), parameter :: t8 = atand(x=-1d0)
real(4) :: x4
real(8) :: x8
if (abs(s4 - 0.5) > 1e-6) error stop 1
if (abs(s8 - 0.5d0) > 1d-12) error stop 2
if (abs(t4 - 45.0) > 1e-4) error stop 3
if (abs(t8 + 45d0) > 1d-12) error stop 4
x4 = 30.0
x8 = 30d0
if (abs(sind(x=x4) - 0.5) > 1e-6) error stop 5
if (abs(sind(x=x8) - 0.5d0) > 1d-12) error stop 6
x4 = 1.0
x8 = 1d0
if (abs(atand(x=x4) - 45.0) > 1e-4) error stop 7
if (abs(atand(x=x8) - 45d0) > 1d-12) error stop 8
x8 = 60d0
if (abs(cosd(x=x8) - 0.5d0) > 1d-12) error stop 9
x8 = 45d0
if (abs(tand(x=x8) - 1d0) > 1d-12) error stop 10
x8 = 0.5d0
if (abs(asind(x=x8) - 30d0) > 1d-12) error stop 11
if (abs(acosd(x=x8) - 60d0) > 1d-12) error stop 12
if (abs(atan2d(y=1d0, x=-1d0) - 135d0) > 1d-12) error stop 13
if (abs(atan2d(x=1d0, y=-1d0) + 45d0) > 1d-12) error stop 14
print *, "ok"
end program
