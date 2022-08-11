program minmax_01
integer :: x, y, z
x = 4
y = 5
! Runtime
z = min0(x, y)
if (z /= 4) error stop
z = max0(x, y)
if (z /= 5) error stop

! Compile time
z = min0(4, 5)
if (z /= 4) error stop
z = max0(4, 5)
if (z /= 5) error stop
end program
