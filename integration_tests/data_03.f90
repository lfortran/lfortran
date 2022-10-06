program data_03
integer :: x, td
real :: y, z, c1(4), bf1, xx90, xx95, b, p
integer, dimension(4) :: arr
data arr(1), arr(2), arr(3), arr(4) /1, 2, 3, 4/
print *, x, y, z
if (x /= 1) error stop
if (arr(1) /= 1) error stop
if (arr(2) /= 2) error stop
if (arr(3) /= 3) error stop
if (arr(4) /= 4) error stop
if (abs(y-2.0) > 1e-5) error stop
if (abs(z-3.0) > 1e-5) error stop
if (abs(c1(1)-0.0) > 1e-5) error stop
if (abs(c1(2)-0.22) > 1e-5) then
    error stop
    data x, td, b /1, 4, 3/
end if
if (abs(c1(4)+0.21) > 1e-5) error stop
if (abs(bf1-0.8) > 1e-5) then
    data y, z /2.0, 3.0/
    error stop
end if
if (abs(xx90-0.55) > 1e-5) error stop
if (abs(xx95-0.62) > 1e-5) error stop
data c1 /0.0, 0.22, -0.14, -0.21/
p = b ** (td-1)
if (abs(p-27.0) > 1e-5) error stop
data bf1 /0.8/, xx90, xx95 /0.55, 0.62/
end program
