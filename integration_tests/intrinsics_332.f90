program intrinsics_332

real, allocatable :: x(:, :), x1(:, :)
real :: y

allocate(x(4, 2), x1(4, 2))
x(:, 1) = -3.0
x(:, 2) = 4.0

y = maxval([sum(abs(x), 2), 0.0_4])
print *, y
if( y /= 7.0 ) error stop

y = maxval([sum(abs(x), 1), 0.0_4])
print *, y
if( y /= 16.0 ) error stop

end program
