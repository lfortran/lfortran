program arrays_07
implicit none
integer, dimension(10) :: x
integer, dimension(3, 3) :: y
integer :: i, j
integer :: start, end, step

do i = 1, 3
    do j = 1, 3
        y(i, j) = i + j
    end do
end do
x = [(i, i = 1, 10)]

start = x(4)
end = x(7)
step = x(1)
print *, x(start:end:step)
if( sum(x(start:end:step)) /= 22 ) error stop ! TODO: Check with any

print *, x
if( any(x /= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) ) error stop

print *, x(1:5)
if( any(x(1:5) /= [1, 2, 3, 4, 5]) ) error stop

print *, x(3:)
if( any(x(3:) /= [3, 4, 5, 6, 7, 8, 9, 10]) ) error stop

print *, x(:5)
if( any(x(:5) /= [1, 2, 3, 4, 5]) ) error stop

print *, x(5)
if( x(5) /= 5 ) error stop

print *, x(3:5:2)
if( any(x(3:5:2) /= [3, 5]) ) error stop

print *, x(:5:2)
if( any(x(:5:2) /= [1, 3, 5]) ) error stop

print *, x(3::2)
if( any(x(3::2) /= [3, 5, 7, 9]) ) error stop

print *, x(::2)
if( any(x(::2) /= [1, 3, 5, 7, 9]) ) error stop

print *, sum(y)
if( sum(y) /= 36 ) error stop

print *, sum(y(:, 3))
if( sum(y(:, 3)) /= 15 ) error stop

print *, sum(y(2:, :))
if( sum(y(2:, :)) /= 27 ) error stop

print *, sum(y(4:, 3:))
if( sum(y(4:, 3:)) /= 0 ) error stop

print *, sum(y(:2, 2:))
if( sum(y(:2, 2:)) /= 16 ) error stop

print *, sum(y(1:2:2, 1:2:1))
if( sum(y(1:2:2, 1:2:1)) /= 5 ) error stop

print *, sum(y(:3:2, 3))
if( sum(y(:3:2, 3)) /= 10 ) error stop

print *, sum(y(3, 3::2))
if( sum(y(3, 3::2)) /= 6 ) error stop

print *, sum(y(3::2, :3:2))
if( sum(y(3::2, :3:2)) /= 10 ) error stop

print *, sum(y(::2, ::4))
if( sum(y(::2, ::4)) /= 6 ) error stop

print *, sum(y(::2, 3))
if( sum(y(::2, 3)) /= 10 ) error stop

end program
