program array_op_1
implicit none

integer :: a(4), b(4)
real :: c(4), d(4)
integer :: i, j

i = 0

a = [i + 1, (i + j, j = 1, i + 3), i + 4]

b = [4*a(i + 1), 5*a(i + 2), 6*a(i + 3), 7*a(i + 4)]

! c = [i + 1, (i*j, j = 1, i + 3), i + 4]

c(1) = i + 1
c(2) = i*1
c(3) = i*2
c(4) = i*3

print *, a(1), a(2), a(3), a(4)

print *, b(1), b(2), b(3), b(4)

print *, c(1), c(2), c(3), c(4)

d = a + b + c
print *, d(1), d(2), d(3), d(4)
if( d(1) /= 6.0 ) error stop
if( d(2) /= 6.0 ) error stop
if( d(3) /= 14.0 ) error stop
if( d(4) /= 24.0 ) error stop

d = a - b*c
print *, d(1), d(2), d(3), d(4)
if( d(1) /= -3.0 ) error stop
if( d(2) /= 1.0 ) error stop
if( d(3) /= 2.0 ) error stop
if( d(4) /= 3.0 ) error stop

d = a*b*c
print *, d(1), d(2), d(3), d(4)
if( d(1) /= 4.0 ) error stop
if( d(2) /= 0.0 ) error stop
if( d(3) /= 0.0 ) error stop
if( d(4) /= 0.0 ) error stop

d = (a*b)/(c + 1)
print *, d(1), d(2), d(3), d(4)
if( d(1) /= 2.0 ) error stop
if( d(2) /= 5.0 ) error stop
if( d(3) /= 24.0 ) error stop
if( d(4) /= 63.0 ) error stop

end program
