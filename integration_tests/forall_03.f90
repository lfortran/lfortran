program forall_03
implicit none
integer :: i, j, k
real :: a(3,3)
integer :: b(2,3,2)

! Test forall with two control variables
a = 0.0
forall(i=1:3, j=1:3) a(i,j) = real(i + j)
if (abs(a(1,1) - 2.0) > 1e-6) error stop
if (abs(a(2,3) - 5.0) > 1e-6) error stop
if (abs(a(3,3) - 6.0) > 1e-6) error stop
print *, a

! Test forall with three control variables
b = 0
forall(i=1:2, j=1:3, k=1:2) b(i,j,k) = i * 100 + j * 10 + k
if (b(1,1,1) /= 111) error stop
if (b(2,3,2) /= 232) error stop
if (b(1,2,2) /= 122) error stop
print *, b

! Test forall with two variables and increment
a = 0.0
forall(i=1:3:2, j=1:3) a(i,j) = real(i * j)
if (abs(a(1,1) - 1.0) > 1e-6) error stop
if (abs(a(3,2) - 6.0) > 1e-6) error stop
if (abs(a(2,1) - 0.0) > 1e-6) error stop
print *, a

end program forall_03
