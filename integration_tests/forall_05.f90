program forall_05
implicit none
integer :: i, j
integer :: a(5), m(3,3)
real :: r(5), s(5)

! Test forall statement with a mask on the index variable
a = -1
forall(i=1:5, mod(i,2) == 0) a(i) = i
if (a(1) /= -1) error stop
if (a(2) /= 2) error stop
if (a(3) /= -1) error stop
if (a(4) /= 4) error stop
if (a(5) /= -1) error stop
print *, a

! Test forall statement with a mask reading a different array
r = [1.0, 2.0, 3.0, 4.0, 5.0]
s = -1.0
forall(i=1:5, r(i) > 3.0) s(i) = r(i) * 2.0
if (abs(s(1) + 1.0) > 1e-6) error stop
if (abs(s(3) + 1.0) > 1e-6) error stop
if (abs(s(4) - 8.0) > 1e-6) error stop
if (abs(s(5) - 10.0) > 1e-6) error stop
print *, s

! Test forall statement with two control variables and a mask
m = 1
forall(i=1:3, j=1:3, i /= j) m(i,j) = 0
if (m(1,1) /= 1) error stop
if (m(2,2) /= 1) error stop
if (m(3,3) /= 1) error stop
if (m(1,2) /= 0) error stop
if (m(3,1) /= 0) error stop
print *, m

! Test forall statement with a compound mask
a = -1
forall(i=1:5, i > 1 .and. i < 5) a(i) = i * 10
if (a(1) /= -1) error stop
if (a(2) /= 20) error stop
if (a(4) /= 40) error stop
if (a(5) /= -1) error stop
print *, a

! Test forall block with a single statement and a mask
m = 7
forall(i=1:3, j=1:3, i + j > 4)
    m(i,j) = i * j
end forall
if (m(1,1) /= 7) error stop
if (m(2,3) /= 6) error stop
if (m(3,3) /= 9) error stop
if (m(1,3) /= 7) error stop
print *, m

! Test forall block with multiple body statements and a mask
a = 0
r = 0.0
forall(i=1:5, mod(i,2) == 1)
    a(i) = i
    r(i) = real(i) + 0.5
end forall
if (a(1) /= 1) error stop
if (a(2) /= 0) error stop
if (a(5) /= 5) error stop
if (abs(r(3) - 3.5) > 1e-6) error stop
if (abs(r(4)) > 1e-6) error stop
print *, a
print *, r

! Test a mask that is never true
a = 3
forall(i=1:5, i > 100) a(i) = -7
do i = 1, 5
    if (a(i) /= 3) error stop
end do
print *, a

! Test a mask that is always true
a = 3
forall(i=1:5, i > 0) a(i) = i
do i = 1, 5
    if (a(i) /= i) error stop
end do
print *, a

end program forall_05
