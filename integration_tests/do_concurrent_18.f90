program do_concurrent_18
implicit none
integer :: i, j, k
integer :: a(5), m(3,3)
real :: r(5), s(5)

! Test do concurrent with a mask on the index variable
a = -1
do concurrent (i = 1:5, mod(i,2) == 0)
    a(i) = i
end do
if (a(1) /= -1) error stop
if (a(2) /= 2) error stop
if (a(3) /= -1) error stop
if (a(4) /= 4) error stop
if (a(5) /= -1) error stop
print *, a

! Test do concurrent with a mask reading a different array
r = [1.0, 2.0, 3.0, 4.0, 5.0]
s = -1.0
do concurrent (i = 1:5, r(i) > 3.0)
    s(i) = r(i) * 2.0
end do
if (abs(s(1) + 1.0) > 1e-6) error stop
if (abs(s(3) + 1.0) > 1e-6) error stop
if (abs(s(4) - 8.0) > 1e-6) error stop
if (abs(s(5) - 10.0) > 1e-6) error stop
print *, s

! Test do concurrent with two control variables and a mask
m = 1
do concurrent (i = 1:3, j = 1:3, i /= j)
    m(i,j) = 0
end do
if (m(1,1) /= 1) error stop
if (m(2,2) /= 1) error stop
if (m(3,3) /= 1) error stop
if (m(1,2) /= 0) error stop
if (m(3,1) /= 0) error stop
print *, m

! Test do concurrent with a mask and multiple body statements
a = 0
r = 0.0
do concurrent (i = 1:5, i > 2)
    a(i) = i * 2
    r(i) = real(i) + 0.5
end do
if (a(1) /= 0) error stop
if (a(2) /= 0) error stop
if (a(3) /= 6) error stop
if (abs(r(2)) > 1e-6) error stop
if (abs(r(5) - 5.5) > 1e-6) error stop
print *, a
print *, r

! Test do concurrent with a mask and a local variable
a = 0
do concurrent (i = 1:5, mod(i,2) == 1) local(k)
    k = i * 3
    a(i) = k
end do
if (a(1) /= 3) error stop
if (a(2) /= 0) error stop
if (a(3) /= 9) error stop
if (a(4) /= 0) error stop
if (a(5) /= 15) error stop
print *, a

! Test a mask that is never true
a = 4
do concurrent (i = 1:5, i > 100)
    a(i) = -7
end do
do i = 1, 5
    if (a(i) /= 4) error stop
end do
print *, a

! Test a mask that is always true
a = 4
do concurrent (i = 1:5, i > 0)
    a(i) = i
end do
do i = 1, 5
    if (a(i) /= i) error stop
end do
print *, a

end program do_concurrent_18
