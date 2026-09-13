program gpu_metal_243
! Unary minus applied to an array operand inside a DO CONCURRENT
! offloaded to the GPU. The element loop that lowers an array or
! array-section assignment had no case for a unary operator, so the
! negation ended up applied to the array pointer -- `(-(p))[i]` --
! instead of to the loaded element.
implicit none
integer, parameter :: n = 4
real :: a(n), b(n), c(n), d(n)
integer :: ia(n), ib(n)
real :: s, t
integer :: j

a = [1.0, 2.0, 3.0, 4.0]
ia = [1, 2, 3, 4]
b = 0.0
c = 0.0
d = 0.0
ib = 0
s = 5.0
t = 0.0

! unary minus on an array section
do concurrent (j = 1:1)
    b(1:n) = -a(1:n)
end do

! unary minus on a whole array
do concurrent (j = 1:1)
    c = -a
end do

! unary minus mixed into a larger array expression
do concurrent (j = 1:1)
    d(1:n) = -a(1:n) * 2.0
end do

! unary minus on an integer array section
do concurrent (j = 1:1)
    ib(1:n) = -ia(1:n)
end do

! a scalar unary minus, which was never broken -- kept as a fence
do concurrent (j = 1:1)
    t = -s
end do

do j = 1, n
    if (abs(b(j) + real(j)) > 1.0e-6) error stop "section unary minus wrong"
    if (abs(c(j) + real(j)) > 1.0e-6) error stop "whole array unary minus wrong"
    if (abs(d(j) + 2.0*real(j)) > 1.0e-6) error stop "unary minus in expression wrong"
    if (ib(j) /= -j) error stop "integer unary minus wrong"
end do
if (abs(t + 5.0) > 1.0e-6) error stop "scalar unary minus wrong"
print *, b
print *, c
print *, d
print *, ib
print *, t
print *, "ok"
end program gpu_metal_243
