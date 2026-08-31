! Companion to gpu_metal_204: the same "decide before mutating" rule has
! to hold for every destructive helper the gpu_offload pass runs on a
! `do concurrent` body. Each loop below is double precision (or logical
! derived from double precision), so Metal declines all of them and they
! must run correctly on the host.
program gpu_metal_205
implicit none
double precision :: a(2,2), c(2,2), r(2,2,2), t(2,2,2), b(2,2), s(2)
logical :: f(2)
integer :: i, k

a = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0], [2, 2])
c = reshape([1.0d0, 0.0d0, 0.0d0, 1.0d0], [2, 2])

! matmul
r = 0.0d0
do concurrent (k = 1:2)
    r(:,:,k) = matmul(a, c)
end do
print *, r(1,1,1), r(2,2,2)
if (abs(r(1,1,1) - 1.0d0) > 1.0d-12) error stop "matmul 1"
if (abs(r(2,2,2) - 4.0d0) > 1.0d-12) error stop "matmul 2"

! sum
s = 0.0d0
do concurrent (i = 1:2)
    s(i) = sum(a(i,:))
end do
print *, s(1), s(2)
if (abs(s(1) - 4.0d0) > 1.0d-12) error stop "sum 1"
if (abs(s(2) - 6.0d0) > 1.0d-12) error stop "sum 2"

! transpose
t = 0.0d0
do concurrent (k = 1:2)
    t(:,:,k) = transpose(a)
end do
print *, t(1,2,1), t(2,1,2)
if (abs(t(1,2,1) - 2.0d0) > 1.0d-12) error stop "transpose 1"
if (abs(t(2,1,2) - 3.0d0) > 1.0d-12) error stop "transpose 2"

! elemental whole-section assignment
b = 0.0d0
do concurrent (i = 1:2)
    b(:,i) = abs(a(:,i))
end do
print *, b(1,1), b(1,2)
if (abs(b(1,1) - 1.0d0) > 1.0d-12) error stop "elemental 1"
if (abs(b(1,2) - 3.0d0) > 1.0d-12) error stop "elemental 2"

! all
f = .false.
do concurrent (i = 1:2)
    f(i) = all(a(i,:) > 0.0d0)
end do
print *, f(1), f(2)
if (.not. f(1)) error stop "all 1"
if (.not. f(2)) error stop "all 2"

print *, "ok"
end program
