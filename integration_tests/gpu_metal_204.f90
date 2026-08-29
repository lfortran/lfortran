! A `do concurrent` over double precision arrays cannot be offloaded to
! Metal (MSL has no 64-bit floating point type), so the gpu_offload pass
! must decline it and leave the loop exactly as it found it. Here the
! loop body assigns to an array section from a negated array section,
! which the pass used to rewrite before deciding not to offload.
program gpu_metal_204
implicit none
double precision :: a(2,2), b(2,2)
integer :: i

a = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0], [2, 2])
b = 0.0d0

do concurrent (i = 1:2)
    b(i,:) = -a(i,:)
end do

print *, b(1,1), b(2,1), b(1,2), b(2,2)
if (abs(b(1,1) + 1.0d0) > 1.0d-12) error stop "b(1,1)"
if (abs(b(2,1) + 2.0d0) > 1.0d-12) error stop "b(2,1)"
if (abs(b(1,2) + 3.0d0) > 1.0d-12) error stop "b(1,2)"
if (abs(b(2,2) + 4.0d0) > 1.0d-12) error stop "b(2,2)"

b = 0.0d0
do concurrent (i = 1:2)
    b(:,i) = -abs(a(:,i))
end do

print *, b(1,1), b(2,1), b(1,2), b(2,2)
if (abs(b(1,1) + 1.0d0) > 1.0d-12) error stop "n(1,1)"
if (abs(b(2,1) + 2.0d0) > 1.0d-12) error stop "n(2,1)"
if (abs(b(1,2) + 3.0d0) > 1.0d-12) error stop "n(1,2)"
if (abs(b(2,2) + 4.0d0) > 1.0d-12) error stop "n(2,2)"

print *, "ok"
end program
