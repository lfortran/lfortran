program gpu_metal_241
! A matmul nested inside an array constructor, written directly in the
! kernel body. It has to be hoisted into its own temporary so it is
! lowered to explicit loops; otherwise it survives into the shader as a
! call to the host runtime helper _lcompilers_matmul_*, which does not
! exist on the device.
implicit none
integer, parameter :: n = 4
real :: a(5,n), v(2), m(2,2), t(2)
integer :: j
v = [10.0, 20.0]
m = reshape([1.0, 0.0, 0.0, 1.0], [2,2])
t = [3.0, 4.0]
a = 0.0
do concurrent (j = 1:n)
    a(:,j) = [0.0, matmul(m, v), t]
end do
do j = 1, n
    if (abs(a(1,j) - 0.0) > 1.0e-6) error stop "leading scalar wrong"
    if (abs(a(2,j) - 10.0) > 1.0e-6) error stop "matmul segment wrong"
    if (abs(a(3,j) - 20.0) > 1.0e-6) error stop "matmul segment wrong"
    if (abs(a(4,j) - 3.0) > 1.0e-6) error stop "trailing segment wrong"
    if (abs(a(5,j) - 4.0) > 1.0e-6) error stop "trailing segment wrong"
end do
print *, a(:,1)
print *, "ok"
end program
