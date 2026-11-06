! The result of matmul in an element-wise expression is computed into the
! assignment target, without a temporary, when the expression cannot read
! the target: `z = matmul(x, w) + b` is `z = matmul(x, w); z = z + b`.
program matmul_13
implicit none
real, allocatable :: x(:), w(:,:), b(:), z(:)
integer :: it
allocate(x(3), w(3,2), b(2), z(2))
x = [1, 2, 3]
w = reshape([1, 2, 3, 4, 5, 6], [3, 2])
b = [10, 20]
do it = 1, 2
    z = matmul(x, w) + b
end do
if (any(z /= [24, 52])) error stop 1
! the target is read by the expression: computed into a temporary
z = matmul(x, w) + z(2:1:-1)
if (any(z /= [66, 56])) error stop 2
print *, z
end program
