program matmul_11
! matmul of a matrix and a vector, and of two matrices: each result
! column is accumulated in place, one column of the matrix at a time
! (#16085)
implicit none
real :: a(3,2), v(2), r(3), b(2,2), c(3,2)
a = reshape([1., 2., 3., 4., 5., 6.], [3,2])
v = [1., -1.]
b = reshape([1., 0., 2., 1.], [2,2])
r = matmul(a, v)
if (r(1) /= -3. .or. r(2) /= -3. .or. r(3) /= -3.) error stop
c = matmul(a, b)
if (c(1,1) /= 1. .or. c(3,1) /= 3. .or. c(1,2) /= 6. .or. c(3,2) /= 12.) error stop
end program
