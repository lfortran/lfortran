! matmul in expression positions the GPU offload pass has to hoist:
! unary minus, an argument of another matmul, an argument of a call, an
! argument of another array intrinsic and an array constructor element.
! Each of these used to survive into the Metal shader as a call to the
! host runtime helper `_lcompilers_matmul*`, which is not defined there.
! The rows that were already lowered correctly are kept as fences.
program gpu_metal_236
implicit none
real :: mm(2,2), x(2,4), c(2), out(2,4), sc(4)
integer :: j

mm = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
c = [1.0, 2.0]
do j = 1, 4
    x(:,j) = [10.0, 20.0]
end do

! fence: matmul is the whole right-hand side
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = matmul(mm, x(:,j))
end do
do j = 1, 4
    if (any(out(:,j) /= [70.0, 100.0])) error stop "whole"
end do

! fence: matmul is a direct operand of a binary operation
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = matmul(mm, x(:,j)) + c
end do
do j = 1, 4
    if (any(out(:,j) /= [71.0, 102.0])) error stop "binop"
end do

! fence: the assignment target is a section
out = 0.0
do concurrent (j = 1:4)
    out(1:2,j) = matmul(mm, x(:,j))
end do
do j = 1, 4
    if (any(out(:,j) /= [70.0, 100.0])) error stop "section"
end do

! matmul under a unary minus
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = -matmul(mm, x(:,j))
end do
do j = 1, 4
    if (any(out(:,j) /= [-70.0, -100.0])) error stop "neg"
end do

! matmul as an argument of another matmul
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = matmul(mm, matmul(mm, x(:,j)))
end do
do j = 1, 4
    if (any(out(:,j) /= [370.0, 540.0])) error stop "nested"
end do

! matmul as an argument of a function call
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = twice(matmul(mm, x(:,j)))
end do
do j = 1, 4
    if (any(out(:,j) /= [140.0, 200.0])) error stop "call"
end do

! matmul as an argument of another array intrinsic
sc = 0.0
do concurrent (j = 1:4)
    sc(j) = sum(matmul(mm, x(:,j)))
end do
do j = 1, 4
    if (sc(j) /= 170.0) error stop "sum"
end do

! matmul as an array constructor element
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = [0.0, sum(matmul(mm, x(:,j)))]
end do
do j = 1, 4
    if (any(out(:,j) /= [0.0, 170.0])) error stop "constructor"
end do

print *, "ok"

contains

    pure function twice(v) result(r)
    real, intent(in) :: v(2)
    real :: r(2)
    r(1) = 2.0*v(1)
    r(2) = 2.0*v(2)
    end function twice

end program gpu_metal_236
