! A matmul combined with a scalar in a GPU offloaded loop. The lowering
! of `matmul(a, b) <op> other` indexed the other operand by the loop
! variables unconditionally, which built an array access over a scalar
! and was rejected by the ASR verifier. A scalar operand is the same for
! every element and must be used as it is. Array operands are kept as
! fences.
program gpu_metal_237
implicit none
real :: mm(2,2), x(2,4), c(2), out(2,4), tr(2,4)
integer :: j

mm = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
c = [1.0, 2.0]
do j = 1, 4
    x(:,j) = [10.0, 20.0]
end do

! scalar on the left of the operator
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = 2.0*matmul(mm, x(:,j))
end do
do j = 1, 4
    if (any(out(:,j) /= [140.0, 200.0])) error stop "scalar left"
end do

! scalar on the right of the operator
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = matmul(mm, x(:,j))*3.0
end do
do j = 1, 4
    if (any(out(:,j) /= [210.0, 300.0])) error stop "scalar right"
end do

! scalar added to a vector-by-matrix product
tr = 0.0
do concurrent (j = 1:4)
    tr(:,j) = matmul(x(:,j), mm) + 5.0
end do
do j = 1, 4
    if (any(tr(:,j) /= [55.0, 115.0])) error stop "scalar vecmat"
end do

! fence: an array operand is still indexed
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = matmul(mm, x(:,j)) + c
end do
do j = 1, 4
    if (any(out(:,j) /= [71.0, 102.0])) error stop "array right"
end do

! fence: an array operand on the left of the operator
out = 0.0
do concurrent (j = 1:4)
    out(:,j) = c + matmul(mm, x(:,j))
end do
do j = 1, 4
    if (any(out(:,j) /= [71.0, 102.0])) error stop "array left"
end do

print *, "ok"
end program gpu_metal_237
