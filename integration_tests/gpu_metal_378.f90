program gpu_metal_378
implicit none
real :: c(6), x(3), neg_res(5)
real :: matrix(2, 3)
integer(8) :: i, n, lb8, ub8
integer(4) :: j, i4
integer(8) :: k, m
real :: total

! Test 1: 1D loop with integer(8) index and bound
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
n = 3_8
do concurrent (i = 1_8:n)
    x(i) = c(i) + 1.0
end do
if (any(x /= [2.0, 3.0, 4.0])) error stop 1

! Test 2: Dynamic bounds from variables
lb8 = 2_8
ub8 = 3_8
do concurrent (i = lb8:ub8)
    x(i) = c(i) + 10.0
end do
if (x(1) /= 2.0) error stop 2
if (x(2) /= 12.0) error stop 3
if (x(3) /= 13.0) error stop 4

! Test 3: Negative bounds
neg_res = 0.0
do concurrent (i = -2_8:2_8)
    neg_res(i + 3_8) = real(i * 2_8)
end do
if (neg_res(1) /= -4.0) error stop 5
if (neg_res(3) /= 0.0) error stop 6
if (neg_res(5) /= 4.0) error stop 7

! Test 4: Multi-dimensional loop nest with mixed integer(4) and integer(8) indices
m = 2_8
matrix = 0.0
do concurrent (j = 1:2, k = 1_8:3_8)
    matrix(j, k) = real(j) + real(k)
end do
if (matrix(1, 1) /= 2.0) error stop 8
if (matrix(2, 1) /= 3.0) error stop 9
if (matrix(1, 2) /= 3.0) error stop 10
if (matrix(2, 2) /= 4.0) error stop 11
if (matrix(1, 3) /= 4.0) error stop 12
if (matrix(2, 3) /= 5.0) error stop 13

! Test 5: Multi-dimensional loop nest with both integer(8) indices
matrix = 0.0
do concurrent (i = 1_8:m, k = 1_8:3_8)
    matrix(i, k) = real(i * 10_8 + k)
end do
if (matrix(1, 1) /= 11.0) error stop 14
if (matrix(2, 1) /= 21.0) error stop 15
if (matrix(1, 2) /= 12.0) error stop 16
if (matrix(2, 2) /= 22.0) error stop 17
if (matrix(1, 3) /= 13.0) error stop 18
if (matrix(2, 3) /= 23.0) error stop 19

! Test 6: Integer(4) index with integer(8) bound
do concurrent (i4 = 1:int(m, 4))
    matrix(i4, 1) = real(i4 * 100)
end do
if (matrix(1, 1) /= 100.0) error stop 20
if (matrix(2, 1) /= 200.0) error stop 21

! Test 7: Reduction with integer(8) loop index
total = 0.0
do concurrent (i = 1_8:3_8) reduce(+:total)
    total = total + x(i)
end do
if (abs(total - 27.0) > 1e-5) error stop 22

end program
