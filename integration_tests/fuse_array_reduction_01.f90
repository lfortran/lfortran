! Test fuse_array_reduction pass: maxval, minval, sum over array expressions
program fuse_array_reduction_01
implicit none

integer, parameter :: n = 50
real(8) :: a(n, n), b(n, n)
real(8) :: result_maxval, result_minval, result_sum
real(8) :: expected
integer :: i, j

! Initialize arrays
do j = 1, n
    do i = 1, n
        a(i, j) = real(i + j, 8)
        b(i, j) = real(i * j, 8) * 0.01d0
    end do
end do

! Test 1: maxval(abs(a - b))
result_maxval = maxval(abs(a(1:n, 1:n) - b(1:n, 1:n)))
! max of |a(i,j) - b(i,j)| = max of |i+j - 0.01*i*j|
! At i=1,j=50: |51 - 0.5| = 50.5
! At i=50,j=1: |51 - 0.5| = 50.5
! At i=1,j=1:  |2 - 0.01| = 1.99
! At i=50,j=50: |100 - 25| = 75
expected = 75.0d0
if (abs(result_maxval - expected) > 1.0d-10) error stop

! Test 2: minval(a)
result_minval = minval(a(1:n, 1:n))
expected = 2.0d0  ! a(1,1) = 1+1 = 2
if (abs(result_minval - expected) > 1.0d-10) error stop

! Test 3: sum(b)
result_sum = sum(b(1:n, 1:n))
! sum of 0.01*i*j for i=1..50, j=1..50
! = 0.01 * (sum i=1..50) * (sum j=1..50)
! = 0.01 * (50*51/2) * (50*51/2) = 0.01 * 1275 * 1275 = 16256.25
expected = 16256.25d0
if (abs(result_sum - expected) > 1.0d-10) error stop

! Test 4: maxval of whole array (not section)
result_maxval = maxval(a)
expected = 100.0d0  ! a(50,50) = 50+50 = 100
if (abs(result_maxval - expected) > 1.0d-10) error stop

! Test 5: sum with unary minus
result_sum = sum(-a(1:n, 1:n))
! = -sum(a) = -(sum of i+j for i=1..50, j=1..50)
! = -(50*sum(i=1..50) + 50*sum(j=1..50)) = -(50*1275 + 50*1275) = -127500
expected = -127500.0d0
if (abs(result_sum - expected) > 1.0d-10) error stop

print *, "All fuse_array_reduction tests passed."

end program fuse_array_reduction_01
