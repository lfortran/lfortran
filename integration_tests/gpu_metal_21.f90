program gpu_metal_21
! Reduction: sum via do concurrent with reduce clause
implicit none
integer, parameter :: n = 100000
real :: x(n)
real :: total, total_expected
integer :: i

total_expected = 0.0
do i = 1, n
    x(i) = 1.0
    total_expected = total_expected + 1.0
end do

total = 0.0
do concurrent (i = 1:n) reduce(+:total)
    total = total + x(i)
end do

if (abs(total - total_expected) > 1.0e-1) error stop

print *, "PASSED"
end program
