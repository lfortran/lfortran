program gpu_metal_50
! Test: associate block inside do concurrent (bare, not wrapped in block)
implicit none
integer :: i, n
real :: x(5)
n = 5
do concurrent (i = 1:n)
    associate(y => real(i))
        x(i) = y
    end associate
end do
if (abs(sum(x) - 15.0) > 1.0e-6) error stop
print *, "PASS"
end program
