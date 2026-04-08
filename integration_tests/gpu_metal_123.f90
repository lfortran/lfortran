program gpu_metal_123
! Test: sum() of a local array slice inside do concurrent block
! Regression: Metal shader failed with undeclared _lcompilers_Sum
! and address space mismatch when sum() was inside a block
implicit none
integer, parameter :: n = 3
real :: r(n)
integer :: i

do concurrent (i = 1:n)
    block
        real :: y(n)
        y = real(i)
        r(i) = sum(y(1:n))
    end block
end do

print *, r(1), r(2), r(3)
if (abs(r(1) - 3.0) > 1e-6) error stop
if (abs(r(2) - 6.0) > 1e-6) error stop
if (abs(r(3) - 9.0) > 1e-6) error stop
end program
