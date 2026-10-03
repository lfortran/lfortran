program gpu_metal_130
! Test: sum() inside a do loop nested within do concurrent
! Regression: Metal shader called undeclared _lcompilers_Sum helper
implicit none
integer :: i, j
real :: a(3), r(1)
a = 1.0
do concurrent (i = 1:1)
    do j = 1, 1
        r(i) = sum(a)
    end do
end do
print *, r(1)
if (abs(r(1) - 3.0) > 1e-5) error stop
end program
