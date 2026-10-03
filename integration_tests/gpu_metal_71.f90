program gpu_metal_71
! Test: all(abs(array_slice - array_slice) < tol) inside do concurrent
implicit none
real :: a(2,2), b(2,2)
logical :: eq(2)
integer :: i

a = 1.0
b = 1.0

do concurrent (i = 1:2)
  eq(i) = all(abs(a(:,i) - b(:,i)) < 1.0e-6)
end do

if (.not. all(eq)) error stop

! Test with different values
a(1,1) = 2.0

do concurrent (i = 1:2)
  eq(i) = all(abs(a(:,i) - b(:,i)) < 1.0e-6)
end do

if (eq(1)) error stop
if (.not. eq(2)) error stop

print *, "PASS"
end program
