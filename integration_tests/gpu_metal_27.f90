program gpu_metal_27
! Test all() intrinsic inside do concurrent with GPU offloading
implicit none
real :: a(4, 3), b(4, 3)
logical :: eq(3)
integer :: l

a = 1.0
b = 1.0

do concurrent(l = 1:3)
  eq(l) = all(a(:,l) == b(:,l))
end do

if (.not. (eq(1) .and. eq(2) .and. eq(3))) error stop

! Test with unequal values
b(2, 2) = 2.0

do concurrent(l = 1:3)
  eq(l) = all(a(:,l) == b(:,l))
end do

if (.not. eq(1)) error stop
if (eq(2)) error stop
if (.not. eq(3)) error stop

print *, "PASS"
end program
