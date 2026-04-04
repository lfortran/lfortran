program gpu_metal_79
! Test: multiple do concurrent loops referencing the same derived type
! should not cause Metal struct redefinition errors.
implicit none
type :: val_t
  real :: v
end type
integer :: i
type(val_t) :: arr(4)
real :: r1(4), r2(4)

arr(1)%v = 1.0
arr(2)%v = 2.0
arr(3)%v = 3.0
arr(4)%v = 4.0

do concurrent(i = 1:4)
  r1(i) = arr(i)%v
end do

do concurrent(i = 1:4)
  r2(i) = arr(i)%v * 2.0
end do

if (abs(r1(1) - 1.0) > 1e-6) error stop
if (abs(r1(2) - 2.0) > 1e-6) error stop
if (abs(r1(3) - 3.0) > 1e-6) error stop
if (abs(r1(4) - 4.0) > 1e-6) error stop

if (abs(r2(1) - 2.0) > 1e-6) error stop
if (abs(r2(2) - 4.0) > 1e-6) error stop
if (abs(r2(3) - 6.0) > 1e-6) error stop
if (abs(r2(4) - 8.0) > 1e-6) error stop

print *, "ok"
end program

