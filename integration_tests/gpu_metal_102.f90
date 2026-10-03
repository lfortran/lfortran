module gpu_metal_102_m
implicit none
contains
pure function double_it(x) result(y)
    real, intent(in) :: x
    real, allocatable :: y(:)
    allocate(y(1))
    y(1) = x * 2.0
end function
end module

program gpu_metal_102
! Test: function returning allocatable array assigned to a fixed-size
! local array inside do concurrent. Verifies that the Metal codegen
! emits element-wise copy when a local alloc result is assigned to a
! non-allocatable fixed-size array (C-style arrays are not assignable).
use gpu_metal_102_m
implicit none
real :: results(4)
integer :: i

do concurrent (i = 1:4)
  block
    real :: tmp(1)
    tmp = double_it(real(i))
    results(i) = tmp(1)
  end block
end do

if (abs(results(1) - 2.0) > 1e-6) error stop
if (abs(results(2) - 4.0) > 1e-6) error stop
if (abs(results(3) - 6.0) > 1e-6) error stop
if (abs(results(4) - 8.0) > 1e-6) error stop
print *, "ok"
end program
