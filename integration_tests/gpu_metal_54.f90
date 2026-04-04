program gpu_metal_54
! Test ArraySection assignment to allocatable array inside do concurrent
implicit none
real, allocatable :: b(:,:)
integer :: l

allocate(b(2,2))
b = 1.0

do concurrent(l = 1:2)
  b(1:2,l) = 0.0
end do

if (any(abs(b) > 0.01)) error stop
print *, "ok"
end program
