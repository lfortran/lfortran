program gpu_metal_106
! matmul with array section arguments inside do concurrent
implicit none
real :: w(2,2), a(2), c(2)
integer :: i

w(1,1) = 1.0; w(2,1) = 2.0
w(1,2) = 3.0; w(2,2) = 4.0
a = [0.5, 1.5]

do concurrent (i = 1:1)
  c = matmul(w(1:2,1:2), a)
end do

! c(1) = 1.0*0.5 + 3.0*1.5 = 5.0
! c(2) = 2.0*0.5 + 4.0*1.5 = 7.0
if (abs(c(1) - 5.0) > 1.0e-5) error stop
if (abs(c(2) - 7.0) > 1.0e-5) error stop

print *, "PASSED"
end program
