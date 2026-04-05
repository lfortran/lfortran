program gpu_metal_105
! matmul inside a regular do loop nested within do concurrent
implicit none
integer :: l, pair
real :: w(2,2), a(2), c(2)

w(1,1) = 1.0; w(2,1) = 1.0
w(1,2) = 1.0; w(2,2) = 1.0
a = [1.0, 2.0]

do concurrent (pair = 1:1)
  do l = 1, 2
    c = matmul(w, a)
  end do
end do

! c(1) = 1*1 + 1*2 = 3, c(2) = 1*1 + 1*2 = 3
if (abs(c(1) - 3.0) > 1.0e-5) error stop
if (abs(c(2) - 3.0) > 1.0e-5) error stop

print *, "PASSED"
end program
