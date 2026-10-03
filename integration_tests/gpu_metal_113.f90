program gpu_metal_113
  implicit none
  real :: w(2,2), v(2), result(2)
  integer :: i

  w(1,1) = 1.0
  w(2,1) = 2.0
  w(1,2) = 3.0
  w(2,2) = 4.0
  v = [1.0, 2.0]
  result = 0.0

  ! matmul(transpose(w), v):
  !   w = [[1,2],[3,4]], transpose(w) = [[1,3],[2,4]]
  !   result(1) = 1*1 + 2*2 = 5
  !   result(2) = 3*1 + 4*2 = 11
  do concurrent (i = 1:1)
    result = matmul(transpose(w), v)
  end do

  if (abs(result(1) - 5.0) > 1e-6) error stop
  if (abs(result(2) - 11.0) > 1e-6) error stop
  print *, result
end program
