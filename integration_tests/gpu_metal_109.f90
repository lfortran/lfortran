program gpu_metal_109
  implicit none
  integer :: pair
  real :: w(3,3), a(3), z(3)

  w = 0.5
  a = 1.0

  do concurrent (pair = 1:2)
    block
      z = matmul(w, a)
    end block
  end do

  if (abs(z(1) - 1.5) > 1.0e-6) error stop
  if (abs(z(2) - 1.5) > 1.0e-6) error stop
  if (abs(z(3) - 1.5) > 1.0e-6) error stop
  print *, "ok"
end program
