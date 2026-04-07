program gpu_metal_115
  implicit none
  integer :: i
  real :: r(4)
  r = 0.0
  do concurrent(i = 1:4)
    block
      real :: a(2, 2)
      a(1, 1) = real(i)
      r(i) = a(1, 1)
    end block
  end do
  if (abs(r(1) - 1.0) > 1e-6) error stop
  if (abs(r(2) - 2.0) > 1e-6) error stop
  if (abs(r(3) - 3.0) > 1e-6) error stop
  if (abs(r(4) - 4.0) > 1e-6) error stop
  print *, "ALL PASSED"
end program
