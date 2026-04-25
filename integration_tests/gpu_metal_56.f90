program gpu_metal_56
  implicit none
  integer :: n(3)
  real :: x(3)
  integer :: i
  n = [1, 2, 3]
  x = 0.0
  associate(m => n)
    do concurrent (i = 1:2)
      associate(k => 1)
        x(m(k)) = 1.0
      end associate
    end do
  end associate
  if (abs(x(1) - 1.0) > 1.0e-6) error stop
  if (abs(x(2) - 0.0) > 1.0e-6) error stop
  if (abs(x(3) - 0.0) > 1.0e-6) error stop
  print *, "PASS"
end program
