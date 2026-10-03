program gpu_metal_145
  implicit none
  real :: x(4), y(4)
  integer :: i
  x = [1.0, 2.0, 3.0, 4.0]
  do concurrent(i = 1:4)
    y(i) = compute(x(i))
  end do
  if (abs(y(1) - 2.0) > 1.0e-6) error stop
  if (abs(y(2) - 3.0) > 1.0e-6) error stop
  if (abs(y(3) - 4.0) > 1.0e-6) error stop
  if (abs(y(4) - 5.0) > 1.0e-6) error stop
  print *, "PASS"
contains
  pure function compute(x) result(r)
    real, intent(in) :: x
    real :: r
    associate(n => 1)
      associate(z => x + real(n))
        r = z
      end associate
    end associate
  end function
end program
