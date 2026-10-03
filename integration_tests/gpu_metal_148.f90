program gpu_metal_148
  implicit none
  real :: x(4), y(4)
  integer :: i
  x = [1.0, 2.0, 3.0, 4.0]
  y = 0.0
  do concurrent(i=1:4)
    associate(a => x(i))
      associate(b => a)
        y(i) = b
      end associate
    end associate
  end do
  print *, y
  if (any(abs(y - x) > 1.0e-6)) error stop
end program
