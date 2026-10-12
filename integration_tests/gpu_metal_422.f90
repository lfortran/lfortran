program gpu_metal_422
  ! gpu_metal_421 with the submodule compiled separately.
  use gpu_metal_422_m, only: f
  implicit none
  integer :: n(3), r(3), i
  n = [16777217, 16777219, 16777221]
  do concurrent (i = 1:3)
    r(i) = f(n(i))
  end do
  print *, r
  if (any(r /= n)) error stop
end program
