program gpu_metal_138
  implicit none
  integer :: i
  real, allocatable :: y(:)
  do concurrent (i = 1:1)
    y = f()
  end do
  print *, y(1)
  if (abs(y(1) - 1.0) > 1e-6) error stop
contains
  pure function f() result(v)
    real, allocatable :: v(:)
    allocate(v(1))
    v(1) = 1.0
  end function
end program
