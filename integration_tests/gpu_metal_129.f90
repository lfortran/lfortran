program gpu_metal_129
  implicit none
  real :: x(1)
  integer :: i
  do concurrent (i = 1:1)
    x(i) = sum(f())
  end do
  print *, x(1)
  if (abs(x(1) - 2.0) > 1e-6) error stop
contains
  pure function f() result(v)
    real, allocatable :: v(:)
    allocate(v(2))
    v = 1.0
  end function
end program
