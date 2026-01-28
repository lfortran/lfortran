program implicit_interface_28
  implicit none
  intrinsic sqrt
  real :: result
  result = find(sqrt, 9.0)
  print *, result
  if (abs(result - 3.0) > 1.0e-6) error stop
contains
  real function find(func, x) 
    real, external :: func
    real, intent(in) :: x
    find = func(x)
  end function find
end program implicit_interface_28
