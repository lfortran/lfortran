module gpu_metal_146_m
  implicit none
  interface
    elemental module function outer_compute(x) result(y)
      real, intent(in) :: x
      real :: y
    end function
  end interface
contains
  elemental function inner_eval(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * 2.0
  end function
end module

submodule(gpu_metal_146_m) gpu_metal_146_s
contains
  module procedure outer_compute
    y = inner_eval(x) + 1.0
  end procedure
end submodule

program gpu_metal_146
  use gpu_metal_146_m, only : outer_compute
  implicit none
  real :: x(4), y(4)
  integer :: i
  x = [1.0, 2.0, 3.0, 4.0]
  do concurrent (i = 1:4)
    y(i) = outer_compute(x(i))
  end do
  if (abs(y(1) - 3.0) > 1e-6) error stop
  if (abs(y(2) - 5.0) > 1e-6) error stop
  if (abs(y(3) - 7.0) > 1e-6) error stop
  if (abs(y(4) - 9.0) > 1e-6) error stop
  print *, "PASS"
end program
