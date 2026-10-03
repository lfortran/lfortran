module gpu_metal_62_m
  implicit none

  type :: network_t(k)
    integer, kind :: k = 4
    real(k) :: w
  contains
    generic :: infer => compute
    procedure, private :: compute
  end type
contains
  elemental real function compute(self, x)
    class(network_t), intent(in) :: self
    real, intent(in) :: x
    compute = self%w * x
  end function
end module

program gpu_metal_62
  use gpu_metal_62_m
  implicit none
  type(network_t) :: net
  real :: x(10), y(10)
  integer :: i

  net%w = 2.0
  x = 1.0
  do concurrent(i=1:10)
    y(i) = net%infer(x(i))
  end do
  if (abs(y(1) - 2.0) > 1e-6) error stop
  if (abs(y(10) - 2.0) > 1e-6) error stop
  print *, "ok"
end program
