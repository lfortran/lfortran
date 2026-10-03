module gpu_metal_142_act_m
implicit none

enum, bind(C)
  enumerator :: gelu = 1
end enum

type, public :: act_t
  integer :: s = gelu
contains
  procedure :: eval
end type

interface
  elemental module function eval(self, x) result(y)
    class(act_t), intent(in) :: self
    real, intent(in) :: x
    real :: y
  end function
end interface

end module gpu_metal_142_act_m

submodule(gpu_metal_142_act_m) gpu_metal_142_act_s
implicit none
contains
module procedure eval
  y = x * 2.0
end procedure
end submodule gpu_metal_142_act_s

module gpu_metal_142_net_m
use gpu_metal_142_act_m, only: act_t
implicit none
type, public :: net_t
  type(act_t) :: a
contains
  procedure :: run
end type
interface
  module subroutine run(self, x, y, n)
    class(net_t), intent(inout) :: self
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: y(n)
  end subroutine
end interface
end module gpu_metal_142_net_m

submodule(gpu_metal_142_net_m) gpu_metal_142_net_s
implicit none
contains
module procedure run
  integer :: i
  do concurrent (i = 1:n)
    y(i) = self%a%eval(x(i))
  end do
end procedure
end submodule gpu_metal_142_net_s
