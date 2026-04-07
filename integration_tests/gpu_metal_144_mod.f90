module gpu_metal_144_constants
  implicit none
  integer, parameter :: offset_val = 10
end module

module gpu_metal_144_m
  implicit none
  interface
    pure module function compute(x) result(y)
      integer, intent(in) :: x
      integer :: y
    end function
  end interface
end module

submodule(gpu_metal_144_m) gpu_metal_144_impl
  use gpu_metal_144_constants, only : offset_val
  implicit none
contains
  pure module function compute(x) result(y)
    integer, intent(in) :: x
    integer :: y
    y = x + offset_val
  end function
end submodule
