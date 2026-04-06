module gpu_metal_136_m
implicit none
type :: point
  real :: x
  real :: y
end type
interface
  pure module function make_point(px, py) result(res)
    real, intent(in) :: px, py
    type(point) :: res
  end function
end interface
end module

submodule(gpu_metal_136_m) gpu_metal_136_sub
implicit none
contains
module procedure make_point
  res%x = px
  res%y = py
end procedure
end submodule
