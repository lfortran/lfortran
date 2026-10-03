module gpu_metal_134_m
implicit none
interface
  elemental module function relu(x) result(y)
    real, intent(in) :: x
    real :: y
  end function
end interface
end module

submodule(gpu_metal_134_m) gpu_metal_134_sub
implicit none
contains
module procedure relu
  y = max(0.0, x)
end procedure
end submodule
