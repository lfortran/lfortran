module gpu_metal_151_dep_m
  implicit none
  integer, parameter :: wp = kind(1.)
end module

module gpu_metal_151_layer_m
  use gpu_metal_151_dep_m, only : wp
  implicit none
end module
