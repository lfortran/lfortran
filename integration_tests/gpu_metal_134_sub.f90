submodule(gpu_metal_134_m) gpu_metal_134_sub
implicit none
contains
module procedure relu
  y = max(0.0, x)
end procedure
end submodule
