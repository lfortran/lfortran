module gpu_metal_151_network_m
  implicit none
  type network_t
    real :: w = 1.0
  contains
    procedure :: infer => default_infer
  end type
  interface
    elemental module function default_infer(self, x) result(y)
      class(network_t), intent(in) :: self
      real, intent(in) :: x
      real :: y
    end function
  end interface
end module

submodule (gpu_metal_151_network_m) gpu_metal_151_network_impl
  use gpu_metal_151_layer_m
contains
  elemental module function default_infer(self, x) result(y)
    class(network_t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = self%w * x
  end function
end submodule
