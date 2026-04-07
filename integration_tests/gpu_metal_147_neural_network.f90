module gpu_metal_147_neural_network
  use gpu_metal_147_activation, only : activation_t
  implicit none
  type neural_network_t
    type(activation_t) :: a
  contains
    procedure :: infer => default_real_infer
  end type
  interface
    elemental module function default_real_infer(self, x) result(y)
      class(neural_network_t), intent(in) :: self
      real, intent(in) :: x
      real y
    end function
  end interface
end module

submodule(gpu_metal_147_neural_network) gpu_metal_147_nn_sub
contains
  module procedure default_real_infer
    y = self%a%evaluate(x)
  end procedure
end submodule
