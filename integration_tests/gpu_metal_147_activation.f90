module gpu_metal_147_activation
  implicit none
  type activation_t
  contains
    procedure :: evaluate => default_real_evaluate
  end type
contains
  elemental function default_real_evaluate(self, x) result(y)
    class(activation_t), intent(in) :: self
    real, intent(in) :: x
    real y
    y = x
  end function
end module
