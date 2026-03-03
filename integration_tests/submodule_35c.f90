submodule(submodule_35_mod) submodule_35_impl
  use submodule_35_util_mod, only: tensor_t
  implicit none
contains
  pure module function reshape_tensor(t, flat) result(output)
    type(tensor_t), intent(in) :: t
    real, intent(in) :: flat(:)
    real :: output(t%rows, t%cols)
    output = reshape(flat(1:t%rows*t%cols), [t%rows, t%cols])
  end function reshape_tensor

  module subroutine fill(t)
    type(tensor_t), intent(inout) :: t
    real :: flat(t%rows * t%cols)
    real :: grid(t%rows, t%cols)
    flat = 1.0
    grid = reshape_tensor(t, flat)
    t%rows = int(grid(1, 1))
  end subroutine fill
end submodule submodule_35_impl
