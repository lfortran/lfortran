submodule (arrays_125_types) arrays_125_iface_sub
  implicit none
contains

  pure module subroutine partial_val_sub(this, grad_in, grad_out)
    class(op_type), intent(in)  :: this
    real, dimension(:,:), intent(in)  :: grad_in
    real, dimension(:,:), intent(out) :: grad_out
    grad_out = grad_in
  end subroutine

end submodule
