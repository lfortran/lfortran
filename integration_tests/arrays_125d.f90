submodule (arrays_125_types) arrays_125_ops_sub
  implicit none
contains

  module function multiply_op(a, b) result(c)
    type(op_type), intent(in), target :: a, b
    type(op_type), pointer :: c

    allocate(c)
    allocate(c%val(size(a%val,1), size(a%val,2)))
    c%val = a%val * b%val
    c%get_partial_left_val => get_partial_mul_left
  end function

  pure subroutine get_partial_mul_left(this, grad_in, grad_out)
    class(op_type), intent(in)  :: this
    real, dimension(:,:), intent(in)  :: grad_in
    real, dimension(:,:), intent(out) :: grad_out
    grad_out = grad_in
  end subroutine

end submodule
