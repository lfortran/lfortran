module arrays_125_types
  implicit none

  type :: op_type
    real, allocatable :: val(:,:)
    procedure(partial_val_sub), pass(this), pointer :: &
         get_partial_left_val => null()
  end type

  interface
    pure module subroutine partial_val_sub(this, grad_in, grad_out)
      class(op_type), intent(in) :: this
      real, dimension(:,:), intent(in)  :: grad_in
      real, dimension(:,:), intent(out) :: grad_out
    end subroutine

    module function multiply_op(a, b) result(c)
      type(op_type), intent(in), target :: a, b
      type(op_type), pointer :: c
    end function
  end interface

end module
