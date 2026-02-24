module custom_operator_15_mod
  implicit none

  type :: vector_t
    double precision :: val
  contains
    generic :: operator(.dot.) => dot_scalar
    procedure, private :: dot_scalar
  end type

  type, extends(vector_t) :: gradient_t
  contains
    generic :: operator(.dot.) => dot_vector_gradient
    procedure, private, pass(grad) :: dot_vector_gradient
  end type

contains

  pure function dot_scalar(self, s) result(res)
    class(vector_t), intent(in) :: self
    double precision, intent(in) :: s
    type(vector_t) :: res
    res%val = self%val * s
  end function

  pure function dot_vector_gradient(v, grad) result(res)
    type(vector_t), intent(in) :: v
    class(gradient_t), intent(in) :: grad
    double precision :: res
    res = v%val * grad%val
  end function

end module

program custom_operator_15
  use custom_operator_15_mod
  implicit none

  type(vector_t) :: v
  type(gradient_t) :: g
  double precision :: result
  type(vector_t) :: result2

  v = vector_t(2.0d0)
  g = gradient_t(3.0d0)

  ! Resolve via RHS type (pass(grad) on the right operand)
  result = v .dot. g
  if (abs(result - 6.0d0) > 1d-10) error stop

  ! Resolve via LHS type (default pass on the left operand)
  result2 = v .dot. 4.0d0
  if (abs(result2%val - 8.0d0) > 1d-10) error stop

  print *, "PASSED"
end program
