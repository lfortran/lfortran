module custom_operator_16_mod
  implicit none

  type :: scalar_t
    integer :: val
  end type

  type :: vector_t
    integer :: val
  contains
    generic :: operator(.x.) => weighted_premultiply
    procedure, pass(self) :: weighted_premultiply
  end type

contains

  pure function weighted_premultiply(s, self) result(res)
    type(scalar_t), intent(in) :: s
    class(vector_t), intent(in) :: self
    integer :: res
    res = s%val * self%val
  end function

end module

program custom_operator_16
  use custom_operator_16_mod
  implicit none

  type(scalar_t) :: f
  type(vector_t) :: v
  integer :: result

  f = scalar_t(3)
  v = vector_t(4)

  ! Operator .x. is defined on vector_t (RHS), not scalar_t (LHS)
  result = f .x. v
  if (result /= 12) error stop

  f = scalar_t(5)
  v = vector_t(7)
  result = f .x. v
  if (result /= 35) error stop

end program
