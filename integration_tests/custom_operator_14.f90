module custom_operator_14_mod
  implicit none

  type :: op_t
  contains
    generic :: operator(.x.) => apply_op
    procedure, private :: apply_op
  end type

contains

  pure function apply_op(self, vec) result(res)
    class(op_t), intent(in) :: self
    double precision, intent(in) :: vec(:)
    double precision, allocatable :: res(:)

    allocate(res(size(vec)))
    res = vec + 1d0
  end function

end module custom_operator_14_mod

program custom_operator_14
  use custom_operator_14_mod
  implicit none

  type(op_t) :: op
  double precision :: values(3), result(3)

  values = [1d0, 2d0, 3d0]
  associate(d => op)
    result = d .x. values
  end associate

  if (any(result /= [2d0, 3d0, 4d0])) error stop
  print *, result
end program custom_operator_14
