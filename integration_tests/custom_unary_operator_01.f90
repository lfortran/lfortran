module custom_unary_operator_01_module
  implicit none
  public :: operator(.negation.)

  interface operator(.negation.)
    module procedure negative
  end interface

contains

  function negative(i) result(res)
    integer, intent(in) :: i
    integer :: res
    res = -i
  end function negative
end module custom_unary_operator_01_module

program custom_unary_operator_01
  use custom_unary_operator_01_module, only : operator(.negation.)
  implicit none
  integer :: res
  integer :: input_int
  input_int = 10
  res = .negation. input_int
  print *, "res: ", res
  print *, "res /= -input_int: ", res /= -input_int
  if (res /= -input_int) error stop
end program custom_unary_operator_01
