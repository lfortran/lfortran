module custom_operator_02_a
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
end module custom_operator_02_a


module custom_operator_02_b
  integer :: negation = 5
end module custom_operator_02_b

program custom_operator_02
  use custom_operator_02_b
  use custom_operator_02_a
  implicit none

  integer :: res, x
  x = 10
  res = .negation. x

  print *, res
end program custom_operator_02
