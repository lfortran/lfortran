module custom_operator_01_a
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
end module custom_operator_01_a


module custom_operator_01_b
  integer :: negation = 5
end module custom_operator_01_b

program custom_operator_01
  use custom_operator_01_b, only : negation
  use custom_operator_01_a, only : operator(.negation.)
  implicit none

  integer :: res, x
  x = 10
  res = .negation. x

  print *, res
end program custom_operator_01
