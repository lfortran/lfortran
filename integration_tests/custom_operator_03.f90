module custom_operator_03_a
  implicit none
  public :: operator(.negation.)
  integer :: y

  interface operator(.negation.)
    module procedure negative
  end interface

contains

  function negative(i) result(res)
    integer, intent(in) :: i
    integer :: res
    res = -i
  end function negative
end module custom_operator_03_a


module custom_operator_03_b
  integer :: negation = 5
end module custom_operator_03_b

program custom_operator_03
  use custom_operator_03_b, only: negation
  use custom_operator_03_a, temp => y
  implicit none

  integer :: res, x
  x = 10
  res = .negation. x

  print *, res
end program custom_operator_03
