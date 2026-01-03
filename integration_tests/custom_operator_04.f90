module custom_operator_04_a
  implicit none
  public :: operator(.negation.)

  integer :: negation = 3

  interface operator(.negation.)
    module procedure negative
  end interface

contains

  function negative(i) result(res)
    integer, intent(in) :: i
    integer :: res
    res = -i
    print *, negation
  end function negative
end module custom_operator_04_a

program custom_operator_04
  use custom_operator_04_a, only: operator(.negation.)
  implicit none

  integer :: res

  res = .negation. 10

  print *, res
end program custom_operator_04
