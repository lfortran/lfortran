module module_operator_overloading_12
  implicit none

  public :: operator(.and.)

  interface operator(.and.)
    elemental module function and(lhs, rhs) result(diagnosis)
      implicit none
      integer, intent(in) :: lhs, rhs
      integer :: diagnosis
    end function
  end interface
contains

  elemental module function and(lhs, rhs) result(diagnosis)
    implicit none
    integer, intent(in) :: lhs, rhs
    integer :: diagnosis

    diagnosis = lhs + rhs
  end function and

end module module_operator_overloading_12

program operator_overloading_12
  use module_operator_overloading_12, only: operator(.and.)
  implicit none

  integer :: a, b, c

  a = 5
  b = 5
  c = a .and. b
  print *, "The result of a .and. b is:", c
  if ( c /= 10 ) error stop
end program operator_overloading_12