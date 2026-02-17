module operator_overloading_26_mod
  implicit none

  type :: result_t
    logical :: passed = .false.
  end type

  interface operator(.and.)
    module procedure combine
  end interface

contains

  elemental function combine(lhs, rhs) result(res)
    type(result_t), intent(in) :: lhs, rhs
    type(result_t) :: res
    res%passed = lhs%passed .and. rhs%passed
  end function

end module

program operator_overloading_26
  use operator_overloading_26_mod
  implicit none
  type(result_t) :: a, b, c, d

  a = result_t(.true.)
  b = result_t(.true.)
  c = result_t(.true.)

  ! Chained overloaded .and. with derived type
  d = a .and. b .and. c
  if (.not. d%passed) error stop

  a = result_t(.false.)
  d = a .and. b .and. c
  if (d%passed) error stop

  print *, "PASSED"
end program
