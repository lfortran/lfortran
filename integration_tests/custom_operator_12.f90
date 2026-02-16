module custom_operator_12_m
  implicit none

  type :: operands_t
    real :: actual, expected
  end type

  type :: result_t
    logical :: passed
  end type

  interface operator(.approximates.)
    module procedure approximates
  end interface

  interface operator(.within.)
    module procedure within
  end interface

contains

  elemental function approximates(actual, expected) result(operands)
    real, intent(in) :: actual, expected
    type(operands_t) :: operands
    operands%actual = actual
    operands%expected = expected
  end function

  elemental function within(operands, tolerance) result(res)
    type(operands_t), intent(in) :: operands
    real, intent(in) :: tolerance
    type(result_t) :: res
    res%passed = abs(operands%actual - operands%expected) <= tolerance
  end function

end module

program custom_operator_12
  use custom_operator_12_m, only: operator(.approximates.), operator(.within.), result_t
  implicit none
  type(result_t) :: r

  ! Chained user-defined operators with "use ... only:" import
  r = 1. .approximates. 2. .within. 3.
  print *, r%passed
  if (.not. r%passed) error stop

  r = 1. .approximates. 2. .within. 0.5
  print *, r%passed
  if (r%passed) error stop
end program
