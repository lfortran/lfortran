module module_operator_overloading_27
  implicit none

  type :: result_t
    logical :: passed = .false.
  end type

  interface operator(.check.)
    module procedure check
  end interface

  interface operator(.and.)
    module procedure and_op
  end interface

contains

  elemental function check(actual, expected) result(res)
    integer, intent(in) :: actual, expected
    type(result_t) :: res
    res%passed = (actual == expected)
  end function

  elemental function and_op(lhs, rhs) result(res)
    type(result_t), intent(in) :: lhs, rhs
    type(result_t) :: res
    res%passed = lhs%passed .and. rhs%passed
  end function

end module

program operator_overloading_27
  use module_operator_overloading_27
  implicit none
  type(result_t) :: diagnosis(3)
  type(result_t) :: scalar_res

  ! Test: user-defined .and. on arrays returned by user-defined .check.
  diagnosis = (2 .check. [2,2,2]) .and. ([0,1,2] .check. [0,1,2])
  if (.not. all(diagnosis%passed)) error stop

  ! Test: user-defined .and. on scalars
  scalar_res = (3 .check. 3) .and. (4 .check. 4)
  if (.not. scalar_res%passed) error stop

  ! Test: one side fails
  scalar_res = (3 .check. 3) .and. (4 .check. 5)
  if (scalar_res%passed) error stop

  ! Test: arrays with partial mismatch
  diagnosis = (1 .check. [1,2,1]) .and. ([3,3,3] .check. [3,3,3])
  if (diagnosis(1)%passed .neqv. .true.) error stop
  if (diagnosis(2)%passed .neqv. .false.) error stop
  if (diagnosis(3)%passed .neqv. .true.) error stop

  print *, "All tests passed."
end program
