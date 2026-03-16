module module_operator_overloading_28
  implicit none
  private
  public :: string_t

  type string_t
    character(len=:), allocatable :: s
  contains
    generic :: operator(==) => string_eq_string
    procedure, private :: string_eq_string
    procedure :: bracket
  end type

contains

  elemental function string_eq_string(lhs, rhs) result(res)
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in) :: rhs
    logical :: res
    res = lhs%s == rhs%s
  end function

  elemental function bracket(self) result(res)
    class(string_t), intent(in) :: self
    type(string_t) :: res
    res%s = "[" // self%s // "]"
  end function

end module

program operator_overloading_28
  use module_operator_overloading_28, only : string_t
  implicit none

  type(string_t) :: arr(2), expected(2)
  arr(1)%s = "ab"
  arr(2)%s = "cd"
  expected(1)%s = "[ab]"
  expected(2)%s = "[cd]"
  if (.not. all(arr%bracket() == expected)) error stop
  print *, "PASS"
end program
