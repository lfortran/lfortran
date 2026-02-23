module custom_operator_13_mod
  implicit none

  type :: string_t
    character(len=:), allocatable :: s
  contains
    procedure :: cat_sc
    procedure, pass(rhs) :: assign_to_char
    generic :: operator(//) => cat_sc
    generic :: assignment(=) => assign_to_char
  end type

contains

  elemental function cat_sc(lhs, rhs) result(res)
    class(string_t), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    type(string_t) :: res
    res%s = lhs%s // rhs
  end function

  pure subroutine assign_to_char(lhs, rhs)
    character(len=:), intent(out), allocatable :: lhs
    class(string_t), intent(in) :: rhs
    lhs = rhs%s
  end subroutine

end module

program custom_operator_13
  use custom_operator_13_mod
  implicit none

  type(string_t) :: x
  character(len=:), allocatable :: result

  x%s = "hello"
  result = x // " world"
  if (result /= "hello world") error stop
  print *, result
end program
