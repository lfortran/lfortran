! Test: intrinsic // inside associate when user-defined operator(//) is in scope
module associate_34_mod
  implicit none

  type :: string_t
    character(len=:), allocatable :: s
  end type

  interface operator(//)
    elemental module function char_cat_string(lhs, rhs) result(res)
      character(len=*), intent(in) :: lhs
      type(string_t), intent(in) :: rhs
      type(string_t) :: res
    end function
  end interface

end module

submodule (associate_34_mod) associate_34_sub
  implicit none
contains
  elemental module function char_cat_string(lhs, rhs) result(res)
    character(len=*), intent(in) :: lhs
    type(string_t), intent(in) :: rhs
    type(string_t) :: res
    res%s = lhs // rhs%s
  end function
end submodule

program associate_34
  use associate_34_mod
  implicit none
  character(len=:), allocatable :: a
  a = "world"
  associate(str => a)
    if ("hello " // str /= "hello world") error stop
  end associate
end program
