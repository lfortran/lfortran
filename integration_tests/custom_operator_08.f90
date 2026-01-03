module custom_operator_08_a
  implicit none

  public :: string_t
  public :: operator(//)

  type string_t
    character(len=:), allocatable :: string_
  end type

  interface operator(//)
    module procedure string_t_cat_string_t
  end interface

contains

  elemental module function from_characters(string) result(new_string)
    implicit none
    character(len=*), intent(in) :: string
    type(string_t) new_string
    new_string%string_ = string
  end function

  pure module function string_t_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
    implicit none
    class(string_t), intent(in) :: lhs, rhs
    type(string_t) lhs_cat_rhs
    lhs_cat_rhs = string_t(lhs%string_ // rhs%string_)
  end function

end module custom_operator_08_a

module custom_operator_08_b
  use custom_operator_08_a
  implicit none

contains

  subroutine test_concat
    type(string_t) :: str
    str = string_t("a") // string_t("b")
  end subroutine test_concat

end module custom_operator_08_b

program custom_operator_08
    use custom_operator_08_b
    implicit none

    call test_concat()

end program custom_operator_08
