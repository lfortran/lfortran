module custom_operator_07_a
  implicit none

  public :: string_t

  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: operator(//)   => string_t_cat_string_t
    procedure, private :: string_t_cat_string_t
  end type
  
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

end module custom_operator_07_a

module custom_operator_07_b
  use custom_operator_07_a
  implicit none

contains

  subroutine test_concat
    type(string_t) :: str, left, right

    left = string_t("a")
    right = string_t("b")

    str = left // right
  end subroutine test_concat

end module custom_operator_07_b

program custom_operator_07
    use custom_operator_07_b
    implicit none

    call test_concat()

end program custom_operator_07
