module custom_operator_09_mod
  implicit none

  type string_t
    character(:), allocatable :: value
  contains
    procedure, private, pass(rhs) :: character_cat_string_t
    generic :: operator(//) => character_cat_string_t

  end type string_t

  contains 

  elemental function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in) :: rhs 
    type(string_t) :: lhs_cat_rhs

    lhs_cat_rhs % value = lhs // rhs % value
  end function character_cat_string_t

end module custom_operator_09_mod

program custom_operator_09
  use custom_operator_09_mod
  implicit none

  type(string_t) :: name_obj, greeting_obj
  character(len=10) :: prefix

  name_obj%value = "world!"
  prefix = "hello, "

  greeting_obj = prefix // name_obj

  print *, "result: ", greeting_obj % value

end program custom_operator_09
