module custom_operator_10_mod
  implicit none

  type string_t
    private
    character(len=:), allocatable :: string_
  contains
    generic :: operator(//) => character_cat_string_t
    procedure, private, pass(rhs) :: character_cat_string_t
  end type

  interface
    elemental module function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      character(len=*), intent(in) :: lhs
      class(string_t), intent(in) :: rhs
      character(len=5) lhs_cat_rhs
    end function
  end interface

  type test_diagnosis_t
    private
    character(len=:), allocatable :: diagnostics_string_
  end type

  interface operator(//)
    elemental module function append_string_if_test_failed(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(test_diagnosis_t), intent(in) :: lhs
      type(string_t), intent(in) :: rhs
      character(len=5) lhs_cat_rhs
    end function
  end interface

contains

  module procedure append_string_if_test_failed
    lhs_cat_rhs = lhs%diagnostics_string_ // rhs
  end procedure

  module procedure character_cat_string_t
    lhs_cat_rhs = lhs // rhs%string_
  end procedure
end module custom_operator_10_mod

program custom_operator_10
  use custom_operator_10_mod
  implicit none

end program custom_operator_10
