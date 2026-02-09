module submodule_17_string_mod
  implicit none

  type :: string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    generic :: operator(//) => character_cat_string_t
    procedure, pass(rhs), private :: character_cat_string_t
  end type

  interface string_t
    module procedure from_characters
  end interface

contains

  elemental function from_characters(string) result(new_string)
    character(len=*), intent(in) :: string
    type(string_t) :: new_string
    new_string%string_ = string
  end function

  pure function as_character(self) result(string)
    class(string_t), intent(in) :: self
    character(len=:), allocatable :: string
    string = self%string_
  end function

  elemental function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in) :: rhs
    type(string_t) :: lhs_cat_rhs
    lhs_cat_rhs = string_t(lhs // rhs%string_)
  end function

end module

module submodule_17_diag_mod
  use submodule_17_string_mod, only: string_t
  implicit none

  type :: test_diagnosis_t
    logical :: test_passed_ = .false.
    character(len=:), allocatable :: diagnostics_string_
  end type

  interface test_diagnosis_t
    elemental module function construct_from_string_t(test_passed, diagnostics_string) result(test_diagnosis)
      logical, intent(in) :: test_passed
      type(string_t), intent(in) :: diagnostics_string
      type(test_diagnosis_t) :: test_diagnosis
    end function

    elemental module function construct_from_character(test_passed, diagnostics_string) result(test_diagnosis)
      logical, intent(in) :: test_passed
      character(len=*), intent(in) :: diagnostics_string
      type(test_diagnosis_t) :: test_diagnosis
    end function
  end interface

  interface operator(//)
    elemental module function append_string_if_test_failed(lhs, rhs) result(lhs_cat_rhs)
      class(test_diagnosis_t), intent(in) :: lhs
      type(string_t), intent(in) :: rhs
      type(test_diagnosis_t) :: lhs_cat_rhs
    end function
  end interface

end module

submodule(submodule_17_diag_mod) submodule_17_diag_sub
  use submodule_17_string_mod, only: string_t
  implicit none
contains
  module procedure append_string_if_test_failed
    if (lhs%test_passed_) then
      lhs_cat_rhs = lhs
    else
      lhs_cat_rhs = test_diagnosis_t(lhs%test_passed_, lhs%diagnostics_string_ // rhs)
    end if
  end procedure

  module procedure construct_from_string_t
    test_diagnosis%test_passed_ = test_passed
    test_diagnosis%diagnostics_string_ = diagnostics_string%string()
  end procedure

  module procedure construct_from_character
    test_diagnosis%test_passed_ = test_passed
    test_diagnosis%diagnostics_string_ = diagnostics_string
  end procedure
end submodule

program submodule_17
  use submodule_17_diag_mod
  use submodule_17_string_mod, only: string_t
  implicit none
  type(test_diagnosis_t) :: d
  type(string_t) :: s

  d%test_passed_ = .false.
  d%diagnostics_string_ = "prefix: "
  s%string_ = "payload"

  d = d // s

  if (d%diagnostics_string_ /= "prefix: payload") error stop

  print *, "OK"
end program
