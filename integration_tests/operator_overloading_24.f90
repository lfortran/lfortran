module operator_overloading_24_mod
  implicit none

  type :: string_t
    character(len=:), allocatable :: s
  contains
    procedure, private :: string_t_eq_string_t
    procedure, private :: string_t_eq_character
    procedure, private, pass(rhs) :: character_eq_string_t
    generic :: operator(==) => string_t_eq_string_t, string_t_eq_character, character_eq_string_t
  end type

contains

  elemental logical function string_t_eq_string_t(lhs, rhs)
    class(string_t), intent(in) :: lhs, rhs
    string_t_eq_string_t = lhs%s == rhs%s
  end function

  elemental logical function string_t_eq_character(lhs, rhs)
    class(string_t), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    string_t_eq_character = lhs%s == rhs
  end function

  elemental logical function character_eq_string_t(lhs, rhs)
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in) :: rhs
    character_eq_string_t = lhs == rhs%s
  end function

end module

program operator_overloading_24
  use operator_overloading_24_mod, only: string_t
  implicit none

  ! string_t == string_t
  if (.not. (string_t("abc") == string_t("abc"))) error stop
  if (string_t("abc") == string_t("xyz")) error stop

  ! string_t == character
  if (.not. (string_t("abc") == "abc")) error stop
  if (string_t("abc") == "xyz") error stop

  ! character == string_t (pass(rhs))
  if (.not. ("abc" == string_t("abc"))) error stop
  if ("abc" == string_t("xyz")) error stop

  print *, "All tests passed."

end program
