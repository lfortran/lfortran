module functions_42_mod
  implicit none
  type string_t
    character(len=:), allocatable :: string_
  end type string_t
contains

  function bracket(x) result(res)
    class(string_t), intent(in) :: x
    type(string_t) :: res

    res%string_ = "2"
    res%string_ = x%string_
  end function bracket

end module functions_42_mod


program functions_42
  use functions_42_mod
  implicit none

  type(string_t) :: lines

  lines%string_ = "1"

  lines = bracket(lines)

  if (lines%string_ /= "1") error stop
end program functions_42
