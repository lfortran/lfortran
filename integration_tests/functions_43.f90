module functions_43_mod
  implicit none
  type string_t
    character(len=:), allocatable :: string_
  end type string_t
contains

  elemental function bracket(x) result(res)
    class(string_t), intent(in) :: x
    type(string_t) :: res

    res%string_ = "0"
    res%string_ = x%string_
  end function bracket

end module functions_43_mod


program functions_43
  use functions_43_mod
  implicit none

  type(string_t) :: lines(3)

  lines(1)%string_ = "1"
  lines(2)%string_ = "2"
  lines(3)%string_ = "3"

  lines = bracket(lines)

  if (lines(1)%string_ /= "1") error stop
  if (lines(2)%string_ /= "2") error stop
  if (lines(3)%string_ /= "3") error stop
end program functions_43
