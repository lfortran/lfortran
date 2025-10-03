module functions_44_mod
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

end module functions_44_mod


program functions_44
  use functions_44_mod
  implicit none

  type(string_t) :: lines(5)

  lines(1)%string_ = "1"

  lines(1) = bracket(lines(1))

  if (lines(1)%string_ /= "1") error stop
end program functions_44
