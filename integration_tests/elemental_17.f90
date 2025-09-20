module elemental_17_mod
  implicit none
  type string_t
    character(len=:), allocatable :: string_
  end type string_t
contains

  elemental function bracket(x) result(res)
    class(string_t), intent(in) :: x
    type(string_t) :: res

    res%string_ = x%string_ // " called func"
  end function bracket

end module elemental_17_mod


program elemental_17
  use elemental_17_mod
  implicit none

  type(string_t) :: lines(3)

  lines(1)%string_ = "Hello"
  lines(2)%string_ = "World"
  lines = bracket(lines)

  print *, lines(1)%string_
  print *, lines(2)%string_
  print *, lines(3)%string_
end program elemental_17
