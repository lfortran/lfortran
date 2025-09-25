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
  allocate(character(0) :: lines(3)%string_) ! Allocate empty strings (unallocated variables musn't be referenced)

  lines = bracket(lines)

  print *, lines(1)%string_
  if(lines(1)%string_ /= "Hello called func") error stop

  print *, lines(2)%string_
  if(lines(2)%string_ /= "World called func") error stop

  print *, lines(3)%string_
  if(lines(3)%string_ /= " called func") error stop
end program elemental_17
