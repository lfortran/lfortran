module derived_types_126_mod
  implicit none
  type :: string_t
    character(len=:), allocatable :: s
  contains
    procedure :: bracket
  end type
contains
  elemental function bracket(self, opening) result(res)
    class(string_t), intent(in) :: self
    character(len=*), intent(in), optional :: opening
    type(string_t) :: res
    if (present(opening)) then
      res%s = opening // self%s
    else
      res%s = self%s
    end if
  end function
end module

program derived_types_126
  use derived_types_126_mod
  implicit none
  type :: box_t
    type(string_t) :: items(2)
  end type
  type(box_t) :: b
  type(string_t) :: results(2)

  b%items(1)%s = "hello"
  b%items(2)%s = "world"

  ! Call elemental type-bound function with optional arg omitted
  ! through a derived-type component array
  results = b%items%bracket()
  if (results(1)%s /= "hello") error stop
  if (results(2)%s /= "world") error stop

  ! Call with optional arg present
  results = b%items%bracket("[")
  if (results(1)%s /= "[hello") error stop
  if (results(2)%s /= "[world") error stop

  print *, "PASS"
end program
