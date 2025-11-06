module class_74_mod
  implicit none

  type :: toml_table
    integer :: count = 0
  contains
    procedure :: push_back
  end type toml_table

  type :: toml_parser
    type(toml_table), pointer :: table
    type(toml_table), allocatable :: table2
  end type toml_parser

contains

  subroutine push_back(self)
    class(toml_table), intent(inout) :: self
    self%count = self%count + 1
  end subroutine push_back

end module class_74_mod


program class_74
  use class_74_mod
  implicit none

  type(toml_table), target :: tab
  class(toml_parser), allocatable :: parser

  allocate(parser)
  parser%table => tab
  parser%table2 = tab

  call add_table(parser%table)
  call add_table(parser%table2)
  print *, parser%table%count
  print *, parser%table2%count
  if (parser%table%count /= 1) error stop
  if (parser%table2%count /= 1) error stop

contains

  subroutine add_table(table)
    class(toml_table), intent(inout) :: table
    call table%push_back()
  end subroutine add_table

end program class_74
