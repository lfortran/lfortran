module class_57_mod
  implicit none

  type, abstract, public :: dumper
  contains
    procedure(to_toml), deferred :: dump_to_toml
    procedure, non_overridable, private :: dump_to_file
    generic :: dump => dump_to_file, dump_to_toml
  end type dumper

  type, extends(dumper) :: wrapper
  contains 
    procedure :: dump_to_toml => dump_x
  end type

  type :: toml_table
    integer :: key
  end type

  abstract interface 
    subroutine to_toml(self, x)
    import dumper, toml_table
    class(dumper), intent(in) :: self
    type(toml_table), intent(inout) :: x
    end subroutine
  end interface
contains

  ! Private target for generic
  subroutine dump_to_file(self, x)
    class(dumper), intent(inout) :: self
    integer, intent(in) :: x
    type(toml_table) ::table
    call self%dump(table)
    if (table%key /= 5)  error stop
  end subroutine

  subroutine dump_x(self, x)
    class(wrapper), intent(in) :: self
    type(toml_table), intent(inout) :: x
    x%key = 5
  end subroutine

end module class_57_mod


program class_57
  use class_57_mod
  implicit none
  class(wrapper), allocatable :: temp
  allocate(temp)
  call temp%dump(3)
end program class_57
