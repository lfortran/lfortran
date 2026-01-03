module class_57_mod
  implicit none

  type, abstract, public :: dumper
  contains
    procedure(to_toml), deferred :: dump_to_toml
    procedure, non_overridable, private :: dump_to_file
    generic :: dump => dump_to_file, dump_to_toml
  end type dumper

  type, extends(dumper) :: wrapper
    integer :: x
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

  subroutine test_assign(self)
    class(dumper), allocatable, intent(inout) :: self
    self = wrapper(5)
  end subroutine

  logical function test_polymorphic_arg(self)
    class(dumper), intent(inout) :: self
    test_polymorphic_arg = .false.
    select type(self)
    type is (wrapper)
      self%x = 10
      test_polymorphic_arg = .true.
    end select
  end function

end module class_57_mod


program class_57
  use class_57_mod
  implicit none
  class(wrapper), allocatable :: temp
  class(dumper), allocatable :: temp2
  type(wrapper) :: w1
  logical :: l1 = .false.
  allocate(temp)
  call temp%dump(3)
  allocate(wrapper :: temp2)
  call test_assign(temp2)
  select type(temp2)
  type is (wrapper)
    if (temp2%x /= 5) error stop
  class default
    error stop
  end select
  l1 = test_polymorphic_arg(w1)
  if (l1 .neqv. .true.) error stop
  if (w1%x /= 10) error stop
end program class_57
