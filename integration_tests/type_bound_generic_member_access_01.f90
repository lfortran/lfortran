module err_mod
  implicit none
  type :: error_t
    integer :: code = 0
  end type error_t
end module err_mod

module toml_mod
  implicit none
  type :: toml_table
     integer :: x = 0
  end type toml_table
end module toml_mod

module base_mod
  use err_mod, only: error_t
  use toml_mod, only: toml_table
  implicit none
  type, abstract :: serializable_t
  contains
    procedure(load_toml), deferred :: load_from_toml
    procedure, private :: load_from_unit
    generic :: load => load_from_toml, load_from_unit
  end type serializable_t

  abstract interface
    subroutine load_toml(self, table, error)
      import :: serializable_t, toml_table, error_t
      class(serializable_t), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_t), allocatable, intent(out) :: error
    end subroutine load_toml
  end interface
contains
  subroutine load_from_unit(self, unit, error)
    class(serializable_t), intent(inout) :: self
    integer, intent(in) :: unit
    type(error_t), allocatable, intent(out) :: error
  end subroutine load_from_unit
end module base_mod

module derived_mod
  use base_mod
  use err_mod, only: error_t
  use toml_mod, only: toml_table
  implicit none
  type, extends(serializable_t) :: cmd_t
  contains
    procedure :: load_from_toml => cmd_load
  end type cmd_t

  type, extends(serializable_t) :: table_t
    type(cmd_t), allocatable :: command(:)
  contains
    procedure :: load_from_toml => table_load
  end type table_t
contains
  subroutine cmd_load(self, table, error)
    class(cmd_t), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_t), allocatable, intent(out) :: error
    table%x = table%x + 1
  end subroutine cmd_load

  subroutine table_load(self, table, error)
    class(table_t), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_t), allocatable, intent(out) :: error
    type(toml_table), target :: elem
    type(toml_table), pointer :: p
    integer :: i

    allocate(self%command(1))
    p => elem
    do i = 1, 1
      call self%command(i)%load(p, error)
    end do
  end subroutine table_load
end module derived_mod

program type_bound_generic_member_access_01
  use derived_mod
  use toml_mod, only: toml_table
  use err_mod, only: error_t
  implicit none
  type(table_t) :: t
  type(toml_table) :: tab
  type(error_t), allocatable :: err

  call t%load(tab, err)
  if (allocated(err)) error stop 1
  print *, "OK"
end program type_bound_generic_member_access_01
