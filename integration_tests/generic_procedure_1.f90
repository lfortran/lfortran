module derived_types
  implicit none

  type :: type_pass
    integer :: value
  contains
    procedure :: set_value_pass
    generic :: set_value => set_value_pass
  end type

  type :: type_nopass
    integer :: value
  contains
    procedure, nopass :: set_value_nopass
    generic :: set_value => set_value_nopass
  end type

contains

  subroutine set_value_pass(this, value)
    class(type_pass), intent(inout) :: this
    integer, intent(in) :: value
    this%value = 2 * value
  end subroutine

  subroutine set_value_nopass(obj, value)
    class(type_nopass), intent(inout) :: obj
    integer, intent(in) :: value
    obj%value = value
  end subroutine

end module

program main
  use derived_types
  implicit none

  type(type_pass) :: obj_pass
  type(type_nopass) :: obj_nopass

  obj_pass%value = 42
  call obj_pass%set_value(value=5)
  if (obj_pass%value /= 10) error stop "Error in set_value_pass"

  obj_nopass%value = 42
  call obj_nopass%set_value(obj_nopass, value=5)
  if (obj_nopass%value /= 5) error stop "Error in set_value_nopass"
end program