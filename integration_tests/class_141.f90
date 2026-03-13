! Test: defined assignment inherited from abstract base type
module class_141_mod
  implicit none

  type, abstract :: base_type
  contains
    procedure, non_overridable, private :: copy
    generic :: assignment(=) => copy
    procedure(copy_iface), deferred :: copy_impl
  end type

  abstract interface
    subroutine copy_iface(lhs, rhs)
      import base_type
      class(base_type), intent(inout) :: lhs
      class(base_type), intent(in) :: rhs
    end subroutine
  end interface

  type, extends(base_type) :: my_list
    integer, pointer :: data => null()
  contains
    procedure :: copy_impl => my_list_copy
  end type

contains

  subroutine copy(lhs, rhs)
    class(base_type), intent(inout) :: lhs
    class(base_type), intent(in) :: rhs
    call lhs%copy_impl(rhs)
  end subroutine

  subroutine my_list_copy(lhs, rhs)
    class(my_list), intent(inout) :: lhs
    class(base_type), intent(in) :: rhs
    select type (rhs)
    type is (my_list)
      if (associated(rhs%data)) then
        allocate(lhs%data, source=rhs%data)
      end if
    end select
  end subroutine

end module

program class_141
  use class_141_mod
  implicit none

  type(my_list) :: a, b

  allocate(a%data, source=42)
  b = a
  a%data = -1

  if (.not. associated(b%data)) error stop
  if (b%data /= 42) error stop
  print *, "PASS"
end program
