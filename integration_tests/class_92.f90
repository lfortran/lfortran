! Test for unlimited polymorphic array member with type-bound procedures
! Fixes issue #7359 - class(*) vtable codegen bug
module class_92_mod
  implicit none

  type :: deque
    private
    class(*), dimension(:), allocatable :: items
    integer :: count = 0
  contains
    procedure :: at_size_kind
    procedure :: at_default
    procedure :: get_count
  end type deque

contains

  function at_size_kind(this, i, rc) result(res)
    class(deque), target, intent(in) :: this
    integer(8), intent(in) :: i
    integer, intent(out) :: rc
    class(*), pointer :: res
    res => null()
    rc = 0
  end function at_size_kind

  function at_default(this, i, rc) result(res)
    class(deque), target, intent(in) :: this
    integer, intent(in) :: i
    integer, intent(out) :: rc
    class(*), pointer :: res

    res => this%at_size_kind(int(i, 8), rc)
  end function at_default

  function get_count(this) result(c)
    class(deque), intent(in) :: this
    integer :: c
    c = this%count
  end function get_count
end module class_92_mod

program class_92
  use class_92_mod
  implicit none
  type(deque) :: d
  integer :: c

  c = d%get_count()
  if (c /= 0) error stop

  print *, "PASS"
end program class_92
