! Test that finalizer calls get correct debug locations with -g.
! Linked list with recursive finalizers on derived types.
module finalization_02_mod
  implicit none
  private
  type :: node
    type(node), pointer :: next => null()
  contains
    final :: dealloc_node
  end type
  type, public :: list
    private
    type(node), pointer :: head => null()
  contains
    procedure :: add
    procedure, private :: copy
    generic :: assignment(=) => copy
    final :: dealloc_list
  end type
contains
  recursive subroutine dealloc_list(this)
    type(list), intent(inout) :: this
    if (associated(this%head)) deallocate(this%head)
  end subroutine
  recursive subroutine dealloc_node(this)
    type(node), intent(inout) :: this
    if (associated(this%next)) deallocate(this%next)
  end subroutine
  recursive function copy_node(n) result(c)
    type(node), intent(in) :: n
    type(node), pointer :: c
    allocate(c, source=n)
    if (associated(n%next)) c%next => copy_node(n%next)
  end function
  subroutine add(this)
    class(list), intent(inout) :: this
    type(node), pointer :: n
    allocate(n)
    n%next => this%head
    this%head => n
  end subroutine
  recursive subroutine copy(lhs, rhs)
    class(list), intent(inout) :: lhs
    class(list), intent(in) :: rhs
    if (associated(lhs%head)) deallocate(lhs%head)
    if (associated(rhs%head)) lhs%head => copy_node(rhs%head)
  end subroutine
end module

program finalization_02
  use finalization_02_mod
  implicit none
  call sub()
  print *, "ok"
contains
  subroutine sub()
    type(list) :: a
    call a%add()
  end subroutine
end program
