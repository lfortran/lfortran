module pointer_intent_in_03_mod
  implicit none
  ! Only the association status of a POINTER, INTENT(IN) dummy is fixed.
  ! Pointer components reached through it, and pointer dummies without
  ! INTENT(IN), can still be associated, nullified, allocated and deallocated.
  type :: node
    integer, pointer :: p => null()
  end type
  integer, target :: y = 7

contains

  subroutine relink(w)
    type(node), pointer, intent(in) :: w
    w%p => y
  end subroutine

  subroutine realloc(w)
    type(node), pointer, intent(in) :: w
    nullify(w%p)
    allocate(w%p)
    w%p = 3
  end subroutine

  subroutine drop(w)
    type(node), pointer, intent(in) :: w
    deallocate(w%p)
    w%p => null()
  end subroutine

  subroutine point(w)
    integer, pointer, intent(inout) :: w
    w => y
  end subroutine

end module

program pointer_intent_in_03
  use pointer_intent_in_03_mod
  implicit none
  type(node), pointer :: n
  integer, pointer :: q

  allocate(n)
  call relink(n)
  if (.not. associated(n%p, y)) error stop "component not associated with y"

  call realloc(n)
  if (associated(n%p, y)) error stop "component still associated with y"
  if (n%p /= 3) error stop "wrong value in reallocated component"

  call drop(n)
  if (associated(n%p)) error stop "component not nullified"

  q => null()
  call point(q)
  if (.not. associated(q, y)) error stop "inout dummy not associated with y"
  deallocate(n)
end program
