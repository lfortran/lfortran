module derived_types_122_mod
  implicit none

  type :: child_t
    integer :: val = 0
  end type

  type :: parent_t
    type(child_t), pointer :: first => null()
  end type

contains

  subroutine test_default_constructor()
    type(parent_t) :: p
    p = parent_t()
    if (associated(p%first)) error stop
  end subroutine

end module

program derived_types_122
  use derived_types_122_mod
  implicit none
  type(parent_t) :: p

  call test_default_constructor()

  p = parent_t()
  if (associated(p%first)) error stop

  print *, "PASS"
end program
