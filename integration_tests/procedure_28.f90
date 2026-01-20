module procedure_28_mod
  implicit none
  type :: action
     procedure(), pointer, nopass :: a => null()
  end type
contains
  function get(this) result(p)
    type(action), intent(in) :: this
    procedure(), pointer :: p
    p => this%a
  end function
end module

program procedure_28
  use procedure_28_mod
  type(action) :: x
  procedure(), pointer :: p

  p => get(x)
  if (associated(p)) call p
end program
