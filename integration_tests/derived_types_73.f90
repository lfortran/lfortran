module derived_types_73_mod
  implicit none

  type :: parent_t
    integer :: a = -1
    integer :: b = -1
  contains
    procedure, pass :: show => show_parent
  end type parent_t

  type, extends(parent_t) :: child_t
    integer :: y = -2
  contains
    procedure, pass :: show => show_child
  end type child_t

  
contains 
  subroutine show_parent(self)
    class(parent_t), intent(inout) :: self
    self%a = 15
    self%b = 30
  end subroutine show_parent

  subroutine show_child(self)
    class(child_t), intent(inout) :: self
    self%a = 45
    self%b = 60
    self%y = 5
  end subroutine show_child
end module derived_types_73_mod

program derived_types_73
  use derived_types_73_mod
  implicit none

  type(parent_t) :: p
  type(child_t)  :: c

  p%a = 10
  p%b = 20
  c%a = 1
  c%b = 2
  c%y = 2
  c%parent_t = p 
  if (c%a /= 10 .or. c%b /= 20) error stop
  call assign_struct_members(c%parent_t)
  if (c%a /= 100 .or. c%b /= 200) error stop
  c%parent_t = parent_t(150, 250)
  if (c%a /= 150 .or. c%b /= 250) error stop
  call c%show()
  if (c%a /= 45 .or. c%b /= 60 .or. c%y /= 5) error stop
  call c%parent_t%show()
  if (c%a /= 15 .or. c%b /= 30) error stop

contains
  
  subroutine assign_struct_members(p)
    type(parent_t), intent(inout) :: p
    p%a = 100
    p%b = 200
  end subroutine assign_struct_members

end program derived_types_73
