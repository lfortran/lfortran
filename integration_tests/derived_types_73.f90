module derived_types_73_mod
  implicit none

  type :: parent_t
    integer :: a = -1
    integer :: b = -1
  end type parent_t

  type, extends(parent_t) :: child_t
    integer :: y = -2
  end type child_t

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
  ! call assign_struct_members(c%parent_t)          !! TODO: handle this case
  ! c%parent_t = parent_t(100, 200)                 !! TODO: handle this case
  ! if (c%a /= 100 .or. c%b /= 200) error stop
contains
  
  subroutine assign_struct_members(p)
    type(parent_t), intent(inout) :: p
    p%a = 100
    p%b = 200
  end subroutine assign_struct_members

end program derived_types_73
