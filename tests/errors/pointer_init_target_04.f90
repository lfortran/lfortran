program pointer_init_target_04
type :: t
  integer :: v
end type
type(t), target :: g(3)
type(t), pointer :: q => g(1:2)
end program pointer_init_target_04
