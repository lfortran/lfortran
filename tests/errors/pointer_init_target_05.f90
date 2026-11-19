program pointer_init_target_05
type :: t
  integer :: v
end type
type :: u
  integer :: v
end type
type(u), target :: g(3)
type(t), pointer :: q => g(2)
end program pointer_init_target_05
