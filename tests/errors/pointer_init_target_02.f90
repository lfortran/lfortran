program pointer_init_target_02
type :: t
  integer :: v
end type
integer :: n = 2
type(t), target :: g(3)
type(t), pointer :: q => g(n)
end program pointer_init_target_02
