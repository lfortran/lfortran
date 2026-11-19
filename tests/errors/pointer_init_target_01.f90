program pointer_init_target_01
type :: t
  integer :: v
end type
type(t) :: g(3)
type(t), pointer :: q => g(2)
end program pointer_init_target_01
