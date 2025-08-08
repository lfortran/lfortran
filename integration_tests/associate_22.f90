program associate_22
  implicit none

  type :: my_type
    integer :: x
  end type my_type

  type(my_type) :: var1 = my_type(5)
  type(my_type), pointer :: p_null => null()

  type(my_type), target :: my_target
  type(my_type), pointer :: p_target => my_target

  type(my_type), dimension(2) :: arr = [ my_type(10), my_type(20) ]

  integer :: my_int = 42

  integer, parameter :: my_const = 30 + 12

  if (var1%x /= 5) error stop "Test Failed: Derived type constructor initialization"

  if (associated(p_null)) error stop "Test Failed: Pointer should be disassociated"

  my_target%x = 100
  if (p_target%x /= 100) error stop "Test Failed: Pointer not associated correctly with target"
  p_target%x = p_target%x + 1
  if (my_target%x /= 101) error stop "Test Failed: Target not modified via pointer"

  if (arr(1)%x /= 10 .or. arr(2)%x /= 20) error stop "Test Failed: Array of derived types initialization"

  if (my_int /= 42) error stop "Test Failed: Intrinsic variable initialization"

  if (my_const /= 42) error stop "Test Failed: Parameter initialization"

end program associate_22

