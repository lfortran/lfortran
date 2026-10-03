module derived_types_127_mod
  implicit none
  type :: my_type
    integer :: val
  contains
    procedure :: init
  end type
contains
  subroutine init(self, input_shape)
    class(my_type), intent(inout) :: self
    integer, intent(in) :: input_shape(:)
    self % val = input_shape(1)
  end subroutine
end module

program derived_types_127
  use derived_types_127_mod
  implicit none
  type(my_type) :: obj
  integer :: n
  n = 5
  call obj % init([n])
  if (obj % val /= 5) error stop
  call obj % init([10])
  if (obj % val /= 10) error stop
  print *, "ok"
end program
