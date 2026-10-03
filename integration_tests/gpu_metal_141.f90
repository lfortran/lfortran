module gpu_metal_141_m
  implicit none
  enum, bind(C)
    enumerator :: gelu = 1
  end enum
contains
  pure integer function add_enum(x)
    integer, intent(in) :: x
    add_enum = x + gelu
  end function
end module gpu_metal_141_m

program gpu_metal_141
  use gpu_metal_141_m, only: add_enum
  implicit none
  integer :: i, y(4)
  do concurrent(i = 1:4)
    y(i) = add_enum(i)
  end do
  print *, y
  if (y(1) /= 2) error stop
  if (y(2) /= 3) error stop
  if (y(3) /= 4) error stop
  if (y(4) /= 5) error stop
end program
