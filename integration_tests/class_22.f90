module class_22_m
  implicit none
  private
  public :: inner_class, cast_integer, expect_pointer

  ! Define the inner class with some integer data
  type :: inner_class
     integer :: value
  end type inner_class

contains

function cast_integer(obj) result(x)
    class(inner_class), intent(in), allocatable :: obj
    logical :: x

    x = .true.
  end function cast_integer

  subroutine expect_pointer(x)
    type(inner_class), pointer, intent(in) :: x
    if (x%value /= 50) error stop
  end subroutine expect_pointer
end module class_22_m

program class_22
  use class_22_m
  implicit none
  class(inner_class), allocatable :: inner_obj
  type(inner_class), target :: x
  x%value = 50
  if (cast_integer(inner_obj) .neqv. .true.) error stop
  call expect_pointer(x)
end program class_22
