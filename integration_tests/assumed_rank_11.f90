program main
  implicit none

  integer, target :: values(2, 3, 4)

  call take_pointer(values)

contains

  subroutine take_pointer(x)
    integer, pointer, intent(in) :: x(..)

    if (any(lbound(x) /= [1, 1, 1])) error stop
    if (any(ubound(x) /= [2, 3, 4])) error stop
  end subroutine take_pointer

end program main
