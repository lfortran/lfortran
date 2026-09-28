! A derived type declared in a main program and used by a contained procedure
! that the program never calls
program nested_vars_14
  implicit none
  type :: a_t
    integer :: x, y
  end type
  type(a_t), parameter :: c = a_t(5, 6)
  type(a_t) :: t
  t = a_t(3, c%y)
  if (t%x /= 3 .or. t%y /= 6) error stop 1
  print *, t%x, t%y
contains
  subroutine show()
    print *, t%x, t%y
  end subroutine
end program
