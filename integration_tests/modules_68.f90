module modules_68_mod
  implicit none
  private

  type, public :: mytype
    integer :: val
  end type

  public :: reverse
  interface reverse
    module procedure :: reverse_mytype
  end interface

contains

  elemental function reverse_mytype(x) result(r)
    type(mytype), intent(in) :: x
    type(mytype) :: r
    r%val = -x%val
  end function

end module

program modules_68
  use modules_68_mod
  implicit none
  type(mytype) :: x, reverse_mytype

  x = mytype(42)
  reverse_mytype = reverse(x)

  if (reverse_mytype%val /= -42) error stop
  print *, "PASS"
end program
