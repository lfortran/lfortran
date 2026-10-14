! Derived types of a main program that depend on other derived types of the
! program (parent type, component types, default initialization) used through
! host association in internal procedures (#13630).
program nested_vars_22
  implicit none
  type :: base_t
    integer :: x
  end type
  type, extends(base_t) :: child_t
    integer :: y
  end type
  type :: in_t
    integer :: a
  end type
  type :: out_t
    type(in_t) :: i = in_t(3)
    integer :: b
  end type
  type(child_t) :: c
  type(out_t) :: o, oa(2)

  c%x = 1; c%y = 2
  o = out_t(in_t(1), 2)
  oa(1)%b = 5
  call swap()
  if (c%x /= 2 .or. c%y /= 1) error stop 1
  if (o%i%a /= 2 .or. o%b /= 1) error stop 2
  if (oa(2)%i%a /= 3 .or. oa(2)%b /= 8) error stop 3
  print *, c%x, c%y, o%i%a, o%b, oa(2)%b

contains

  subroutine swap()
    type(out_t) :: z
    c = child_t(c%y, c%x)
    o = out_t(in_t(o%b), o%i%a)
    z%b = oa(1)%b + z%i%a
    oa(2) = z
  end subroutine swap

end program nested_vars_22
