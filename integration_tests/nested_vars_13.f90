! Derived types declared in a main program and used by its contained
! procedures: array broadcast of a constructor, a local of the program's type,
! a named constant, and a type guard on an extended type.
program nested_vars_13
  implicit none
  type :: a_t
    integer :: x, y
  end type
  type, extends(a_t) :: b_t
    integer :: z
  end type
  type(a_t), parameter :: c = a_t(5, 6)
  type(a_t) :: t, arr(3)
  type(b_t), target :: bt
  class(a_t), pointer :: p
  t = a_t(3, 4)
  call fill()
  if (any(arr(1:2)%x /= 1) .or. any(arr(1:2)%y /= 2)) error stop 1
  if (arr(3)%x /= 4 .or. arr(3)%y /= 7) error stop 2
  if (t%x /= 4 .or. t%y /= 6) error stop 3
  bt = b_t(1, 2, 3)
  p => bt
  call guard()
  if (bt%z /= 9) error stop 4
  print *, t%x, t%y, arr(3)%x, bt%z
contains
  subroutine fill()
    type(a_t) :: v
    arr = a_t(1, 2)
    v = a_t(t%y, 7)
    arr(3) = v
    t = a_t(v%x, c%y)
  end subroutine
  subroutine guard()
    select type (p)
    type is (b_t)
      p%z = 9
    class default
      error stop 5
    end select
  end subroutine
end program
