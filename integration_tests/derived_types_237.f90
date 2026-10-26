module derived_types_237_m
  ! An array element passed to an assumed-size dummy of derived type,
  ! `type(t) :: a(*)`, starts the sequence of elements the dummy is
  ! associated with (#15406).
  implicit none
  type :: t
    integer :: i
  end type
contains
  subroutine set_second(a)
    type(t), intent(inout) :: a(*)
    a(2)%i = 42
  end subroutine
end module

program derived_types_237
  use derived_types_237_m, only: t, set_second
  implicit none
  type(t) :: x(4)
  x%i = 0
  call set_second(x(2))
  print *, x%i
  if (any(x%i /= [0, 0, 42, 0])) error stop
end program
