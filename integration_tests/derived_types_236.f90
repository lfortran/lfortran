module derived_types_236_m
  ! An assumed-size dummy of derived type, `type(t) :: a(*)`, is passed as
  ! its data alone, like `integer :: a(*)`, not as a descriptor like the
  ! assumed-shape `a(:)`. A caller without an explicit interface passes the
  ! data alone, so the two must agree (#15406).
  implicit none
  type :: t
    integer :: i
  end type
end module

subroutine set_one(a, n)
  use derived_types_236_m, only: t
  implicit none
  integer, intent(in) :: n
  type(t) :: a(*)
  a(n)%i = 42
end subroutine

subroutine set_column(a, m, n)
  use derived_types_236_m, only: t
  implicit none
  integer, intent(in) :: m, n
  type(t) :: a(m, *)
  a(:, n)%i = 7
end subroutine

program derived_types_236
  use derived_types_236_m, only: t
  implicit none
  external :: set_one, set_column
  type(t) :: x(3), y(2, 3)
  x%i = 0
  call set_one(x, 2)
  print *, x%i
  if (any(x%i /= [0, 42, 0])) error stop
  y%i = 0
  call set_column(y, 2, 3)
  print *, y%i
  if (any(y(:, 1)%i /= 0) .or. any(y(:, 2)%i /= 0)) error stop
  if (any(y(:, 3)%i /= 7)) error stop
end program
