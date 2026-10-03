module custom_operator_19_inner
  implicit none
  interface operator(.myop.)
    module procedure int_myop
  end interface
contains
  pure function int_myop(a, b) result(r)
    integer, intent(in) :: a, b
    logical :: r
    r = (a == b)
  end function
end module

module custom_operator_19_outer
  use custom_operator_19_inner, only: operator(.myop.)
  implicit none
end module

module custom_operator_19_user
  use custom_operator_19_outer, only: operator(.myop.)
  implicit none
  interface operator(.myop.)
    module procedure logical_myop
  end interface
contains
  pure function logical_myop(a, b) result(r)
    logical, intent(in) :: a, b
    logical :: r
    r = (a .eqv. b)
  end function
end module

program custom_operator_19
  use custom_operator_19_user
  implicit none
  integer :: x, y
  logical :: p, q

  x = 1; y = 1
  if (.not. (x .myop. y)) error stop
  x = 1; y = 2
  if (x .myop. y) error stop

  p = .true.; q = .true.
  if (.not. (p .myop. q)) error stop
  p = .true.; q = .false.
  if (p .myop. q) error stop

  print *, "PASS"
end program
