program equivalence_34
  ! Test: EQUIVALENCE variable updated when COMMON modified via separate
  ! COMMON statements in a different program unit.
  implicit none
  integer :: v1
  integer :: a, b
  equivalence (a, b)
  common v1
  common a
  v1 = 0
  a = 10
  call modify()
  if (a /= 100) error stop
  if (b /= 100) error stop
  print *, "PASS"
end program

subroutine modify()
  implicit none
  integer :: v1, x
  common v1
  common x
  x = x * x
end subroutine
