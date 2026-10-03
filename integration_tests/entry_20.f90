program entry_20
  implicit none
  integer :: a, b, c
  common a, b, c
  a = 0
  b = 65
  c = 23
  call entry_no_parens
  if (a /= 88) error stop
  a = 4
  b = -17
  call entry_empty_parens()
  if (c /= -13) error stop
  print *, "PASS"
end program

subroutine dummy_sub(x, y)
  implicit none
  integer, intent(inout) :: x, y
  integer :: a, b, c
  common a, b, c
  x = x + y
  return
  entry entry_no_parens
  a = b + c
  return
  entry entry_empty_parens()
  c = a + b
  return
end subroutine
