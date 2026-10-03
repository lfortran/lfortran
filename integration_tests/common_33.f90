program common_33
  ! Test: named COMMON blocks may have different sizes in different
  ! scoping units (storage association extension accepted by all
  ! major compilers).
  implicit none
  integer :: x
  common /blk/ x
  x = 42
  call sub()
  if (x /= 10) error stop
  print *, "ok"
end program

subroutine sub()
  implicit none
  integer :: a, b
  common /blk/ a, b
  a = 10
  b = 20
end subroutine
