program transfer_27
  implicit none
  type :: dt
    integer :: val
  end type
  type(dt) :: x
  integer(1) :: bytes(4)
  integer :: i

  ! Transfer a zeroed derived type to a byte array
  x%val = 0
  bytes = transfer(x, bytes)
  if (any(bytes /= 0)) error stop

  ! Transfer a non-zero derived type and verify round-trip
  x%val = 42
  bytes = transfer(x, bytes)
  x%val = 0
  x = transfer(bytes, x)
  if (x%val /= 42) error stop

  print *, "PASS"
end program
