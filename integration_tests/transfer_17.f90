program transfer_17
  use iso_fortran_env, only: int8, int32
  implicit none
  integer(int32) :: a, b, c, d
  integer(int8) :: digest(16)

  a = 1
  b = 2
  c = 3
  d = 4
  digest = transfer([a, b, c, d], digest)

  if (digest(1) /= 1) error stop
  if (digest(2) /= 0) error stop
  if (digest(3) /= 0) error stop
  if (digest(4) /= 0) error stop
  if (digest(5) /= 2) error stop
  if (digest(6) /= 0) error stop
  if (digest(7) /= 0) error stop
  if (digest(8) /= 0) error stop
  if (digest(9) /= 3) error stop
  if (digest(10) /= 0) error stop
  if (digest(11) /= 0) error stop
  if (digest(12) /= 0) error stop
  if (digest(13) /= 4) error stop
  if (digest(14) /= 0) error stop
  if (digest(15) /= 0) error stop
  if (digest(16) /= 0) error stop

  print *, "All tests passed."
end program
