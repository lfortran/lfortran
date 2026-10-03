program file_66
  ! Test unformatted sequential read with mixed scalar + array in I/O list
  implicit none
  integer :: a(3), b(3), x, y
  integer :: c(2), d(2), p, q, r, s

  ! Test 1: scalar followed by array
  x = 42
  a = (/10, 20, 30/)
  open(20, form='unformatted', status='scratch')
  write(20) x, a
  rewind(20)
  y = 0
  b = 0
  read(20) y, b
  close(20)
  if (y /= 42) error stop
  if (b(1) /= 10) error stop
  if (b(2) /= 20) error stop
  if (b(3) /= 30) error stop

  ! Test 2: array followed by scalar
  c = (/7, 8/)
  p = 99
  open(21, form='unformatted', status='scratch')
  write(21) c, p
  rewind(21)
  d = 0
  q = 0
  read(21) d, q
  close(21)
  if (d(1) /= 7) error stop
  if (d(2) /= 8) error stop
  if (q /= 99) error stop

  ! Test 3: scalar, array, scalar
  open(22, form='unformatted', status='scratch')
  write(22) p, c, x
  rewind(22)
  r = 0
  d = 0
  s = 0
  read(22) r, d, s
  close(22)
  if (r /= 99) error stop
  if (d(1) /= 7) error stop
  if (d(2) /= 8) error stop
  if (s /= 42) error stop

  print *, "PASS"
end program
