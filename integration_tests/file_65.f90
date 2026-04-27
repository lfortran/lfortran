program file_65
  ! Test: unformatted sequential read of multiple values from a single record
  implicit none
  integer :: a, b, c, d
  integer :: e, f, g, h
  real :: x, y
  real :: rx, ry

  a = 1
  b = 2
  c = 0
  d = 0

  ! Test 1: Two integers in one record
  open(unit=10, form='unformatted', access='sequential', status='scratch')
  write(10) a, b
  rewind(10)
  read(10) c, d
  close(10)

  if (c /= 1) error stop
  if (d /= 2) error stop

  ! Test 2: Four integers in one record
  e = 10
  f = 20
  g = 30
  h = 40
  open(unit=11, form='unformatted', access='sequential', status='scratch')
  write(11) e, f, g, h
  rewind(11)
  e = 0; f = 0; g = 0; h = 0
  read(11) e, f, g, h
  close(11)

  if (e /= 10) error stop
  if (f /= 20) error stop
  if (g /= 30) error stop
  if (h /= 40) error stop

  ! Test 3: Two reals in one record
  x = 3.14
  y = 2.71
  open(unit=12, form='unformatted', access='sequential', status='scratch')
  write(12) x, y
  rewind(12)
  rx = 0.0; ry = 0.0
  read(12) rx, ry
  close(12)

  if (abs(rx - 3.14) > 1.0e-5) error stop
  if (abs(ry - 2.71) > 1.0e-5) error stop

  ! Test 4: Multiple records, each with multiple values
  open(unit=13, form='unformatted', access='sequential', status='scratch')
  write(13) 100, 200
  write(13) 300, 400
  rewind(13)
  a = 0; b = 0; c = 0; d = 0
  read(13) a, b
  read(13) c, d
  close(13)

  if (a /= 100) error stop
  if (b /= 200) error stop
  if (c /= 300) error stop
  if (d /= 400) error stop

  print *, "All tests passed."
end program
