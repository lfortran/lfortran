subroutine sub ()

  common /cblock/ x, y, z
  double precision x, y, z

  common /charblk/ c1, c2, c3
  character c1*2, c2*3, c3(3)*5

  if (x /= 1.5d0) error stop
  if (y /= 2.5d0) error stop
  if (z /= 3.5d0) error stop

  if (c1 /= 'he') error stop
  if (c2 /= 'llo') error stop
  if (.not. all(c3 == ['hello', 'world', '12345'])) error stop

end subroutine
program common_28

  common /cblock/ x, y, z
  double precision x, y, z

  common /charblk/ c1, c2, c3
  character c1*2, c2*3, c3(3)*5

  x = 1.5d0
  y = 2.5d0
  z = 3.5d0

  c1 = 'he'
  c2 = 'llo'
  c3 = ['hello', 'world', '12345']

  call sub ()

end program
