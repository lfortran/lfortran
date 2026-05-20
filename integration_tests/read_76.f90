program read_76
  implicit none

  double precision :: d1, d2, d3
  complex :: c1, c2

  open (42, file='read_76.dat', status='replace', form='formatted')
  write (42,'(a)') '1.0D0  (2.0, 2.0)  3.0D0  (4.0, 4.0)  5.0D0'
  write (42,'(a)') '6.0D0  (7.0, 7.0) / 8.0D0  (9.0, 9.0) 10.0D0'
  rewind (42)

  d1 = -1.0d0
  c1 = (-2.0, -3.0)
  d2 = -3.0d0
  c2 = (-4.0, -5.0)
  d3 = -5.0d0
  read (42, *) d1, c1, d2, c2, d3

  ! Slash should terminate assignment for the remaining items in this READ.
  d1 = -1.0d0
  c1 = (-2.0, -3.0)
  d2 = -3.0d0
  c2 = (-4.0, -5.0)
  d3 = -5.0d0
  read (42, *) d1, c1, d2, c2, d3

  if (abs(d1 - 6.0d0) > 0.0001d0) error stop 'd1 failed'
  if (abs(real(c1) - 7.0) > 0.0001 .or. abs(aimag(c1) - 7.0) > 0.0001) error stop 'c1 failed'
  if (abs(d2 + 3.0d0) > 0.0001d0) error stop 'd2 failed'
  if (abs(real(c2) + 4.0) > 0.0001 .or. abs(aimag(c2) + 5.0) > 0.0001) error stop 'c2 failed'
  if (abs(d3 + 5.0d0) > 0.0001d0) error stop 'd3 failed'

  close (42, status='delete')
  print *, 'ok'
end program
