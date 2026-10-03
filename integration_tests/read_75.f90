program read_75
  implicit none

  ! Test list-directed read with null values and repeat-count syntax.
  ! Input: ", (2.0, 3.0),,6.0D0, 2*,"
  !   d1 -> null (keep 1.0), c1 -> (2.0,3.0), c2 -> null (keep (4.0,5.0)),
  !   d2 -> 6.0, c3 -> null via 2* (keep (7.0,8.0)), d3 -> null via 2* (keep 9.0)

  character(*), parameter :: cdata = ", (2.0, 3.0),,6.0D0, 2*,"

  double precision :: d1, d2, d3
  complex :: c1, c2, c3

  open (42, file='read_75.dat', status='replace', form='formatted')
  write (42,'(a)') cdata
  rewind (42)
  d1 = 1.0d0
  c2 = (4.0, 5.0)
  c3 = (7.0, 8.0)
  d3 = 9.0d0
  read (42, *) d1, c1, c2, d2, c3, d3

  if (abs(d1 - 1.0d0) > 0.0001d0) error stop "d1 failed"
  if (abs(real(c1) - 2.0) > 0.0001 .or. abs(aimag(c1) - 3.0) > 0.0001) error stop "c1 failed"
  if (abs(real(c2) - 4.0) > 0.0001 .or. abs(aimag(c2) - 5.0) > 0.0001) error stop "c2 failed"
  if (abs(d2 - 6.0d0) > 0.0001d0) error stop "d2 failed"
  if (abs(real(c3) - 7.0) > 0.0001 .or. abs(aimag(c3) - 8.0) > 0.0001) error stop "c3 failed"
  if (abs(d3 - 9.0d0) > 0.0001d0) error stop "d3 failed"

  close (42, status='delete')
  print *, "ok"
end program
