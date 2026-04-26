program read_82
  implicit none

  character(4) :: c1
  real :: r1
  integer :: i1
  logical :: l1

  open (42, status='scratch', form='formatted')
  write (42,'(a)') "'ONE ',,3,F"

  rewind (42)
  c1 = 'none'
  r1 = 2.0
  i1 = -42
  l1 = .true.
  read (42, *) c1, r1, i1, l1

  if (c1 /= 'ONE ') error stop "c1 failed"
  if (abs(r1 - 2.0) >= 0.0001) error stop "r1 (null) not preserved"
  if (i1 /= 3) error stop "i1 failed"
  if (l1) error stop "l1 failed"

  close (42)
end program