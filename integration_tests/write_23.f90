program write_23
  implicit none
  integer :: u, x

  ! Test write(u, pos=N) for unformatted stream I/O
  open(newunit=u, file='write_23.bin', access='stream', &
       form='unformatted', status='replace', action='readwrite')

  ! Write at pos=1, then at pos=101, then overwrite at pos=1
  write(u, pos=1) 42
  write(u, pos=101) 99
  write(u, pos=1) 7

  ! Read back and verify the overwrite at pos=1
  read(u, pos=1) x
  if (x /= 7) error stop

  ! Verify pos=101 is still intact
  read(u, pos=101) x
  if (x /= 99) error stop

  close(u, status='delete')
  print *, 'PASS'
end program
