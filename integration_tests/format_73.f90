program format_73
  implicit none

  real :: a, b

  open (42, file='read_fmt12.dat', status='replace', form='formatted')
  write (42,'(a)') '1.2345E0  1.2345'
  rewind (42)
  read (42, 100) a, b
  print *, a, b
  if( abs (a-1.2345) >= 1e-3) error stop
  if( abs (b-12.345) >= 1e-3) error stop

100 format (-1P,2F8.1)

end program format_73
