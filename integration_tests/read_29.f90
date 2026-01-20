program read_complex
  implicit none

  integer :: iunit
  complex :: cnum

  open (newunit=iunit, file='complex.data', form='formatted', status='unknown')

  write (iunit, '(2f10.5)') (42.5, -42.5)
  rewind (iunit)
  read (iunit, '(2f10.5)') cnum
  close (iunit, status='keep')

  if (abs(real(cnum) - 42.5) > 1.0e-5 .or. abs(aimag(cnum) + 42.5) > 1.0e-5) stop

  print *, 'test passed'

end program