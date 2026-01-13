program bnbz2
  implicit none

! Specify blank mode via OPEN statement

  integer :: iunit
  integer :: i1, i2
  character(8) :: blank_mode

  open (newunit=iunit, file='bnbz.dat', status='unknown')
  write (iunit,'(a)') '1   1   '
  close (iunit)

  open (newunit=iunit, file='bnbz.dat', status='old', blank='null')
  read (iunit, '(2i4)') i1, i2
  if (i1 /= 1 .or. i2 /= 1) error stop
  inquire (unit=iunit, blank=blank_mode)
  if (blank_mode /= 'NULL') error stop
  close (iunit)

  open (newunit=iunit, file='bnbz.dat', status='old', blank='zero')
  read (iunit, '(2i4)') i1, i2
  if (i1 /= 1000 .or. i2 /= 1000) error stop
  inquire (unit=iunit, blank=blank_mode)
  if (blank_mode /= 'ZERO') error stop
  close (iunit)

end program
