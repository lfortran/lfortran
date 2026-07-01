program file_72
  implicit none

  real :: rdata(5), r_expected(5)
  integer :: i
  integer :: io_lun

  open (newunit=io_lun, file='file_72.dat', status='replace', form='formatted')
  write (io_lun, '(a)') '246801357912345678901234      '
  write (io_lun, '(a)') '.10203040506070809010E+0233.33'
  
  rewind (io_lun)

  rdata = 0.0
  i = -42
  read (io_lun, 100) (rdata(i),i=1,5)
  if (i /= size(rdata) + 1) error stop 1

  r_expected = [246.8, 135.79, 1.234567890E13, 10.20304, 33.33]
  do, i=1, size (rdata)
    if (abs(rdata(i) - r_expected(i)) >= 0.0001) error stop 2
  end do

  close (io_lun, status='delete')

100   FORMAT(2F5.2, F14.0 / E25.20, F5.2)

end program