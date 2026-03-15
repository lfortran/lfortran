program read_fmt10
  implicit none

  integer :: idata(27)
  integer, parameter :: idata_correct(27) = [  &
    107, 8, 7, 1, 137, 80, 0,  &
    101, 102, 103, 104, 105, 106, 107, 108, 109, 110,  &
    111, 112, 113, 114, 115, 116, 117, 118, 119, 120   &
  ]
  integer :: i

  open (10, file='fort.10', status='replace', form='formatted')
  write (10,'(a)') '107 8 7  1137 80   0101102103104105106107108109110'
  write (10,'(a)') '111112113114115116117118119120'

  rewind (10)
  idata = -42
  read (10, 100) idata

  do, i=1, size (idata)
     if (idata(i) /= idata_correct(i)) error stop
  end do

100 format (I3, 2(I2), 3(I3), I4, 10(I3))

end program
