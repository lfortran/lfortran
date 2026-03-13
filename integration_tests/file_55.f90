program read_fmt8
  implicit none

  integer :: i1, i2, i3, i4

! Default is blank='null'
  open (10, file='file_55_fort.10', status='replace', form='formatted')
  write (10,'(a20)') ' '

  rewind (10)
  i1=1; i2=2; i3=3; i4=4
  read (10, '(4(i5))') i1, i2, i3, i4
  print *, 'default: i1, i2, i3, i4 =', i1, i2, i3, i4
  if(i1+i2+i3+i4 /= 0) error stop

  rewind (10)
  open (10, blank='zero')
  i1=1; i2=2; i3=3; i4=4
  read (10, '(4(i5))') i1, i2, i3, i4
  print *, 'zero: i1, i2, i3, i4 =', i1, i2, i3, i4
  if(i1+i2+i3+i4 /= 0) error stop

  rewind (10)
  open (10, blank='null')
  i1=1; i2=2; i3=3; i4=4
  read (10, '(4(i5))') i1, i2, i3, i4
  print *, 'null: i1, i2, i3, i4 =', i1, i2, i3, i4
  if (i1+i2+i3+i4 /= 0) error stop

  rewind (10)
  write (10,'(a20)') '1 2 3 4  5 6  7  8  '

  rewind (10)
  open (10, blank='zero')
  i1=1; i2=2; i3=3; i4=4
  read (10, '(4(i5))') i1, i2, i3, i4
  print *, 'zero: i1, i2, i3, i4 =', i1, i2, i3, i4
  if (i1+i2+i3+i4 /= 21015) error stop

  rewind (10)
  open (10, blank='null')
  i1=1; i2=2; i3=3; i4=4
  read (10, '(4(i5))') i1, i2, i3, i4
  print *, 'null: i1, i2, i3, i4 =', i1, i2, i3, i4
  if (i1+i2+i3+i4 /= 243) error stop
  close (10)

end program
