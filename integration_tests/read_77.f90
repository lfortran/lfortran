program read_77
  implicit none

  integer :: i, i1, i2, idata(4)

  open (42, file='fort.42', status='replace', form='formatted')
  write (42, '(a)') '1234567890'
  write (42, '(a)') ' 12345'
  rewind (42)
  read (42, 100) i1, i2
100 format (tr4, i2, tl2, i3)

  if (i1 /= 56) then
     error stop
  end if

  if (i2 /= 567) then
     error stop
  end if

  idata = -42
  read (42, 101) (idata(i), i=1,4)
101 format (i6.6, t1, i6.4, tl6, i6.3, tl9, i6.0)

  if (.not. all(idata == 12345)) then
     error stop
  end if

end program read_77