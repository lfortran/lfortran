program test_read
  implicit none
  integer :: i1, i2

  character(len=8) :: cdata = '   1   2'

  read (cdata, '(i4, i4)') i1, i2

  if(i1 /= 1 .or. i2 /= 2) error stop

  print *, "OK"
end program