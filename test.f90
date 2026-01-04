program test_read
  implicit none
  integer :: i1, i2

  read ('   1   2', '(i4, i4)') i1, i2

  if (i1 == 1 .and. i2 == 2) then
     print *, "OK"
  else
     print *, "FAIL"
  end if
end program