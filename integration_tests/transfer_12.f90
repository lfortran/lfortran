! Test for https://github.com/lfortran/lfortran/issues/3961
! transfer string to character array
program transfer_12
  implicit none
  character(1) :: lstring*26, larray(26)
  integer i
  lstring = 'abcdefghijklmnopqrstuvwxyz'
  larray = transfer(lstring, larray)
  do i = 1, 26
    if (larray(i) /= lstring(i:i)) error stop
  end do
  print *, "PASS"
end program transfer_12
