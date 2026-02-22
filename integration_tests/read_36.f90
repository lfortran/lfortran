program read_fmt5_test
  implicit none

  integer :: ia(5)
  integer :: i

  open (10, file='fort.10', status='replace', form='formatted')

  ! Format is repeated for each value on a new record
  write (10, '(i4)') (i, i=1,5)

  rewind (10)

  ! Should read each element from the next record
  read (10, '(i4)') ia

  do i = 1, 5
     if (ia(i) /= i) then
        print *, "FAIL: ia(", i, ") =", ia(i), " expected ", i
        stop 1
     end if
  end do

  print *, "test passed"
  close(10)

end program