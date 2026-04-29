program test_endfile_02
! Test ENDFILE on a unit that has not been explicitly opened.
! Per the Fortran standard, I/O operations on an unconnected unit
! implicitly open a file (e.g., "fort.<unit>").
implicit none
integer :: iu, x
iu = 18

! ENDFILE on an unconnected unit should implicitly open it
endfile iu

! REWIND so we can read from the beginning
rewind iu

! READ should immediately hit end-of-file since ENDFILE truncated at position 0
read(iu, end=10) x
error stop "Expected end-of-file on read"
10 continue

close(iu, status='delete')
print *, "PASS"
end program test_endfile_02
