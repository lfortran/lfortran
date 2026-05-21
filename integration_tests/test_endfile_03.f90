program test_endfile_03
  ! Test that ENDFILE on a preconnected unit does not prevent
  ! a subsequent unformatted WRITE after REWIND.
  implicit none
  integer :: iunit, val
  iunit = 8

  ! ENDFILE implicitly opens "fort.8", then REWIND and unformatted WRITE
  endfile iunit
  rewind iunit
  write(iunit) 42

  ! Verify the value can be read back
  rewind iunit
  read(iunit) val
  if (val /= 42) error stop

  close(iunit, status='delete')
  print *, "PASS"
end program
