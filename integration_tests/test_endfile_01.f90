program test_endfile_02
  use iso_fortran_env, only: iostat_end
  implicit none
  integer :: u, ios
  character(len=20) :: line

  u = 20
  open(unit=u, file="test_endfile_02.txt", status="replace", action="readwrite", form="formatted")

  ! Write some data
  write(u, '(a)') "line1"
  write(u, '(a)') "line2"

  ! Mark the end of the file
  endfile(u)

  ! === New check: WRITE after ENDFILE without repositioning must fail ===
  write(u, '(a)', iostat=ios) "this should not be written"
  if (ios <= 0) then
    print *, "ERROR: WRITE after ENDFILE did not produce an error condition"
    close(u, status="delete")
    stop 1
  end if

  ! Now reposition to the beginning (required after ENDFILE)
  rewind(u)

  ! Read the two lines back
  read(u, '(a)', iostat=ios) line
  if (ios /= 0 .or. trim(adjustl(line)) /= "line1") then
    print *, "ERROR: Failed to read line1 correctly"
    stop 1
  end if

  read(u, '(a)', iostat=ios) line
  if (ios /= 0 .or. trim(adjustl(line)) /= "line2") then
    print *, "ERROR: Failed to read line2 correctly"
    stop 1
  end if

  ! Third read must hit end-of-file condition
  read(u, '(a)', iostat=ios) line
  if (ios /= iostat_end) then
    print *, "ERROR: Did not detect end-of-file after ENDFILE"
    stop 1
  end if

  close(u, status="delete")
  print *, "ENDFILE test passed"
end program test_endfile_02
