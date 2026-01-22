program read_array
  implicit none
  
  integer :: u
  integer :: idata(4)
  integer :: x, y, arr(2)

  open(newunit=u, file="read_31_data.txt", status="old")
  
  ! Test 1: Read integer array with format
  read(u, '(i3,i2,i2,i3)') idata
  if (.not. all(idata == [100, 1, 7, 1])) error stop "Test 1 failed"
  
  ! Test 2: Read mix of scalars and array  
  read(u, '(i3,i2,i2,i3)') x, arr, y
  if (x /= 50 .or. arr(1) /= 2 .or. arr(2) /= 3 .or. y /= 99) error stop "Test 2 failed"
  
  close(u)
  print *, "PASS"

end program read_array

