program test_read_int_iostat
  implicit none
  integer i, n, ios
  character(4):: input(3) = ['42  ','3.14','abcd']

  do i = 1, 3
    read(input(i), "(I4)", iostat=ios) n
    if (ios /= 0) exit
    print "(A,I0)", 'You said ', n
  end do

  if (i /= 2) error stop "expected exit on iteration 2"
  if (ios == 0) error stop "iostat should be nonzero for '3.14'"
  print *, "Test passed"

end program