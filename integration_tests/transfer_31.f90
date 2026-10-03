program transfer_31
  implicit none

  character(len=1), allocatable :: chars(:)
  character(len=17) :: expected_str
  integer :: i

  expected_str = " ABCDEFG abcdefg "

  chars = transfer(expected_str, "A", size=17)

  if (size(chars) /= 17) error stop 1
  
  do i = 1, 17
     if (chars(i) /= expected_str(i:i)) error stop 2
  end do

  print *, chars
end program transfer_31