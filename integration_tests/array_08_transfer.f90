program array_08_transfer
  implicit none
  character(kind=1), dimension(5) :: chr_arr1
  character(kind=1, len=2), dimension(5) :: chr_arr2
  character(len=*), parameter :: rhs = "xyzw"
  character(kind=1), dimension(4) :: chr_arr3
  
  !Check indexed assignment
  chr_arr1(2:5) = transfer(rhs, chr_arr1)
  print *, chr_arr1
  print *, rhs
  if (chr_arr1(2) /= 'x' .or. chr_arr1(3) /= 'y' .or. &
      chr_arr1(4) /= 'z' .or. chr_arr1(5) /= 'w') error stop
  
  !Check multi-length assignment
  chr_arr2(1:2) = transfer(rhs, chr_arr2)
  print *, chr_arr2(1:2)
  print *, rhs 
  if (chr_arr2(1) /= 'xy' .or. chr_arr2(2) /= 'zw') error stop
  
  !Check assignment to entire array
  chr_arr3 = transfer(rhs, chr_arr3)
  print *, chr_arr3
  print *, rhs
  if (chr_arr3(1) /= 'x' .or. chr_arr3(2) /= 'y' .or. &
      chr_arr3(3) /= 'z' .or. chr_arr3(4) /= 'w') error stop
end program array_08_transfer