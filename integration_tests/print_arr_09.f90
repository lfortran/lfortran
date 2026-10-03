program print_arr_09
  implicit none
  character(len=100) :: str1
  write(str1, "(6(1X,I0))") [1,2,3,4,5,6]
  if (trim(str1) /= " 1 2 3 4 5 6") error stop
end program print_arr_09
