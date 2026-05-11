program print_arr_10
  integer, parameter :: a(*) = [1,2,3,4]
  integer :: res(2)
  res = [a([3,2])]
  if (res(1) /= 3 .or. res(2) /= 2) error stop
  print *, [a([3,2])]
  print *, "test passed"
end program print_arr_10